{-|
Module      : ExHack.ProcessingSteps
Description : Processing operations used to both generate the ExHack database and the HTML documentation.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module ExHack.ProcessingSteps (
    dlAssets,
    generateDb,
    generateHtmlPages,
    genGraphDep,
    indexSymbols,
    parseStackage,
    retrievePkgsExports,
    saveGraphDep
) where

import           Control.DeepSeq                (force)
import           Control.Lens                   (view)
import           Control.Monad                  (foldM_)
import           Control.Monad.Catch            (MonadCatch, MonadThrow,
                                                 displayException, handleAll,
                                                 throwM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader.Class     (asks)
import qualified Data.ByteString                as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy           as BL (writeFile)
import           Data.FileEmbed                 (embedFile)
import qualified Data.HashMap.Strict            as HM (HashMap, elems, empty,
                                                       filterWithKey, insert,
                                                       lookup)
import qualified Data.HashSet                   as HS (foldl', unions)
import           Data.List                      (foldl')
import           Data.Maybe                     (fromJust)
import qualified Data.Text                      as T (pack, replace, unpack)
import qualified Data.Text.IO                   as T (readFile)
import qualified Data.Text.Lazy                 as TL (Text)
import qualified Data.Text.Lazy.IO              as TL (hPutStr)
import           Database.Selda                 (SeldaM)
import           Database.Selda.SQLite          (withSQLite)
import           Network.HTTP.Client            (Manager, httpLbs,
                                                 managerSetProxy, newManager,
                                                 parseRequest_,
                                                 proxyEnvironment, responseBody)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                ((<.>), (</>))
import           System.IO                      (IOMode (WriteMode),
                                                 hSetEncoding, utf8, withFile)
import           Text.Blaze.Html.Renderer.Text  (renderHtml)

import           ExHack.Cabal.Cabal             (buildPackage)
import           ExHack.Cabal.CabalParser       (parseCabalFile)
import           ExHack.Data.Db                 (getHomePagePackages,
                                                 getModulePageSyms,
                                                 getPackagePageMods,
                                                 getPkgImportScopes, initDb,
                                                 saveModuleUnifiedSymbols,
                                                 savePackageDeps,
                                                 savePackageMods, savePackages)
import           ExHack.Ghc                     (getModImports, getModSymbols,
                                                 unLoc)
import           ExHack.Hackage.Hackage         (findComponentRoot,
                                                 getPackageExports,
                                                 unpackHackageTarball)
import           ExHack.ModulePaths             (toModFilePath)
import           ExHack.Renderer.Html           (addLineMarker, highLightCode,
                                                 homePageTemplate,
                                                 modulePageTemplate,
                                                 packagePageTemplate)
import qualified ExHack.Renderer.Types          as RT (HighlightedSourceCodeFile (..),
                                                       HighlightedSymbolOccurs (..),
                                                       HomePagePackage (..),
                                                       ModuleName (..),
                                                       PackageName (..),
                                                       SymbolOccurs (..),
                                                       renderRoute)
import           ExHack.Stackage.StackageParser (getHackageUrls,
                                                 parseStackageYaml)
import           ExHack.Types                   (AlterDatabase,
                                                 CabalBuildError (..),
                                                 CabalFilesDir (..),
                                                 ComponentRoot (..),
                                                 DatabaseHandle,
                                                 DatabaseStatus (..),
                                                 HtmlDir (..), ImportsScope,
                                                 IndexedModuleNameT (..),
                                                 IndexedSym (..),
                                                 LocatedSym (..), ModuleName,
                                                 ModuleNameT (..),
                                                 MonadLog (..), MonadStep,
                                                 Package (allComponents, packageFilePath),
                                                 PackageComponent (..),
                                                 PackageDesc (..),
                                                 PackageDlDesc,
                                                 PackageDlDesc (..),
                                                 PackageFilePath (..),
                                                 SourceCodeFile (..),
                                                 StackageFile (..), SymName,
                                                 TarballsDir (..),
                                                 UnifiedSym (..), WorkDir (..),
                                                 getDatabaseHandle, getModNameT,
                                                 getName, getPackageNameT,
                                                 logInfo, packagedlDescName)
import           ExHack.Utils                   (Has (..), foldM')

-- | `Step` 1: database generation.
--
--   This function creates a new SQLite database initialized according
--   to ex-hack's internal SQL scheme.
generateDb :: forall c m. 
    (Has c (DatabaseHandle 'New), 
     MonadStep c m) 
    => m (DatabaseHandle (AlterDatabase 'New))
generateDb = do
    logInfoTitle "[Step 1] Generating database scheme."
    dh <- asks (view hasLens) :: m (DatabaseHandle 'New)
    let (fp, dh') = getDatabaseHandle dh 
    withSQLite fp initDb
    pure dh'

-- | `Step` 2: stackage file parsing.
--
--   This function parses the stackage file that will be used to 
--   generate the packages dependancy graph.
parseStackage :: forall c m.
    (Has c StackageFile,
     MonadStep c m)
    => m [PackageDlDesc]
parseStackage = do
    logInfoTitle "[Step 2] Parsing Stackage file"
    (StackageFile stackageFp) <- asks (view hasLens)
    stackageYaml <- liftIO $ T.readFile stackageFp
    let packages = fromJust $ parseStackageYaml stackageYaml 
    pure $ getHackageUrls packages

-- | `Step` 3: assets downloading.
--
--   This function downloads both the cabal files and the taballs of the packages.
--   Everything will be downloaded from the <https://hackage.haskell.org> mirror.
dlAssets :: forall c m.
    (Has c TarballsDir,
     Has c CabalFilesDir,
     MonadStep c m)
    => [PackageDlDesc] -> m ()
dlAssets packages = do
    logInfoTitle "[Step 3] Downloading hackage assets (cabal files, tarballs)."
    let settings = managerSetProxy
            (proxyEnvironment Nothing)
            tlsManagerSettings
    tbd <- asks (view hasLens)
    cd <- asks (view hasLens)
    m <- liftIO $ newManager settings
    _ <- foldr (dlFoldCabalFiles cd tbd m (length packages)) (return 1) packages
    return ()
  where
    dlFoldCabalFiles :: CabalFilesDir -> TarballsDir -> Manager -> Int -> PackageDlDesc -> m Int -> m Int
    dlFoldCabalFiles !cd !td man totalSteps !p step = handleAll logErrors $ do 
        step' <- step
        let !pn = packagedlDescName p
        logInfoProgress 3 totalSteps step' $ "Downloading " <> pn <> " assets."
        downloadHackageFiles cd td man p
        return $ step' + 1
      where
        logErrors e = do
            logError $ "[Step 3] ERROR while downloading " <> packagedlDescName p 
                        <> " assets: " <> T.pack (displayException e)
            step' <- step
            pure (step' + 1)
    downloadHackageFiles :: CabalFilesDir -> TarballsDir -> Manager -> PackageDlDesc -> m ()
    downloadHackageFiles 
      (CabalFilesDir cabalFilesDir) (TarballsDir tarballsDir) man 
      (PackageDlDesc (name, cabalUrl, tarballUrl)) = 
        liftIO $ do
            f <- httpLbs (parseRequest_ $ T.unpack cabalUrl) man 
            BL.writeFile (cabalFilesDir </> T.unpack name <.> "cabal") $ responseBody f 
            f' <-  httpLbs (parseRequest_ $ T.unpack tarballUrl) man
            BL.writeFile (tarballsDir </> T.unpack name <.> "tar.gz") $ responseBody f' 
            return ()

-- | `Step` 4: Dependencies graph generation.
--
--   This function generates the packages dependancy graph.
--
genGraphDep :: forall c m.
    (Has c TarballsDir,
     Has c CabalFilesDir,
     Has c WorkDir,
     Has c (DatabaseHandle 'Initialized),
     MonadStep c m)
    => [PackageDlDesc] -> m [Package]
genGraphDep pd = do
    logInfoTitle "[Step 4] Generating dependencies graph."
    tbd <- asks (view hasLens)
    cd <- asks (view hasLens)
    wd <- asks (view hasLens) 
    logInfo "[+] Parsing cabal files."
    (_, !pkgs) <- foldM' (readPkgsFiles cd wd tbd (length pd)) (1,[]) pd
    pure pkgs
  where
    readPkgsFiles (CabalFilesDir cabalFilesDir) (WorkDir wd) (TarballsDir tarballsDir) !totalSteps (!step, xs) p = 
        handleAll logErrors $ do
            logInfoProgress 4 totalSteps step $ "Unzip " <> packagedlDescName p <> " package."
            let tbp = tarballsDir </> T.unpack (packagedlDescName p) <.> "tar.gz"
            tb <- liftIO $ BS.readFile tbp
            pfp <- unpackHackageTarball wd tb
            logInfoProgress 4 totalSteps step $ "Reading " <> packagedlDescName p <> " cabal file."
            !cf <- liftIO $ T.readFile $ cabalFilesDir </> T.unpack (packagedlDescName p) <.> "cabal"
            let !pack = parseCabalFile $ PackageDesc (pfp,cf)
            case pack of
            -- TODO: log err
                Nothing -> pure (step + 1, xs)
                Just !x  -> pure (step + 1, x:xs)
      where
        logErrors e = do
            logError $ "[Step 4] ERROR cannot read " <> packagedlDescName p
                       <> " cabal file: " <> T.pack (displayException e)
            pure (step + 1, xs)

-- | `Step` 5: Save dependancies graph.
-- 
--   This step takes the previously generated dependancies graph and saves it
--   in the database.
--
--   Caution: this step can be **really** long.
saveGraphDep :: forall c m.
    (Has c TarballsDir,
     Has c CabalFilesDir,
     Has c (DatabaseHandle 'Initialized),
     MonadStep c m)
    => [Package] -> m (DatabaseHandle 'DepsGraph)
saveGraphDep pkgs = do
    logInfoTitle "[Step 5] Saving dependencies graph."
    dbHandle <- asks (view hasLens) :: m (DatabaseHandle 'Initialized)
    let (dbFp, dbHandle') = getDatabaseHandle dbHandle
    liftIO $ withSQLite dbFp $ do
        logInfo "[+] Saving packages to DB (may take some time)..."
        savePackages pkgs
        logInfo "[+] Done."
        logInfo "[+] Saving dependancies to DB..."
        -- TODO: maybe speedup this insert by caching the packages ids
        -- in a hasmap in the memory. (or use sqlite in memory system????)
        foldM_ (foldInsertDep (length pkgs)) 1 pkgs
        logInfo "[+] Done."
        return ()
    pure dbHandle'
  where
    foldInsertDep :: Int -> Int -> Package -> SeldaM Int
    foldInsertDep totalDeps step pkg = handleAll logErrors $ do 
        savePackageDeps pkg
        logInfoProgress 5 totalDeps step $ "Saving " <> getName pkg <> " dependancies to DB."
        pure $ step + 1
      where
        logErrors e = do
            logError $ "[Step 5] ERROR cannot insert " <> getName pkg <> " dependancies to DB: "
                <> T.pack (displayException e)
            pure $ step + 1

-- | `Step` 6: extracting and indexing modules exports.
--
--   Builds the packages using cabal, load the modules in a 
--   GHC-API program which extracts the exports and finally save
--   everything in the ex-hack database.
retrievePkgsExports :: forall c m.
    (Has c (DatabaseHandle 'DepsGraph),
     MonadStep c m)
   => [Package] -> m (DatabaseHandle 'PkgExports)
retrievePkgsExports pkgs = do
    logInfoTitle "[Step 6] Retrieving modules exports."
    dbHandle <- asks (view hasLens) :: m (DatabaseHandle 'DepsGraph)
    let (dbFp, dbHandle') = getDatabaseHandle dbHandle
    foldM_ (getPkgExports dbFp (length pkgs)) 1 pkgs
    pure dbHandle'
  where
    getPkgExports :: FilePath -> Int -> Int -> Package -> m Int
    getPkgExports dbFp totalSteps !nb p = handleAll logErrors $ do
        logInfoProgress 6 totalSteps nb $ "Retrieving "<> getName p <> " exports." 
        let pfp = packageFilePath p
        cr <- buildPackage pfp
        maybe (pure ()) (\(errCode, errStr) -> throwM $ CabalBuildError errCode errStr) cr 
        x  <- getPackageExports pfp p
        logInfoProgress 6 totalSteps nb $ "Saving "<> getName p <> " exports to DB." 
        _ <- liftIO $ withSQLite dbFp $ savePackageMods p $ force x
        pure $ nb + 1
      where
        logErrors e = do
            logError $ "[Step 6] ERROR cannot get exports for " <> getName p <> ": " 
                     <> T.pack (displayException e)
            pure $ nb + 1
            
-- | `Step` 7: Indexes the code source symbols in the database.
--
-- For each package, component and module, this step will:
--
-- 1. Retrieve the imported symbols and try to match them to the previously
--    indexed package exports.
-- 2. Use GHC parser to get this file symbols.
-- 3. Unify these symbols to the imported one.
-- 4. We save each unified occurence in the database.
indexSymbols :: forall c m.
    (MonadStep c m,
     MonadCatch m,
     MonadThrow m,
     Has c (DatabaseHandle 'PkgExports))
  => [Package] -> m (DatabaseHandle 'IndexedSyms)
indexSymbols pkgs = do
    logInfoTitle "[Step 7] Indexing used symbols."
    dbh <- asks (view hasLens) :: m (DatabaseHandle 'PkgExports)
    let (dbfp, dbh') = getDatabaseHandle dbh
    foldM_ (indexPackage dbfp (length pkgs)) 1 pkgs 
    pure dbh'
  where
    indexPackage :: FilePath -> Int -> Int -> Package -> m Int 
    indexPackage !dbFp nb cur p = do
        logInfoProgress 7 nb cur $ "Indexing " <> getName p <> " used symbols."
        is <- liftIO $ withSQLite dbFp $ getPkgImportScopes p
        indexComponent dbFp p (packageFilePath p) is `mapM_` allComponents p 
        pure $ cur + 1
    indexComponent :: FilePath -> Package -> PackageFilePath -> ImportsScope 
                   -> PackageComponent -> m ()
    indexComponent dbh p pfp is pc = handleAll logErrors $ do
            mfps <- findModuleFilePath pfp (roots pc) `mapM` mods pc
            indexModule dbh p pfp is `mapM_` mfps
      where
        logErrors e =
            logError $ "[Step 7] ERROR while indexing component " <> T.pack (show pc) <> " from package "
                     <> getName p <> ": " <> T.pack (displayException e)
    indexModule :: FilePath -> Package -> PackageFilePath -> ImportsScope 
                -> (ModuleName, ComponentRoot) -> m ()
    indexModule dbFp p pfp is (mn,cr) = handleAll logErrors $ do
        imports <- getModImports pfp cr mn 
        -- fis: filtered import scope according to this module imports
        -- isyms: imported symbols hashsets on which we will perform the unification
        let !fis = HM.filterWithKey (\(IndexedModuleNameT (n, _)) _ -> n `elem` imports) is
            !isyms = HS.unions $ HM.elems fis
            !isymsMap = HS.foldl' (\hm is'@(IndexedSym (n, _)) -> HM.insert n is' hm) HM.empty isyms 
        syms <- getModSymbols p pfp cr mn
        fileContent <- liftIO $ T.readFile $ toModFilePath pfp cr mn
        let !file = SourceCodeFile fileContent (getModNameT mn) (getPackageNameT p)
            unsyms = unifySymbols isymsMap syms
        withSQLite dbFp $ saveModuleUnifiedSymbols unsyms file 
      where
        logErrors e = do
            let (ModuleNameT mnt) = getModNameT mn
            logError $ "[Step 7] ERROR while indexing module " <> mnt <> " from package "
                     <> getName p <> ": " <> T.pack (displayException e)
                    
    findModuleFilePath :: PackageFilePath -> [ComponentRoot] -> ModuleName -> m (ModuleName, ComponentRoot)
    findModuleFilePath pfp crs mn = do
        cr <- findComponentRoot pfp crs mn
        pure (mn, cr)
    unifySymbols :: HM.HashMap SymName IndexedSym -> [LocatedSym] -> [UnifiedSym]
    unifySymbols isyms = foldl' foldLSym []
      where
        foldLSym xs ls@(LocatedSym (_, _, locSym)) = 
            maybe xs (\is -> UnifiedSym(is,ls) : xs) (HM.lookup (unLoc locSym) isyms) 

-- | `Step` 8: Generates the HTML documentation using the previously
--    generated database.
generateHtmlPages :: forall c m.
    (MonadStep c m,
     MonadCatch m,
     MonadThrow m,
     Has c (DatabaseHandle 'IndexedSyms),
     Has c HtmlDir)
  => m ()
generateHtmlPages = do
    logInfoTitle "[Step 8] Generating the HTML documentation."
    HtmlDir outfp <- asks (view hasLens) :: m HtmlDir
    dbh <- asks (view hasLens) :: m (DatabaseHandle 'IndexedSyms)
    let (dbfp,_) = getDatabaseHandle dbh
    pkgs <- liftIO $ withSQLite dbfp getHomePagePackages
    let hp = renderHtml $ homePageTemplate pkgs RT.renderRoute
    liftIO $ withFile 
                (outfp </> "index.html") 
                WriteMode 
                (\h -> hSetEncoding h utf8 >> TL.hPutStr h hp)
    foldM_ (generatePackPage dbfp outfp (length pkgs)) 1 pkgs
    copyAssets outfp
  where
    generatePackPage :: FilePath -> FilePath -> Int -> Int -> RT.HomePagePackage -> m Int
    generatePackPage !dbfp outfp nbI i hp@(RT.HomePagePackage pack@(RT.PackageName (_,pname)) _) =
        handleAll logErrors $ do
            expmods <- liftIO $ withSQLite dbfp (getPackagePageMods pack)
            logInfoProgress 8 nbI i $ "Generating HTML documentation for " <> pname
            let fp = outfp </> "packages" </> T.unpack pname
                pp = renderHtml $ packagePageTemplate hp expmods RT.renderRoute
            liftIO $ do createDirectoryIfMissing True fp
                        writeUtf8File fp pp
            generateModPage dbfp fp hp `mapM_` expmods 
            pure $ i + 1
      where
        logErrors e = do
            logError $ "[Step 7] ERROR while generating " <> pname <> 
                " HTML documentation: " <> T.pack (displayException e) 
            pure $ i + 1 
    generateModPage :: FilePath -> FilePath -> RT.HomePagePackage -> RT.ModuleName -> m ()
    generateModPage dbfp packfp hp@(RT.HomePagePackage pack _) modn@(RT.ModuleName (_,modnt)) = do
        syms <- liftIO $ withSQLite dbfp (getModulePageSyms pack modn)
        hsyms <- highLightOccs syms
        let mp = renderHtml $ modulePageTemplate hp modn hsyms RT.renderRoute
            fp = packfp </> T.unpack (T.replace "." "-" modnt)
        liftIO $ do createDirectoryIfMissing True fp
                    writeUtf8File fp mp
    -- | TODO: highlighting shouldn't be performed here but directly when extracted from the DB.
    --         This hack has been performed in a rush, should be fixed after V0.
    highLightOccs :: [RT.SymbolOccurs] -> m [RT.HighlightedSymbolOccurs]
    highLightOccs xs = highSyms `mapM` xs
        where
            highSyms (RT.SymbolOccurs sn xs') = do
                hs <- highSym `mapM` xs'
                pure $ RT.HighlightedSymbolOccurs sn hs
            highSym (col, line, SourceCodeFile c p m) = do
                hc <- handleAll highErr $ highLightCode c
                pure (col, line, RT.HighlightedSourceCodeFile (addLineMarker line hc) p m)
              where
                highErr e = do
                    logError $ "[Step 8] HIGHLIGHT ERROR " <> T.pack (displayException e)
                    pure c 
    copyAssets :: FilePath -> m ()
    copyAssets fp = do
        let font = $(embedFile "./src/ExHack/Renderer/templates/static/Inter-UI-Regular.woff") 
            list = $(embedFile "./src/ExHack/Renderer/templates/static/list.min.js") 
            style = $(embedFile "./src/ExHack/Renderer/templates/static/style.css") 
            stat = fp </> "static"
        liftIO $ do
            createDirectoryIfMissing True stat
            BS.writeFile (stat </> "Inter-UI-Regular.woff") font
            BS.writeFile (stat </> "list.min.js") list 
            BS.writeFile (stat </> "style.css") style
    writeUtf8File :: FilePath -> TL.Text -> IO ()
    writeUtf8File fp txt = 
        withFile (fp </> "index.html")
                 WriteMode
                 (\h -> hSetEncoding h utf8 >> TL.hPutStr h txt)
