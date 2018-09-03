{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module ProcessingSteps (
    generateDb,
    parseStackage,
    dlAssets,
    genGraphDep,
    retrievePkgsExports,
    indexSymbols
) where

import qualified Data.ByteString.Lazy as BL (writeFile)
import qualified Data.ByteString as BS (readFile)
import Data.Maybe (fromJust)
import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.HashMap.Strict (filterWithKey)
import qualified Data.Text as T (unpack, pack)
import qualified Data.Text.IO as T (readFile)
import Database.Selda (SeldaM)
import Database.Selda.SQLite (withSQLite)

import ExHack.Cabal.CabalParser (parseCabalFile, getSuccParse)
import ExHack.Ghc (getModImports, getModSymbols)
import ExHack.Utils (Has(..))
import ExHack.Hackage.Hackage (unpackHackageTarball, getPackageExports)
import ExHack.Stackage.StackageParser (getHackageUrls,
                                       parseStackageYaml)
import ExHack.Types (MonadStep, DatabaseHandle,
                     DatabaseStatus(..), PackageDlDesc,
                     StackageFile, StackageFile(..),
                     TarballsDir(..), CabalFilesDir(..), PackageDlDesc(..),
                     TarballDesc(..), Package(tarballPath, allModules),
                     PackageExports(..), WorkDir(..),
                     MonadLog(..), ImportsScope, PackageComponent(..), ModuleName,
                     ComponentRoot, PackageFilePath(..), IndexedModuleNameT(..),
                     LocatedSym(..), UnifiedSym(..), packagedlDescName, logInfo)
import ExHack.Data.Db (initDb, savePackages, savePackageDeps,
                       savePackageMods, getPkgImportScopes, saveUnifiedSymbols)
import Network.HTTP.Client (managerSetProxy, proxyEnvironment,
                            newManager, Manager, httpLbs,
                            parseRequest_, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- general TODO: properly catch database exceptions

generateDb :: forall c m. 
    (Has c (DatabaseHandle 'New), 
     MonadStep c m) 
    => m (DatabaseHandle 'Initialized)
generateDb = do
    fp <- asks (view hasLens)
    withSQLite fp initDb
    pure fp

parseStackage :: forall c m.
    (Has c StackageFile,
     MonadStep c m)
    => m [PackageDlDesc]
parseStackage = do
    (StackageFile stackageYaml) <- asks (view hasLens)
    let packages = fromJust $ parseStackageYaml stackageYaml 
    pure $ getHackageUrls packages

dlAssets :: forall c m.
    (Has c TarballsDir,
     Has c CabalFilesDir,
     MonadStep c m)
    => [PackageDlDesc] -> m ()
dlAssets packages = do
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
    dlFoldCabalFiles !cd !td man totalSteps !p step = do 
        step' <- step
        let pn = packagedlDescName p
        downloadHackageFiles cd td man p
        logInfo ("----" <> ("[" <> T.pack (show step') <> "/" <> T.pack (show totalSteps) <> "] " <> pn))
        return $ step' + 1
    downloadHackageFiles :: CabalFilesDir -> TarballsDir -> Manager -> PackageDlDesc -> m ()
    downloadHackageFiles 
      (CabalFilesDir cabalFilesDir) (TarballsDir tarballsDir) man 
      (PackageDlDesc (name, cabalUrl, tarballUrl)) = 
        liftIO $ do
            f <- httpLbs (parseRequest_ $ T.unpack cabalUrl) man 
            BL.writeFile (cabalFilesDir ++ T.unpack name ++ ".cabal") $ responseBody f 
            f' <-  httpLbs (parseRequest_ $ T.unpack tarballUrl) man
            BL.writeFile (tarballsDir <> T.unpack name <> ".tar.gz") $ responseBody f' 
            return ()

genGraphDep :: forall c m.
    (Has c TarballsDir,
     Has c CabalFilesDir,
     Has c (DatabaseHandle 'Initialized),
     MonadStep c m)
    => [PackageDlDesc] -> m (DatabaseHandle 'DepsGraph, [Package])
genGraphDep pd = do
    dbHandle <- asks (view hasLens)
    tbd <- asks (view hasLens)
    cd <- asks (view hasLens)
    logInfo "[+] Parsing cabal files."
    pkgs <- readPkgsFiles cd tbd `mapM` pd
    let pkgs' = getSuccParse (parseCabalFile <$> pkgs)
    -- 2
    liftIO $ withSQLite dbHandle $ do
        logInfo "[+] Saving packages to DB..."
        savePackages pkgs'
        logInfo "[+] Done."
        -- 3
        logInfo "[+] Saving dependancies to DB..."
        _ <- foldr (foldInsertDep (length pkgs)) (return 1) pkgs'
        logInfo "[+] Done."
        return ()
    pure (dbHandle, pkgs')
  where
    readPkgsFiles :: CabalFilesDir -> TarballsDir -> PackageDlDesc -> m TarballDesc
    readPkgsFiles (CabalFilesDir cabalFilesDir) (TarballsDir tarballsDir) p = do
        let tp = tarballsDir <> T.unpack (packagedlDescName p) <> ".tar.gz"
        cf <- liftIO $ T.readFile $ cabalFilesDir <> T.unpack (packagedlDescName p) <> ".cabal"
        pure $ TarballDesc (tp,cf)
    foldInsertDep :: Int -> Package -> SeldaM Int -> SeldaM Int
    foldInsertDep totalDeps pkg step = do 
      step' <- step
      savePackageDeps pkg
      logInfo $ "----" <> "[" <> T.pack (show step') <> "/" <> T.pack (show totalDeps) 
                <> "] " <> T.pack (show pkg)
      return $ step' + 1

retrievePkgsExports :: forall c m.
    (Has c WorkDir,
     Has c (DatabaseHandle 'DepsGraph),
     MonadStep c m)
   => [Package] -> m (DatabaseHandle 'PkgExports, [PackageExports])
retrievePkgsExports pkgs = do
    dbHandle <- asks (view hasLens)
    wd <- asks (view hasLens) 
    pkgsExports <- getPkgExports wd `mapM` pkgs
    let seldaActions = savePackageMods <$> pkgsExports
    _ <- liftIO $ withSQLite dbHandle $ sequence seldaActions
    pure (dbHandle, pkgsExports)
  where
    -- TODO think about error handling here.
    getPkgExports :: WorkDir -> Package -> m PackageExports
    getPkgExports (WorkDir wd) p = do
        tb <- liftIO . BS.readFile $ tarballPath p
        tbp <- unpackHackageTarball wd tb  
        getPackageExports tbp p

indexSymbols :: forall c m.
    (MonadStep c m,
     Has c (DatabaseHandle 'PkgExports))
  => [PackageExports] -> m ()
indexSymbols pkgs = do
    dbh <- asks (view hasLens)
    indexPackage dbh `mapM_` pkgs 
  where
    indexPackage :: DatabaseHandle 'PkgExports -> PackageExports -> m ()
    indexPackage !dbh (PackageExports (p, pfp, _)) = do
        is <- liftIO $ withSQLite dbh $ getPkgImportScopes p
        indexComponent dbh pfp is `mapM_` allModules p 
        pure ()
    indexComponent :: DatabaseHandle 'PkgExports -> PackageFilePath -> ImportsScope 
                   -> PackageComponent -> m ()
    indexComponent dbh pfp is pc = do
        mfps <- findModuleFilePath pfp (roots pc) `mapM` mods pc
        indexModule dbh pfp is `mapM_` mfps
    indexModule :: DatabaseHandle 'PkgExports -> PackageFilePath -> ImportsScope 
                -> (ModuleName, ComponentRoot) -> m ()
    indexModule dbh (PackageFilePath pfp) is (mn,cr) = do
        imports <- getModImports pfp cr mn 
        let fis = filterWithKey (\(IndexedModuleNameT (n, _)) _ -> n `elem` imports) is
        syms <- getModSymbols pfp cr mn
        let unsyms = unifySymbols fis syms
        withSQLite dbh $  saveUnifiedSymbols unsyms
        undefined -- Run selda actions in DB 
    findModuleFilePath :: PackageFilePath -> [ComponentRoot] -> ModuleName -> m (ModuleName, ComponentRoot)
    findModuleFilePath = undefined
    unifySymbols :: ImportsScope -> [LocatedSym] -> [UnifiedSym]
    unifySymbols = undefined
-- 3. For each file/module
--    a. See what's imported => Get pack id + query db
--    b. Create a "scope"
--    c. Get the symbols
--    d. Filter the symbols.
