{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module ExHack.ProcessingSteps (
    generateDb,
    parseStackage,
    dlAssets,
    genGraphDep,
    retrievePkgsExports,
    indexSymbols
) where

import Control.Lens (view)
import Control.Monad (filterM)
import Control.Monad.Catch (throwM, MonadCatch, MonadThrow)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import qualified Data.ByteString.Lazy as BL (writeFile)
import qualified Data.ByteString as BS (readFile)
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.HashMap.Strict as HM (HashMap, filterWithKey, empty, elems,
                                            insert, lookup)
import qualified Data.HashSet as HS (unions, foldl')
import Data.List (foldl')
import qualified Data.Text as T (unpack, pack)
import qualified Data.Text.IO as T (readFile)
import Database.Selda (SeldaM)
import Database.Selda.SQLite (withSQLite)
import Distribution.ModuleName (toFilePath)
import System.Directory (doesFileExist)

import ExHack.Cabal.CabalParser (parseCabalFile, getSuccParse)
import ExHack.Ghc (getModImports, getModSymbols, unLoc)
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
                     ComponentRoot(..), PackageFilePath(..), IndexedModuleNameT(..),
                     LocatedSym(..), UnifiedSym(..), ModuleFileError(..), 
                     IndexedSym(..), SymName, packagedlDescName, logInfo)
import ExHack.Data.Db (initDb, savePackages, savePackageDeps,
                       savePackageMods, getPkgImportScopes, saveModuleUnifiedSymbols)
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
    let seldaActions = savePackageMods `mapM` pkgsExports
    _ <- liftIO $ withSQLite dbHandle $ seldaActions
    pure (dbHandle, pkgsExports)
  where
    -- TODO think about error handling here.
    getPkgExports :: WorkDir -> Package -> m PackageExports
    getPkgExports (WorkDir wd) p = do
        tb <- liftIO . BS.readFile $ tarballPath p
        tbp <- unpackHackageTarball wd tb  
        getPackageExports tbp p

-- | Indexes the code source symbols in the database.
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
  => [PackageExports] -> m ()
indexSymbols pkgs = do
    dbh <- asks (view hasLens)
    indexPackage dbh `mapM_` pkgs 
  where
    indexPackage :: DatabaseHandle 'PkgExports -> PackageExports -> m ()
    indexPackage !dbh (PackageExports (p, pfp, _)) = do
        is <- liftIO $ withSQLite dbh $ getPkgImportScopes p
        indexComponent dbh p pfp is `mapM_` allModules p 
        pure ()
    indexComponent :: DatabaseHandle 'PkgExports -> Package -> PackageFilePath -> ImportsScope 
                   -> PackageComponent -> m ()
    indexComponent dbh p pfp is pc = do
        mfps <- findModuleFilePath pfp (roots pc) `mapM` mods pc
        indexModule dbh p pfp is `mapM_` mfps
    indexModule :: DatabaseHandle 'PkgExports -> Package -> PackageFilePath -> ImportsScope 
                -> (ModuleName, ComponentRoot) -> m ()
    indexModule dbh p (PackageFilePath pfp) is (mn,cr) = do
        imports <- getModImports pfp cr mn 
        -- fis: filtered import scope according to this module imports
        -- isyms: imported symbols hashmap on which we will perform the unification
        let !fis = HM.filterWithKey (\(IndexedModuleNameT (n, _)) _ -> n `elem` imports) is
            !isyms = HS.unions $ HM.elems fis
            !isymsMap = HS.foldl' (\hm is'@(IndexedSym (n, _)) -> HM.insert n is' hm) HM.empty isyms 
        syms <- getModSymbols p pfp cr mn
        let !unsyms = unifySymbols isymsMap syms
            -- TODO: Move file searching from GHC to somewhere I can access from here.
            file = undefined
        withSQLite dbh $ saveModuleUnifiedSymbols unsyms file 
    findModuleFilePath :: PackageFilePath -> [ComponentRoot] -> ModuleName -> m (ModuleName, ComponentRoot)
    findModuleFilePath (PackageFilePath pfp) crs mn = do
        mcr <- listToMaybe <$> filterM testCr crs 
        maybe (throwM $ CannotFindModuleFile $ show mn) (\cr -> pure (mn, cr)) mcr
      where
        testCr (ComponentRoot cr) = liftIO $ doesFileExist $ pfp <> cr <> toFilePath mn
    unifySymbols :: HM.HashMap SymName IndexedSym -> [LocatedSym] -> [UnifiedSym]
    unifySymbols isyms = foldl' foldLSym []
      where
        foldLSym xs ls@(LocatedSym (_, _, locSym)) = 
            maybe xs (\is -> UnifiedSym(is,ls) : xs) (HM.lookup (unLoc locSym) isyms) 
