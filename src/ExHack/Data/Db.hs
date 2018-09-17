{-|
Module      : ExHack.Data.Db
Description : Database-related operations.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module ExHack.Data.Db (
    getPkgImportScopes,
    initDb,
    saveModuleExports,
    saveModuleUnifiedSymbols,
    savePackageDeps,
    savePackageMods,
    savePackages
) where

import           Control.Monad          (unless)
import           Control.Monad.Catch    (Exception, MonadMask, throwM)
import qualified Data.HashMap.Strict    as HM (fromList)
import qualified Data.HashSet           as HS (HashSet, fromList)
import           Data.Maybe             (listToMaybe, maybe)
import           Data.Text              (Text, pack)
import           Database.Selda
import           Database.Selda.Backend (MonadSelda (..), SqlValue (SqlInt))
import           ExHack.Types           (ImportsScope, IndexedModuleNameT (..),
                                         IndexedSym (..), LocatedSym (..),
                                         ModuleNameT (..), PackageExports (..),
                                         PackageNameT (..), SourceCodeFile (..),
                                         SymName (..), UnifiedSym (..),
                                         depsNames, getModName, getName)
import qualified ExHack.Types           as T (Package (..))
import           GHC                    (SrcSpan (..), getLoc, srcSpanStartCol,
                                         srcSpanStartLine)

packageId :: Selector (RowID :*: Text :*: Text :*: Text) RowID
packageName :: Selector (RowID :*: Text :*: Text :*: Text) Text
packages :: Table (RowID :*: Text :*: Text :*: Text)
(packages, packageId :*: packageName :*: _ :*: _) 
  = tableWithSelectors "packages" $
              autoPrimary "packageId"
              :*: required "name"
              :*: required "tarball_path"
              :*: required "cabal_file"

dependancies :: Table (RowID :*: RowID :*: RowID)
dependancies = table "dependancies" $
                   autoPrimary "id"
                   :*: required "packID" `fk` (packages, packageId)
                   :*: required "depID" `fk` (packages, packageId)

exposedModules :: Table (RowID :*: Text :*: RowID)
modId :: Selector (RowID :*: Text :*: RowID) RowID
modName :: Selector (RowID :*: Text :*: RowID) Text
modPack :: Selector (RowID :*: Text :*: RowID) RowID
(exposedModules, modId :*: modName :*: modPack) = tableWithSelectors "exposedModules" $
                   autoPrimary "id"
                   :*: required "name"
                   :*: required "packID" `fk` (packages, packageId)

exposedSymbols :: Table (RowID :*: Text :*: RowID)
symId :: Selector (RowID :*: Text :*: RowID) RowID
symName :: Selector (RowID :*: Text :*: RowID) Text
symModId :: Selector (RowID :*: Text :*: RowID) RowID
(exposedSymbols, symId :*: symName :*: symModId) = tableWithSelectors "exposedSymbols" $
                   autoPrimary "id"
                   :*: required "name"
                   :*: required "modId" `fk` (exposedModules, modId)

sourceFiles :: Table (RowID :*: Text :*: Text :*: Text)
fileId :: Selector (RowID :*: Text :*: Text :*: Text) RowID
(sourceFiles, fileId :*: _ :*: _ :*: _)
  = tableWithSelectors "sourceFiles" $
    autoPrimary "id"
    :*: required "fileContent"
    :*: required "modName"
    :*: required "packName"

symbolOccurences :: Table (RowID :*: Int :*: Int :*: RowID :*: RowID)
(symbolOccurences, _ :*: _ :*: _ :*: _) 
  = tableWithSelectors "symbolOccurences" $
    autoPrimary "id"
    :*: required "column"
    :*: required "line"
    :*: required "sourceFileId" `fk` (sourceFiles, fileId)
    :*: required "importedSymID" `fk` (exposedSymbols, symId)


-- | Create the internal database schema.
initDb :: (MonadSelda m) => m ()
initDb = do
    tryCreateTable packages 
    tryCreateTable dependancies 
    tryCreateTable exposedModules
    tryCreateTable exposedSymbols
    tryCreateTable symbolOccurences
    tryCreateTable sourceFiles

-- | Save a package dependancies.
--
-- Note that if we can't a dependancy in the
-- packages table, we'll ignore it.
--
-- You should make sure your package database is already
-- populated before using this.
savePackageDeps :: (MonadSelda m) => T.Package -> m ()
savePackageDeps p = do
    mpid <- queryPkg p
    let resPackDeps = depsNames p 
    mapM_ (\rowId -> saveDep rowId `mapM_` resPackDeps) mpid
  where
    saveDep pid d = do
      mdid <- query $ do 
        pks <- select packages
        restrict (pks ! packageName .== text (pack d))
        return $ pks ! packageId
      mapM_ (\depId -> insert_ dependancies [ def :*: depId :*: pid ]) (listToMaybe mdid)

-- | Save a package list in the DB.
savePackages :: (MonadSelda m) => [T.Package] -> m ()
savePackages xs = insert_ packages $
    (\p -> def :*: getName p :*: T.cabalFile p :*: (pack . T.tarballPath) p) <$> xs 

data SaveModuleException = PackageNotInDatabase
    deriving (Show)

instance Exception SaveModuleException

-- | Potentially confusing:
--   * If we have a package id in the Package type, use it
--   * Otherwise retrieve the package id from the DB
--   * If the package is not in the DB, something weird happened...
--     Throw an error
getPackageId :: forall m. (MonadSelda m, MonadMask m)
             => T.Package -> m RowID
getPackageId p = maybe
    (queryPkg p >>= maybe (throwM PackageNotInDatabase) pure)
    pure
    (T.dbId p)

-- | Save the exposed modules as well as their exposed symbols.
savePackageMods :: forall m. (MonadSelda m, MonadMask m) 
                => PackageExports -> m ()
savePackageMods (PackageExports (p, _, xs)) = do
    pid <- getPackageId p
    saveMod pid `mapM_` xs
  where
    saveMod pid (m, syms) = do
        mid <- insertWithPK exposedModules [def :*: getModName m :*: pid]
        insert_  exposedSymbols $ (\(SymName sn) -> def :*: sn :*: mid) <$> syms

-- | Given a module database ID, saves the exported symbols of this
--   module in ExHack's database.
saveModuleExports :: (MonadSelda m) => Int -> [SymName] -> m ()
saveModuleExports midi xs = insert_ exposedSymbols $ 
    (\(SymName s) -> def :*: s :*: fromSql (SqlInt midi)) <$> xs

queryPkg :: (MonadSelda m) => T.Package -> m (Maybe RowID)
queryPkg p = do
    let r = query $ do
            pks <- select packages
            restrict (pks ! packageName .== (text . getName) p)
            return $ pks ! packageId 
    listToMaybe <$> r

getPkgModules :: (MonadSelda m, MonadMask m) => T.Package -> m [IndexedModuleNameT]
getPkgModules p = do
    pid <- getPackageId p
    q <- query $ do
        mods <- select exposedModules
        restrict (mods ! modPack .== literal pid)
        return (mods ! modId :*: mods ! modName)
    pure $ wrapResult <$> q 
  where
    wrapResult (i :*: n) = IndexedModuleNameT (ModuleNameT n, fromRowId i)

-- | Query ExHack database to retrieve the available symbols to be imported
--   from within this package.
--
--   This scope should be filtered on a per-module basis, depending on the module
--   imports, before being used in a symbol unification.
getPkgImportScopes :: forall m. (MonadSelda m, MonadMask m) => T.Package -> m ImportsScope
getPkgImportScopes p = do
    mods <- getPkgModules p
    o <- sequence (wrapSyms <$> mods)
    pure $ HM.fromList o
  where
    wrapSyms :: IndexedModuleNameT -> m (IndexedModuleNameT, HS.HashSet IndexedSym)
    wrapSyms mnt@(IndexedModuleNameT (_, i)) = do
        let mid = fromSql $ SqlInt i :: RowID
        q <- query $ do
            mods <- select exposedModules 
            syms <- select exposedSymbols
            restrict (mods ! modId .== literal mid)
            restrict (syms ! symModId .== mods ! modId)
            pure $ syms ! symId :*: syms ! symName
        pure (mnt, HS.fromList (wrapResult <$> q)) 
    wrapResult (i :*: n) = IndexedSym (SymName n, fromRowId i)

-- | Insert both the source file in which some symbols have been unified as well as 
--   the symbols occurences in ExHack's database.
saveModuleUnifiedSymbols :: forall m. (MonadSelda m, MonadMask m) => [UnifiedSym] -> SourceCodeFile -> m ()
saveModuleUnifiedSymbols xs (SourceCodeFile f (ModuleNameT mnt) (PackageNameT pnt)) = 
    unless (null xs) $ do
        fid <- insertWithPK sourceFiles [def :*:  f :*: mnt :*: pnt]
        insert_ symbolOccurences $ generateLine fid <$> xs
  where
      generateLine fid (UnifiedSym (IndexedSym (_, sidi), LocatedSym (_, _, gloc))) = 
          def :*: col :*: line :*: fid :*: sid 
        where
          (RealSrcSpan loc) = getLoc gloc
          !line = srcSpanStartLine loc
          !col = srcSpanStartCol loc
          !sid = fromSql (SqlInt sidi)
