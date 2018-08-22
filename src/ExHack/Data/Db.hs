{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExHack.Data.Db (
    mkHandle,
    initDb,
    savePackages,
    savePackageDeps,
    savePackageMods,
    saveModuleExports 
) where

import Control.Monad.Catch         (MonadMask, Exception, throwM)
import Data.Maybe                  (listToMaybe, maybe)
import Data.Text                   (Text, pack)
import Database.Selda    
import Database.Selda.Backend      (MonadSelda(..))
import qualified ExHack.Types as T (Package(..))
import ExHack.Types                (DatabaseHandle, DatabaseStatus(..),
                                    PackageExports(..), SymbolName(..), getName, 
                                    getModName, depsNames)

mkHandle :: FilePath -> DatabaseHandle 'New
mkHandle = id

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

exposedSymbol :: Table (RowID :*: Text :*: RowID)
exposedSymbol = table "exposedModules" $
                   autoPrimary "id"
                   :*: required "name"
                   :*: required "modId" `fk` (exposedModules, modId)

-- | Create the internal database schema.
initDb :: (MonadSelda m) => m ()
initDb = tryCreateTable packages >> tryCreateTable dependancies >> tryCreateTable exposedModules

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

-- | Save the exposed modules of a package in the DB.
savePackageMods :: forall m. (MonadSelda m, MonadMask m) 
                => PackageExports -> m ()
savePackageMods (PackageExports pe) = do
    let !p = fst pe
        !xs = snd pe
        !mpid = T.dbId p
    -- Potentially confusing:
    --   * If we have a package id in the Package type, use it
    --   * Otherwise retrieve the package id from the DB
    --   * If the package is not in the DB, something weird happened...
    --     Throw an error
    pid <- maybe
            (queryPkg p >>= maybe (throwM PackageNotInDatabase) pure)
            pure
            mpid
    insert_ exposedModules $ 
        (\(m,_) -> def :*: getModName m :*: pid) <$>  xs

saveModuleExports :: (MonadSelda m) => RowID -> [SymbolName] -> m ()
saveModuleExports mid xs = insert_ exposedSymbol $ 
    (\(SymbolName s) -> def :*: s :*: mid) <$> xs

queryPkg :: (MonadSelda m) => T.Package -> m (Maybe RowID)
queryPkg p = do
    let r = query $ do
            pks <- select packages
            restrict (pks ! packageName .== (text . getName) p)
            return $ pks ! packageId 
    listToMaybe <$> r
