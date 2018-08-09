{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ExHack.Data.Db (
  initDb,
  savePackages,
  savePackageDeps,
  savePackageMods,
  saveModuleSymbols
) where

import Data.Maybe                  (listToMaybe)
import Data.Text                   (Text, pack)
import Database.Selda    
import qualified ExHack.Types as T (Package(..))
import ExHack.Types                (ModuleName, SymbolName(..), getName, depsNames)

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
  mpid <- query $ do
    pks <- select packages
    restrict (pks ! packageName .== (text . getName) p)
    return $ pks ! packageId 
  let resPackDeps = depsNames p 
  mapM_ (\rowId -> saveDep rowId `mapM_` resPackDeps) (listToMaybe mpid)
  where
    saveDep pid d = do
      mdid <- query $ do 
        pks <- select packages
        restrict (pks ! packageName .== text (pack d))
        return $ pks ! packageId
      mapM_ (\depId -> insert_ dependancies [ def :*: depId :*: pid ]) (listToMaybe mdid)

-- | Save a package list in the DB.
savePackages :: (MonadSelda m) => [T.Package] -> m ()
savePackages xs = insert_ packages $ generateCols <$> xs 
  where
    generateCols p = def :*: getName p :*: T.cabalFile p :*: (pack . T.tarballPath) p

-- | Save the exposed modules of a package in the DB.
-- TODO: Why did I end up with a maybe moduleName???
savePackageMods :: (MonadSelda m) => Maybe [ModuleName] -> RowID -> m ()
savePackageMods (Just xs) pid = insert_ exposedModules $ generateCols <$> xs
  where
    generateCols m = def :*: pack (show m) :*: pid 
savePackageMods _ _ = pure ()

saveModuleSymbols :: (MonadSelda m) => RowID -> [SymbolName] -> m ()
saveModuleSymbols mid xs = insert_ exposedSymbol $ generateCols <$> xs
    where
      generateCols (SymbolName s) = def :*: s :*: mid 
