{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ExHack.Data.Db (
  initDb,
  savePackages,
  savePackageDeps,
  savePackageMods
) where

import Data.Maybe                  (listToMaybe)
import Data.Text                   (Text, pack)
import Database.Selda    
import qualified ExHack.Types as T (Package(..))
import ExHack.Types                (ModuleName, getName, depsNames)

exposedModules :: Table (RowID :*: RowID :*: Text)
exposedModules = table "exposedModules" $
                   autoPrimary "id"
                   :*: required "packID" `fk` (packages, packageId)
                   :*: required "name"

dependancies :: Table (RowID :*: RowID :*: RowID)
dependancies = table "dependancies" $
                   autoPrimary "id"
                   :*: required "packID" `fk` (packages, packageId)
                   :*: required "depID" `fk` (packages, packageId)

packageId :: Selector (RowID :*: Text :*: Text :*: Text) RowID
packageName :: Selector (RowID :*: Text :*: Text :*: Text) Text
packages :: Table (RowID :*: Text :*: Text :*: Text)
(packages, packageId :*: packageName :*: _ :*: _) 
  = tableWithSelectors "packages" $
              autoPrimary "packageId"
              :*: required "name"
              :*: required "tarball_path"
              :*: required "cabal_file"

-- | Create the internal database schema.
initDb :: SeldaM ()
initDb = tryCreateTable packages >> tryCreateTable dependancies >> tryCreateTable exposedModules

-- | Save a package dependancies.
--
-- Note that if we can't a dependancy in the
-- packages table, we'll ignore it.
--
-- You should make sure your package database is already
-- populated before using this.
savePackageDeps :: T.Package -> SeldaM ()
savePackageDeps p = do
  mpid <- query $ do
    pks <- select packages
    restrict (pks ! packageName .== (text . getName) p)
    return $ pks ! packageId 
  let resPackDeps = depsNames p 
  mapM_ (\rowId -> saveDep rowId `mapM_` resPackDeps) (listToMaybe mpid)
  where
    saveDep :: RowID -> String -> SeldaM ()
    saveDep pid d = do
      mdid <- query $ do 
        pks <- select packages
        restrict (pks ! packageName .== text (pack d))
        return $ pks ! packageId
      mapM_ (\depId -> insert_ dependancies [ def :*: depId :*: pid ]) (listToMaybe mdid)

-- | Save a package list in the DB.
savePackages :: [T.Package] -> SeldaM ()
savePackages xs = (insert_ packages . generateCols) `mapM_` xs 
  where
    generateCols p = [def :*: getName p :*: T.cabalFile p :*: (pack . T.tarballPath) p ]

-- | Save the exposed modules of a package in the DB.
savePackageMods :: Maybe [ModuleName] -> RowID -> SeldaM ()
savePackageMods (Just xs) pid = (insert_ exposedModules . generateCols) `mapM_` xs
  where
    generateCols m = [def :*: pid :*: pack (show m)] 
savePackageMods _ _ = pure ()
