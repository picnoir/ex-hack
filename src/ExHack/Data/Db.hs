{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ExHack.Data.Db (
  initDb,
  savePackages,
  savePackageDeps
) where

import Data.Maybe        (listToMaybe)
import Data.Text         (Text, pack)
import Database.Selda    
import ExHack.Types      (Package(..), getName,
                          depsNames)

dependancies :: Table (RowID :*: RowID :*: RowID)
dependancies = table "dependancies" $
                   autoPrimary "id"
                   :*: required "packID" `fk` (packages, packageId)
                   :*: required "depID" `fk` (packages, packageId)

packages :: Table (RowID :*: Text :*: Text :*: Text)
(packages, packageId :*: packageName :*: _ :*: _) 
  = tableWithSelectors "packages" $
              autoPrimary "packageId"
              :*: required "name"
              :*: required "tarball_path"
              :*: required "cabal_file"

-- | Create the internal database schema.
initDb :: SeldaM ()
initDb = tryCreateTable packages >> tryCreateTable dependancies 

-- | Save a package dependancies.
--
-- Note that if we can't a dependancy in the
-- packages table, we'll ignore it.
--
-- You should make sure your package database is already
-- populated before using this.
savePackageDeps :: Package -> SeldaM ()
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
savePackages :: [Package] -> SeldaM ()
savePackages xs = (insert_ packages . generateCols) `mapM_` xs 
  where
    generateCols p = [def :*: getName p :*: cabalFile p :*: (pack . tarballPath) p]


