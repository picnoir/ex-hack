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
import ExHack.Types      (Package(..), getName)

packages :: Table (RowID :*: Text :*: Text :*: Text)
(packages, packageId :*: packageName :*: _ :*: _) 
  = tableWithSelectors "package" $
              autoPrimary "packageId"
              :*: required "name"
              :*: required "tarball_path"
              :*: required "cabal_file"

initDb :: SeldaM ()
initDb = tryCreateTable packages >> tryCreateTable dependancies 

savePackageDeps :: Package -> SeldaM ()
savePackageDeps p = do
  packId <- query $ do
    pack <- select packages
    restrict (pack ! packageName .== (text . getName) p)
    return . listToMaybe $ pack ! packageId 
  insert_ dependancies [ x :*: x ]
  return ()
    
--pkgId <- toSql <$> getPkgId (getName p)
--depsId <- getDepsIds p
--stm <- prepare c "INSERT INTO DEPENDENCIES (PACKID, DEPID) VALUES (?,?)"
--executeMany stm $ buildParams pkgId (catMaybes depsId)
--where
--  buildParams :: SqlValue -> [Int] -> [[SqlValue]]
--  buildParams pId dIds = (\di -> [pId, toSql di]) <$> dIds
--  getPkgId :: String -> IO (Maybe Int)
--  getPkgId dn = packageId <$> quickQuery' c "SELECT ID FROM PACKAGES WHERE NAME = ?" [toSql dn] 
--  packageId :: [[SqlValue]] -> Maybe Int
--  packageId xs = if null xs then Nothing else fromSql . head $ head xs
--  getDepsIds pkg = mapM getPkgId $ depsNames pkg 

savePackages :: [Package] -> SeldaM ()
savePackages xs = (insert_ packages . generateCols) `mapM_` xs 
  where
    generateCols p = [def :*: getName p :*: cabalFile p :*: (pack . tarballPath) p]


dependancies :: Table (RowID :*: RowID :*: RowID)
dependancies = table "dependancies" $
                   autoPrimary "id"
                   :*: required "packID" `fk` (packages, packageId)
                   :*: required "depID" `fk` (packages, packageId)

