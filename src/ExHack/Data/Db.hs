module ExHack.Data.Db (
  initDb,
  savePackages,
  savePackageDeps
) where

import Data.Maybe          (catMaybes)
import Database.HDBC       (quickQuery', fromSql)
import Database.HDBC.Types (IConnection, run, executeMany, toSql,
                            prepare, SqlValue)
import ExHack.Types (Package(..), getName, depsNames)

savePackageDeps :: IConnection c => c -> Package -> IO ()
savePackageDeps c p = do
  pkgId <- toSql <$> getPkgId (getName p)
  depsId <- getDepsIds p
  stm <- prepare c "INSERT INTO DEPENDENCIES (PACKID, DEPID) VALUES (?,?)"
  executeMany stm $ buildParams pkgId (catMaybes depsId)
  where
    buildParams :: SqlValue -> [Int] -> [[SqlValue]]
    buildParams pId dIds = (\di -> [pId, toSql di]) <$> dIds
    getPkgId :: String -> IO (Maybe Int)
    getPkgId dn = packageId <$> quickQuery' c "SELECT ID FROM PACKAGES WHERE NAME = ?" [toSql dn] 
    packageId :: [[SqlValue]] -> Maybe Int
    packageId xs = if null xs then Nothing else fromSql . head $ head xs
    getDepsIds pkg = mapM getPkgId $ depsNames pkg 

savePackages :: IConnection c => c -> [Package] -> IO ()
savePackages c p = do
  stm <- prepare c "INSERT INTO PACKAGES (NAME, CABAL_FILE, TARBALL_PATH) VALUES (?, ?, ?)"
  executeMany stm sqlValues
  where
    !sqlValues = getPackageParams <$> p
    getPackageParams pack = [toSql $ getName pack, 
                             toSql $ cabalFile pack, 
                             toSql $ tarballPath pack]

initDb :: IConnection c => c -> IO ()
initDb c = do
             _ <- run c "CREATE TABLE PACKAGES(\
                   \ ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
                   \ NAME TEXT,\
                   \ TARBALL_PATH TEXT,\
                   \ CABAL_FILE TEXT);" []
             _ <- run c "CREATE TABLE DEPENDENCIES(\
                   \ ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
                   \ PACKID INTEGER,\
                   \ DEPID INTEGER,\
                   \ FOREIGN KEY (PACKID) REFERENCES PACKAGES(ID),\
                   \ FOREIGN KEY (DEPID) REFERENCES PACKAGES(ID));" [] 
             return ()
