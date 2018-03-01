module ExHack.Data.Db (
  initDb,
  savePackages,
  savePackageDeps
) where

import Database.HDBC       (quickQuery', fromSql)
import Database.HDBC.Types (IConnection, run, executeMany, toSql,
                            prepare, SqlValue)
import ExHack.Types (Package(..), getName, depsNames)

savePackageDeps :: IConnection c => c -> Package -> IO ()
savePackageDeps c p = do
  pkgId <- toSql <$> getPkgId (getName p)
  depsId <- getDepsIds p
  stm <- prepare c "INSERT INTO DEPENDENCIES (PACKID, DEPID) VALUES (?,?)"
  executeMany stm $ buildParams pkgId depsId 
  where
    
    buildParams :: SqlValue -> [Int] -> [[SqlValue]]
    buildParams pId dIds = (\di -> [pId, toSql di]) <$> dIds
    getPkgId :: String -> IO Int
    getPkgId dn = fromSql . head . head <$> quickQuery' c "SELECT ID FROM PACKAGES WHERE NAME = ?" [toSql dn] 
    getDepsIds pkg = mapM getPkgId $ depsNames pkg 

savePackages :: IConnection c => c -> [Package] -> IO ()
savePackages c p = do
  stm <- prepare c "INSERT INTO PACKAGES (NAME) VALUES (?)"
  executeMany stm sqlValues
  where
    sqlValues = ((:[]) . toSql . getName) <$> p

initDb :: IConnection c => c -> IO ()
initDb c = do
             _ <- run c "CREATE TABLE PACKAGES(\
                   \ ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
                   \ NAME TEXT)" []
             _ <- run c "CREATE TABLE DEPENDENCIES(\
                   \ ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\
                   \ PACKID INTEGER,\
                   \ DEPID INTEGER,\
                   \ FOREIGN KEY (PACKID) REFERENCES PACKAGES(ID),\
                   \ FOREIGN KEY (DEPID) REFERENCES PACKAGES(ID));" [] 
             return ()
