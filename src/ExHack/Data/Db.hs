module ExHack.Data.Db (
  initDb,
  savePackages,
  savePackage
) where

import Database.HDBC.Types (IConnection, run)

import ExHack.Types (Package)

savePackages :: IConnection c => c -> [Package] -> IO Int
savePackages = undefined

savePackage :: IConnection c => c -> Package -> IO Int
savePackage = undefined

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
