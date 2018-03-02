{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad.State
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import Database.HDBC.Types (commit, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory (getDirectoryContents, doesFileExist, removeFile)

import ExHack.Stackage.StackageTypes
import ExHack.Stackage.StackageParser
import ExHack.Data.Db (initDb)

import Log (logProgress, logTitle)
import Cli (step, promptUser)

main :: IO ()
main = do
  step "Step 0: Init DB" isDb step0
  -- Retrieving cabal URLs
  logTitle "STEP 1: Parsing stackage LTS-10.5"
  stackageYaml <- readFile "./data/lts-10.5.yaml"  
  let packages = fromJust (getHackageCabalUrl <$> parseStackageYaml stackageYaml)
  -- Downloading cabal files
  step "STEP 2: Downloading cabal files." shouldDlCabalFiles (step2 packages)

step0 :: IO ()
step0 = do
  c <- connectSqlite3 "test.db"
  initDb c
  commit c
  disconnect c

isDb :: IO Bool
isDb = do
  f <- doesFileExist "test.db"
  if f
    then do
      del <- promptUser "A database is already here, delete it?"
      if del
        then putStrLn "Deleting..." >> removeFile "test.db" >> return True
        else return False
    else return True

step2 :: [(Text, Text)] -> IO ()
step2 packages = do
  let settings = managerSetProxy
        (proxyEnvironment Nothing)
        tlsManagerSettings
  m <- newManager settings
  foldr (dlFoldCabalFiles m (length packages)) (return 1) packages
  return ()

shouldDlCabalFiles :: IO Bool
shouldDlCabalFiles = do
  f <- getDirectoryContents "cabal/"
  if not (null f)
    then do
      r <- promptUser "Looks like your cabal directory already contains cabal files, wanna skip\
              \ this step?"
      if r then putStrLn "Skipping..." >> return False else return True
    else
      return True

dlFoldCabalFiles :: Manager -> Int -> (Text,Text) -> IO Int -> IO Int
dlFoldCabalFiles man totalSteps pack step = do 
  step <- step
  downloadCabalFile man pack
  logProgress "----" ("["++ show step ++ "/" ++ show totalSteps ++ "] " ++ T.unpack (fst pack))
  return $ step + 1

downloadCabalFile :: Manager -> (Text,Text) -> IO ()
downloadCabalFile m (name, url) = do
  f <- httpLbs (parseRequest_ $ T.unpack url) m 
  writeFile ("cabal/" ++ T.unpack name ++ ".cabal") $ getResponse f 
  return ()
  where
    getResponse f = E.decodeUtf8 . toStrict $ responseBody f
