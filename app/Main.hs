{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad.State
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import Data.List (isSuffixOf)
import Database.HDBC.Types (commit, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory (getDirectoryContents, doesFileExist, 
                         removeFile, listDirectory)

import ExHack.Cabal.CabalParser (parseCabalFile, getSuccParse)
import ExHack.Stackage.StackageTypes
import ExHack.Stackage.StackageParser
import ExHack.Types (Package(..))
import ExHack.Data.Db (initDb, savePackages,
                       savePackageDeps)

import Config (cabalFilesDir, tarballsDir, dbFilePath, dataDir)
import Cli (step, promptUser, PreCondition)
import Log (logProgress, logTitle)

main :: IO ()
main = do
  step "[+] STEP 0: Creating DB" isDb step0
  logTitle "[+] STEP 1: Parsing stackage LTS-10.5"
  stackageYaml <- readFile "./data/lts-10.5.yaml"  
  let packages = fromJust $ parseStackageYaml stackageYaml
  step "[+] STEP 2: Downloading hackage files (cabal builds + tarballs)" shouldDlCabalFiles (step2 $ getHackageUrls packages)
  step "[+] STEP 3: Generating dependancy graph" (return True) step3

step0 :: IO ()
step0 = do
  c <- connectSqlite3 dbFilePath
  initDb c
  commit c
  disconnect c

isDb :: PreCondition
isDb = do
  f <- doesFileExist dbFilePath
  if f
    then do
      del <- promptUser "A database has already been generated, delete it?"
      if del
        then putStrLn "Deleting..." >> removeFile dbFilePath >> return True
        else return False
    else return True

step2 :: [(Text, Text, Text)] -> IO ()
step2 packages = do
  let settings = managerSetProxy
        (proxyEnvironment Nothing)
        tlsManagerSettings
  m <- newManager settings
  foldr (dlFoldCabalFiles m (length packages)) (return 1) packages
  return ()

shouldDlCabalFiles :: PreCondition
shouldDlCabalFiles = do
  c <- getDirectoryContents cabalFilesDir
  t <- getDirectoryContents tarballsDir
  if not (null $ c `mappend` t)
    then do
      r <- promptUser "Looks like your data directory is not empty, wanna skip\
              \ this step?"
      if r then putStrLn "Skipping..." >> return False else return True
    else
      return True

dlFoldCabalFiles :: Manager -> Int -> (Text,Text,Text) -> IO Int -> IO Int
dlFoldCabalFiles man totalSteps p@(pn, _, _) step = do 
  step <- step
  downloadHackageFiles man p
  logProgress "----" ("["++ show step ++ "/" ++ show totalSteps ++ "] " ++ T.unpack pn)
  return $ step + 1

downloadHackageFiles :: Manager -> (Text,Text,Text) -> IO ()
downloadHackageFiles m (name, cabalUrl, tarballUrl) = do
  f <- httpLbs (parseRequest_ $ T.unpack cabalUrl) m 
  writeFile (cabalFilesDir ++ T.unpack name ++ ".cabal") $ getResponse f 
  f <- httpLbs (parseRequest_ $ T.unpack tarballUrl) m
  writeFile (tarballsDir ++ T.unpack name ++ ".tar.gz") $ getResponse f 
  return ()
  where
    getResponse f = E.decodeUtf8 . toStrict $ responseBody f

step3 :: IO ()
step3 = do
  -- 1. List cabal files
  -- 2. Parse cabal files
  -- 3. Insert Packages
  -- 4. Insert Deps
  --
  -- 1
  f <- filter (isSuffixOf ".cabal") <$> listDirectory cabalFilesDir
  -- 2
  pkgT <- mapM (readFile . (cabalFilesDir ++ )) f
  let pkgs = getSuccParse $ parseCabalFile <$> pkgT
  -- 3
  c <- connectSqlite3 dbFilePath
  putStrLn "[+] Saving packages to DB..."
  savePackages c pkgs
  commit c
  putStrLn "[+] Done."
  -- 4
  putStrLn "[+] Saving dependancies to DB..."
  foldr (foldInsertDep c (length pkgs)) (return 1) pkgs
  commit c
  putStrLn "[+] Done."
  disconnect c
  return ()

foldInsertDep :: Connection -> Int -> Package -> IO Int -> IO Int
foldInsertDep c totalDeps pkg step = do 
  step <- step
  savePackageDeps c pkg
  logProgress "----" ("["++ show step ++ "/" ++ show totalDeps ++ "] " ++ show pkg)
  return $ step + 1
