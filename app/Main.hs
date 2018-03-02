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

import Config (cabalFilesDir, dbFilePath)
import Cli (step, promptUser, PreCondition)
import Log (logProgress, logTitle)

main :: IO ()
main = do
  step "STEP 0: Creating DB" isDb step0
  logTitle "STEP 1: Parsing stackage LTS-10.5"
  stackageYaml <- readFile "./data/lts-10.5.yaml"  
  let packages = fromJust $ parseStackageYaml stackageYaml
  step "STEP 2: Downloading cabal files" shouldDlCabalFiles (step2 $ getHackageCabalUrl packages)
  step "STEP 3: Generating dependancy graph" (return True) step3

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
      del <- promptUser "A database is already here, delete it?"
      if del
        then putStrLn "Deleting..." >> removeFile dbFilePath >> return True
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

shouldDlCabalFiles :: PreCondition
shouldDlCabalFiles = do
  f <- getDirectoryContents cabalFilesDir
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
  writeFile (cabalFilesDir ++ T.unpack name ++ ".cabal") $ getResponse f 
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
  putStrLn "Saving packages to DB..."
  savePackages c pkgs
  putStrLn "Done."
  putStrLn "Saving dependancies to DB..."
  foldr (foldInsertDep c (length pkgs)) (return 1) pkgs
  putStrLn "Done."
  commit c
  disconnect c
  return ()

foldInsertDep :: Connection -> Int -> Package -> IO Int -> IO Int
foldInsertDep c totalDeps pkg step = do 
  step <- step
  savePackageDeps c pkg
  logProgress "----" ("["++ show step ++ "/" ++ show totalDeps ++ "] " ++ show pkg)
  return $ step + 1
