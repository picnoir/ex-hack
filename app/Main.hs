{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS (writeFile)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Database.Selda (SeldaM)
import Database.Selda.SQLite (withSQLite)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory (doesFileExist, removeFile, listDirectory)

import ExHack.Cabal.CabalParser (parseCabalFile, getSuccParse)
import ExHack.Stackage.StackageParser
import ExHack.Types (Package(..))
import ExHack.Data.Db (initDb, savePackages,
                       savePackageDeps)

import Config (cabalFilesDir, tarballsDir, dbFilePath)
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
step0 = withSQLite dbFilePath initDb

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
  _ <- foldr (dlFoldCabalFiles m (length packages)) (return 1) packages
  return ()

shouldDlCabalFiles :: PreCondition
shouldDlCabalFiles = do
  c <- listDirectory cabalFilesDir
  t <- listDirectory tarballsDir
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
  BS.writeFile (cabalFilesDir ++ T.unpack name ++ ".cabal") $ responseBody f 
  f <- httpLbs (parseRequest_ $ T.unpack tarballUrl) m
  BS.writeFile (tarballsDir ++ T.unpack name ++ ".tar.gz") $ responseBody f 
  return ()

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
  let pkgs = getSuccParse $ (parseCabalFile tarballsDir) <$> pkgT
  -- 3
  withSQLite dbFilePath $ do
    liftIO $ putStrLn "[+] Saving packages to DB..."
    savePackages pkgs
    liftIO $ putStrLn "[+] Done."
    -- 4
    liftIO $ putStrLn "[+] Saving dependancies to DB..."
    _ <- foldr (foldInsertDep (length pkgs)) (return 1) pkgs
    liftIO $ putStrLn "[+] Done."
  return ()

foldInsertDep :: Int -> Package -> SeldaM Int -> SeldaM Int
foldInsertDep totalDeps pkg step = do 
  step <- step
  savePackageDeps pkg
  liftIO $ logProgress "----" ("["++ show step ++ "/" ++ show totalDeps ++ "] " ++ show pkg)
  return $ step + 1
