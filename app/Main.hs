{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS (writeFile)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text as T
import Database.Selda (SeldaM)
import Database.Selda.SQLite (withSQLite)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory (doesFileExist, removeFile, listDirectory)

import ExHack.Cabal.CabalParser (parseCabalFile, getSuccParse)
import ExHack.Stackage.StackageParser
import ExHack.Types (Package(..), PackageDlDesc(..),
                     UnparsedPackage(..), packagedlDescName, packagedlDescName)
import ExHack.Data.Db (initDb, savePackages,
                       savePackageDeps)

import Config (cabalFilesDir, tarballsDir, dbFilePath,
               hoogleFilesDir)
import Cli (step, promptUser, PreCondition)
import Log (logProgress, logTitle)

main :: IO ()
main = do
  step "[+] STEP 0: Creating DB" isDb sGenerateDb 
  logTitle "[+] STEP 1: Parsing stackage LTS-10.5"
  stackageYaml <- readFile "./data/lts-10.5.yaml"  
  let packages = fromJust $ parseStackageYaml stackageYaml
      hackageUrl = getHackageUrls packages
  logTitle "[+] STEP 2: bootstrapping GHC"
  step "[+] STEP 2: Downloading hackage files (cabal builds + tarballs)" shouldDlCabalFiles $ sDlHack hackageUrl
  step "[+] STEP 3: Generating dependancy graph" (return True) (sGenDepGraph hackageUrl)

sGenerateDb :: IO ()
sGenerateDb = withSQLite dbFilePath initDb

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

sDlHack :: [ PackageDlDesc ] -> IO ()
sDlHack packages = do
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

dlFoldCabalFiles :: Manager -> Int -> PackageDlDesc -> IO Int -> IO Int
dlFoldCabalFiles man totalSteps p step = do 
  step <- step
  let pn = packagedlDescName p
  downloadHackageFiles man p
  logProgress "----" ("["++ show step ++ "/" ++ show totalSteps ++ "] " ++ T.unpack pn)
  return $ step + 1

downloadHackageFiles :: Manager -> PackageDlDesc -> IO ()
downloadHackageFiles m (PackageDlDesc (name, cabalUrl, tarballUrl, hoogleUrl)) = do
  f <- httpLbs (parseRequest_ $ T.unpack cabalUrl) m 
  BS.writeFile (cabalFilesDir ++ T.unpack name ++ ".cabal") $ responseBody f 
  f <- httpLbs (parseRequest_ $ T.unpack tarballUrl) m
  BS.writeFile (tarballsDir ++ T.unpack name ++ ".tar.gz") $ responseBody f 
  f <- httpLbs (parseRequest_ $ T.unpack hoogleUrl) m
  BS.writeFile (hoogleFilesDir ++ T.unpack name ++ ".txt") $ responseBody f 
  return ()

sGenDepGraph :: [PackageDlDesc] -> IO ()
sGenDepGraph pkgsDesc = do
  -- 1. Parse cabal files
  -- 2. Insert Packages
  -- 3. Insert Deps
  --
  -- 1
  putStrLn "[+] Parsing cabal files."
  pkgs <- readPkgsFiles `mapM` pkgsDesc
  let pkgs' = getSuccParse (parseCabalFile . UnparsedPackage <$> pkgs)
  -- 2
  withSQLite dbFilePath $ do
    liftIO $ putStrLn "[+] Saving packages to DB..."
    savePackages pkgs'
    liftIO $ putStrLn "[+] Done."
    -- 3
    liftIO $ putStrLn "[+] Saving dependancies to DB..."
    _ <- foldr (foldInsertDep (length pkgs)) (return 1) pkgs'
    liftIO $ putStrLn "[+] Done."
  return ()
    where
      readPkgsFiles :: PackageDlDesc -> IO (String, Text, Text)
      readPkgsFiles p = do
        let tp = tarballsDir <> T.unpack (packagedlDescName p) <> ".tar.gz"
        cf <- readFile $ cabalFilesDir <> T.unpack (packagedlDescName p) <> ".cabal"
        hf <- readFile $ hoogleFilesDir <> T.unpack (packagedlDescName p) <> ".txt"
        return (tp,cf,hf)

foldInsertDep :: Int -> Package -> SeldaM Int -> SeldaM Int
foldInsertDep totalDeps pkg step = do 
  step <- step
  savePackageDeps pkg
  liftIO $ logProgress "----" ("["++ show step ++ "/" ++ show totalDeps ++ "] " ++ show pkg)
  return $ step + 1
