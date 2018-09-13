{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (when)
import           Data.Text.IO           (readFile)
import           Prelude                hiding (readFile)
import           System.Directory       (XdgDirectory (XdgData),
                                         createDirectoryIfMissing,
                                         getXdgDirectory, listDirectory)
import           System.FilePath        ((</>))


import           ExHack.Data.Db         (mkHandle)
import           ExHack.ProcessingSteps (dlAssets, genGraphDep, generateDb,
                                         indexSymbols, parseStackage,
                                         retrievePkgsExports)
import           ExHack.Types           (CabalFilesDir (..), Config (..),
                                         DatabaseHandle, DatabaseStatus (..),
                                         StackageFile (..), TarballsDir (..),
                                         WorkDir (..), newDatabaseHandle,
                                         runStep)

main :: IO ()
main = do
    c <- initConf
    dbInit <- shouldBypassDBInit (_dbHandle c) $ runStep generateDb c
    let ci = c {_dbHandle= dbInit} :: Config 'Initialized
    descs <- runStep parseStackage ci
    shouldBypassAssetsDl (_tarballsDir c) $ runStep (dlAssets descs) ci 
    (dbGraph,pkgs) <- runStep (genGraphDep descs) ci
    let cg = ci {_dbHandle=dbGraph} :: Config 'DepsGraph
    (dbExprt,pe) <- runStep (retrievePkgsExports pkgs) cg
    let ce = cg {_dbHandle=dbExprt} :: Config 'PkgExports
    runStep (indexSymbols pe) ce
    pure ()

initConf :: IO (Config 'New)
initConf = do
    dataDir <- getXdgDirectory XdgData "ex-hack"
    let tarballs = dataDir </> "tarballs"
        cabal    = dataDir </> "cabal-files"
        workdir  = dataDir </> "workdir"
    createDirectoryIfMissing True tarballs
    createDirectoryIfMissing True cabal
    createDirectoryIfMissing True workdir
    stackageYaml <- readFile "./data/lts-10.5.yaml"
    pure $ Config (mkHandle $ dataDir </> "database.sqlite")
                  (StackageFile stackageYaml) 
                  (TarballsDir tarballs) 
                  (CabalFilesDir cabal)
                  (WorkDir workdir)

shouldBypassAssetsDl :: TarballsDir -> IO () -> IO ()
shouldBypassAssetsDl (TarballsDir fp) s = do
    empty <- null <$> listDirectory fp
    when empty $ do
        r <- promptUser "Your assets folder is not empty. Do you want to empty it and re-download everything?"
        if r
            then s
            else pure ()

shouldBypassDBInit :: FilePath -> IO (DatabaseHandle 'Initialized) -> IO (DatabaseHandle 'Initialized)
shouldBypassDBInit dbfp s = do
    r <- promptUser "Do you wanna skip the database init?" 
    if r
        then pure $ newDatabaseHandle dbfp
        else s

promptUser :: String -> IO Bool
promptUser str = do
    putStrLn (str <> " [y/N]")
    res <- getLine
    if head (words res) == "y"
        then return True
        else return False
