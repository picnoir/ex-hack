{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO           (readFile)
import           Prelude                hiding (readFile)
import           System.Directory       (XdgDirectory (XdgData),
                                         createDirectoryIfMissing,
                                         getXdgDirectory, listDirectory,
                                         removeDirectoryRecursive)
import           System.FilePath        ((</>))


import           ExHack.Data.Db         (depGraphAlreadyHere, mkHandle)
import           ExHack.ProcessingSteps (dlAssets, genGraphDep, generateDb,
                                         indexSymbols, parseStackage,
                                         retrievePkgsExports, saveGraphDep)
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
    shouldBypassAssetsDl (_tarballsDir c) (_cabalFilesDir c) $ runStep (dlAssets descs) ci 
    pkgs <- runStep (genGraphDep descs) ci
    dbGraph <- shouldBypassGraphDepsGen dbInit $ runStep (saveGraphDep pkgs) ci
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

shouldBypassDBInit :: FilePath -> IO (DatabaseHandle 'Initialized) -> IO (DatabaseHandle 'Initialized)
shouldBypassDBInit dbfp =
    promptUser "Do you wanna skip the database init?" 
               (pure $ newDatabaseHandle dbfp)

shouldBypassAssetsDl :: TarballsDir -> CabalFilesDir -> IO () -> IO ()
shouldBypassAssetsDl (TarballsDir fpt) (CabalFilesDir fpc) s = do
    dirT <- listDirectory fpt
    dirC <- listDirectory fpc
    if null (dirT <> dirC) 
        then s 
        else promptUser "Your assets folder is not empty. Do you want to empty it and re-download everything?"
                        (removeDirectoryRecursive fpt >> removeDirectoryRecursive fpc >> s)
                        (pure ())

shouldBypassGraphDepsGen :: DatabaseHandle 'New -> IO (DatabaseHandle 'DepsGraph) -> IO (DatabaseHandle 'DepsGraph)
shouldBypassGraphDepsGen h =
    promptUser "Do you wanna skip the dependancy graph generation?"
               (pure $ depGraphAlreadyHere h)

promptUser :: String -> IO a -> IO a -> IO a
promptUser str true false = do
    putStrLn (str <> " [y/N]")
    res <- getLine
    if head (words res) == "y"
        then true
        else false
