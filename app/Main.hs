{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO           (readFile)
import           Prelude                hiding (readFile)


import           ExHack.Data.Db         (mkHandle)
import           ExHack.ProcessingSteps (dlAssets, genGraphDep, generateDb,
                                         parseStackage)
import           ExHack.Types           (CabalFilesDir (..), Config (..),
                                         DatabaseStatus (..), StackageFile (..),
                                         TarballsDir (..), WorkDir (..),
                                         logInfoTitle, runStep)

type PreCondition = IO Bool

initConf :: IO (Config 'New)
initConf = do
    stackageYaml <- readFile "./data/lts-10.5.yaml"
    pure $ Config (mkHandle "./data/data.db") (StackageFile stackageYaml) 
                  (TarballsDir "/home/minoulefou/exhst/tb/") 
                  (CabalFilesDir "/home/minoulefou/exhst/cabal")
                  (WorkDir "/home/minoulefou/exhst/wd")

main :: IO ()
main = do
    logInfoTitle "[+] STEP 0: Initializing database"
    c <- initConf
    dbInit <- runStep generateDb c
    let ci = c {_dbHandle= dbInit} :: Config 'Initialized
    logInfoTitle "[+] STEP 1: Parsing stackage LTS-10.5"
    descs <- runStep parseStackage ci
    logInfoTitle "[+] STEP 2: Downloading hackage files (cabal builds + tarballs)"
    runStep (dlAssets descs) ci 
    logInfoTitle "[+] STEP 3: Generating dependancy graph"
    pgks <- runStep (genGraphDep descs) ci
    pure ()
