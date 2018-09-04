{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Prelude hiding (readFile)
import Data.Text.IO (readFile)


import ExHack.Types (DatabaseStatus(..), Config(..), StackageFile(..), 
                     TarballsDir(..), CabalFilesDir(..),
                     WorkDir(..), runStep, logTitle)
import ExHack.Data.Db (mkHandle)

import ExHack.ProcessingSteps (generateDb, parseStackage,
                               dlAssets, genGraphDep)

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
    logTitle "[+] STEP 0: Initializing database"
    c <- initConf
    dbInit <- runStep generateDb c
    let ci = c {_dbHandle= dbInit} :: Config 'Initialized
    logTitle "[+] STEP 1: Parsing stackage LTS-10.5"
    descs <- runStep parseStackage ci
    logTitle "[+] STEP 2: Downloading hackage files (cabal builds + tarballs)"
    runStep (dlAssets descs) ci 
    logTitle "[+] STEP 3: Generating dependancy graph"
    pgks <- runStep (genGraphDep descs) ci
    pure ()
