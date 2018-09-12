{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO           (readFile)
import           Prelude                hiding (readFile)


import           ExHack.Data.Db         (mkHandle)
import           ExHack.ProcessingSteps (dlAssets, genGraphDep, generateDb,
                                         indexSymbols, parseStackage,
                                         retrievePkgsExports)
import           ExHack.Types           (CabalFilesDir (..), Config (..),
                                         DatabaseStatus (..), StackageFile (..),
                                         TarballsDir (..), WorkDir (..),
                                         runStep)

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
    c <- initConf
    dbInit <- runStep generateDb c
    let ci = c {_dbHandle= dbInit} :: Config 'Initialized
    descs <- runStep parseStackage ci
    runStep (dlAssets descs) ci 
    (dbGraph,pkgs) <- runStep (genGraphDep descs) ci
    let cg = ci {_dbHandle=dbGraph} :: Config 'DepsGraph
    (dbExprt,pe) <- runStep (retrievePkgsExports pkgs) cg
    let ce = cg {_dbHandle=dbExprt} :: Config 'PkgExports
    runStep (indexSymbols pe) ce
    pure ()
