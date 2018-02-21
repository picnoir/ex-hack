{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Control.Monad.State
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text as T

import Stackage.StackageTypes
import Stackage.StackageParser

data Computations =  Computations {
    steps :: [IO()]
}

data CabalFiles = CabalFiles {
    urls :: [Text]
}


main :: IO ()
main = do
  stackageYaml <- readFile "./data/lts-10.5.yaml"  
  let packages = getHackageCabalUrl <$> parseStackageYaml stackageYaml
  print packages
  
