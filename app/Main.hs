module Main where

import Control.Monad.State
import qualified Data.Text as T

import Stackage.StackageTypes

class Loggable a where
    log :: a -> Text

class ComputationStep where
    compute :: IO ()

data Computations = Computations {
    steps :: [ComputationStep],
    progress :: Int,
    current :: ComputationStep
}

instance Loggable Computations where
    log (Computations cSteps p c) = T.concat ["[Step ", p, "/", length cSteps, "]", log c]

data CabalFiles = CabalFiles {
    urls :: [Text]
}


main :: IO ()
main = do
  stackageYaml <- undefined
  let packages = getHackageCabalUrl <$> parseStackageYaml stackageYaml
  putStrLn packages
  
