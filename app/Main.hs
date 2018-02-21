{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad.State
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import ExHack.Stackage.StackageTypes
import ExHack.Stackage.StackageParser

main :: IO ()
main = do
  -- Retrieving cabal URLs
  logTitle "STEP 1: Parsing stackage LTS-10.5"
  stackageYaml <- readFile "./data/lts-10.5.yaml"  
  let packages = fromJust (getHackageCabalUrl <$> parseStackageYaml stackageYaml)

  -- Downloading cabal files
  logTitle "STEP 2: Downloading cabal files."
  let settings = managerSetProxy
        (proxyEnvironment Nothing)
        tlsManagerSettings
  m <- newManager settings
  foldr (sequenceLog m (length packages)) (return 1) packages
  return ()

sequenceLog :: Manager -> Int -> (Text,Text) -> IO Int -> IO Int
sequenceLog man totalSteps pack step = do 
  step <- step
  downloadCabalFile man pack
  logProgress "----" ("["++ show step ++ "/" ++ show totalSteps ++ "] " ++ T.unpack (fst pack))
  return $ step + 1

logProgress :: String -> String -> IO ()
logProgress prefix log = putStrLn (prefix ++ log)

logTitle :: String -> IO ()
logTitle txt = line >> putStrLn txt >> line
  where line = putStrLn (replicate (length txt) '=')

downloadCabalFile :: Manager -> (Text,Text) -> IO ()
downloadCabalFile m (name, url) = do
  f <- httpLbs (parseRequest_ $ T.unpack url) m 
  writeFile ("cabal/" ++ T.unpack name ++ ".cabal") $ getResponse f 
  return ()
  where
    getResponse f = E.decodeUtf8 . toStrict $ responseBody f
