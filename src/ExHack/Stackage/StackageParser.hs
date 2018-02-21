{-# LANGUAGE OverloadedStrings #-}
module ExHack.Stackage.StackageParser (
  parseStackageYaml,
  getHackageCabalUrl
) where

import Data.Map   (foldlWithKey)
import Data.Text  (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml (decode)
import qualified Distribution.Types.PackageName as C

import ExHack.Stackage.StackageTypes (Packages(..), PackagePlan(..))

parseStackageYaml :: Text -> Maybe Packages
parseStackageYaml = decode . E.encodeUtf8

getHackageCabalUrl :: Packages -> [(Text,Text)]
getHackageCabalUrl (Packages m) = foldlWithKey getCabal [] m
  where getCabal xs k e = (packName,T.concat ["https://hackage.haskell.org/package/", 
                                    packName,
                                    "-",
                                    ppVersion e,
                                    "/",
                                    packName,
                                    ".cabal"]) : xs
          where
            !packName = T.pack $ C.unPackageName k
