{-# LANGUAGE OverloadedStrings #-}
module ExHack.Stackage.StackageParser (
  parseStackageYaml,
  getHackageUrls
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

getHackageUrls :: Packages -> [(Text,Text,Text)]
getHackageUrls (Packages m) = foldlWithKey getCabal [] m
  where getCabal xs k e = (packName,
                            mconcat [base,
                                    packName,
                                    ".cabal"],
                            mconcat [base, packName, "-", ppVersion e, ".tar.gz"]) : xs
          where
            !packName = T.pack $ C.unPackageName k
            !base = mconcat ["https://hackage.haskell.org/package/", 
                                packName, "-", ppVersion e, "/"]
