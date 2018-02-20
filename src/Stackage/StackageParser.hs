module Stackage.StackageParser (
  parseStackageYaml
) where

import Data.Yaml (decode)
import Data.Text  (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E

import Stackage.StackageTypes (Packages(..))

parseStackageYaml :: Text -> Maybe Packages
parseStackageYaml = decode . E.encodeUtf8

getHackageCabalUrl :: Packages -> [Text]
getHackageCabalUrl = foldlWithKey getCabal ""
  where getCabal xs k e = T.concat ["https://hackage.haskell.org/package/", 
                                    packName
                                    "-",
                                    ppVersion e,
                                    "/",
                                    packName,
                                    ".cabal"]
          where
            !packName = T.pack $ unPackageName k
