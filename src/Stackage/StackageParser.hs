module Stackage.StackageParser (
  parseStackageYaml
) where

import Data.Yaml (decode)
import Data.Text  (Text)
import Data.Text.Encoding as E

import Stackage.StackageTypes (Packages(..))

parseStackageYaml :: Text -> Maybe Packages
parseStackageYaml = decode . E.encodeUtf8
