module ExHack.Cabal.CabalParser (
  parseCabalFile,
  ParseResult(..)
) where

import Data.Text (Text, unpack)
import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Types.Dependency

import ExHack.Types

parseCabalFile :: Text -> ParseResult Package
parseCabalFile str = Package <$> n <*> dn
    where
      packageDesc = packageDescription <$> parseGenericPackageDescription (unpack str)
      n = package <$> packageDesc
      d = (setupDepends . fromJust . setupBuildInfo ) <$> packageDesc
      dn = (fmap . fmap) depPkgName d
