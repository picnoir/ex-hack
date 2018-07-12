{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExHack.CabalSpec (spec) where

import Data.FileEmbed
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Set (fromList)
import Test.Hspec

import ExHack.Cabal.CabalParser
import ExHack.Types

spec :: Spec
spec = describe "parseCabalFile" $
          it "should parse a file" $ do
            let 
              cf  = decodeUtf8 $(embedFile "test/fixtures/cabal-fixture.cabal")
              res = parseCabalFile $ UnparsedPackage ("", cf, "")
            case runParseResult res of
                (_, Left err) -> fail $ show err
                (_, Right res) -> res `shouldBe` Package (PackageIdentifier "ad" $ mkVersion [4, 3, 5]) (fromList ["array", "base", "comonad", "containers", "data-reify", "erf", "free", "nats", "reflection", "semigroups", "transformers", "directory", "doctest", "filepath", "criterion"]) cf "" ""
