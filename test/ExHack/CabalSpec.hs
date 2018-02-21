{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExHack.CabalSpec (spec) where

import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import Test.Hspec

import ExHack.Cabal.CabalParser
import ExHack.Types

spec :: Spec
spec = describe "parseCabalFile" $
          it "should parse a file" $ do
            let res = parseCabalFile (decodeUtf8 $(embedFile "test/fixtures/cabal-fixture.cabal"))
            case res of
                (ParseFailed err) -> fail $ show err
                (ParseOk _ res) -> res `shouldBe` Package (PackageIdentifier "ad" $ mkVersion [4, 3, 5]) ["array", "base", "comonad", "containers", "data-reify", "erf", "free", "nats", "reflection", "semigroups", "transformers"]
