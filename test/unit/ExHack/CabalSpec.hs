{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExHack.CabalSpec (spec) where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)
import Data.Set (fromList)
import Test.Hspec (Spec, describe, it, shouldBe)

import ExHack.Cabal.CabalParser (parseCabalFile, runParseResult)
import ExHack.Types (PackageIdentifier(..), UnparsedPackage(..), Package(..),
                    mkVersion, ModuleName(..), fromComponents)

spec :: Spec
spec = describe "parseCabalFile" $
          it "should parse a file" $ do
            let 
              cf  = decodeUtf8 $(embedFile "test/unit/fixtures/cabal-fixture.cabal")
              res = parseCabalFile $ UnparsedPackage ("", cf, "")
            case runParseResult res of
                (_, Left err) -> fail $ show err
                (_, Right r) -> r `shouldBe` Package (PackageIdentifier "ad" $ mkVersion [4, 3, 5]) (fromList ["array", "base", "comonad", "containers", "data-reify", "erf", "free", "nats", "reflection", "semigroups", "transformers", "directory", "doctest", "filepath", "criterion"]) cf "" "" exposedMods


exposedMods :: Maybe [ModuleName]
exposedMods = Just [fromComponents ["Numeric","AD"],fromComponents ["Numeric","AD","Halley"],fromComponents ["Numeric","AD","Internal","Dense"],fromComponents ["Numeric","AD","Internal","Forward"],fromComponents ["Numeric","AD","Internal","Forward","Double"],fromComponents ["Numeric","AD","Internal","Identity"],fromComponents ["Numeric","AD","Internal","Kahn"],fromComponents ["Numeric","AD","Internal","On"],fromComponents ["Numeric","AD","Internal","Or"],fromComponents ["Numeric","AD","Internal","Reverse"],fromComponents ["Numeric","AD","Internal","Sparse"],fromComponents ["Numeric","AD","Internal","Tower"],fromComponents ["Numeric","AD","Internal","Type"],fromComponents ["Numeric","AD","Jacobian"],fromComponents ["Numeric","AD","Jet"],fromComponents ["Numeric","AD","Mode"],fromComponents ["Numeric","AD","Mode","Forward"],fromComponents ["Numeric","AD","Mode","Forward","Double"],fromComponents ["Numeric","AD","Mode","Kahn"],fromComponents ["Numeric","AD","Mode","Reverse"],fromComponents ["Numeric","AD","Mode","Sparse"],fromComponents ["Numeric","AD","Mode","Tower"],fromComponents ["Numeric","AD","Newton"],fromComponents ["Numeric","AD","Newton","Double"],fromComponents ["Numeric","AD","Rank1","Forward"],fromComponents ["Numeric","AD","Rank1","Forward","Double"],fromComponents ["Numeric","AD","Rank1","Halley"],fromComponents ["Numeric","AD","Rank1","Kahn"],fromComponents ["Numeric","AD","Rank1","Newton"],fromComponents ["Numeric","AD","Rank1","Newton","Double"],fromComponents ["Numeric","AD","Rank1","Sparse"],fromComponents ["Numeric","AD","Rank1","Tower"]]
