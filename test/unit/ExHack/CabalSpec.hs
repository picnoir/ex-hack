{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ExHack.CabalSpec (spec) where

import           Data.FileEmbed           (embedFile)
import           Data.Maybe               (maybeToList)
import           Data.Set                 (fromList)
import           Data.Text.Encoding       (decodeUtf8)
import           Test.Hspec               (Spec, describe, it, shouldBe)

import           ExHack.Cabal.CabalParser (parseCabalFile, runParseResult)
import           ExHack.Types             (ComponentRoot (..), Package (..),
                                           PackageComponent (..),
                                           PackageIdentifier (..),
                                           TarballDesc (..), fromComponents,
                                           mkVersion)

spec :: Spec
spec = describe "parseCabalFile" $
          it "should parse a file" $ do
            let 
              cf  = decodeUtf8 $(embedFile "test/unit/fixtures/cabal-fixture.cabal")
              res = parseCabalFile $ TarballDesc ("", cf)
            case runParseResult res of
                (_, Left err) -> fail $ show err
                (_, Right r) -> r `shouldBe` Package (PackageIdentifier "ad" $ mkVersion [4, 3, 5]) (fromList ["array", "base", "comonad", "containers", "data-reify", "erf", "free", "nats", "reflection", "semigroups", "transformers", "directory", "doctest", "filepath", "criterion"]) cf "" exposedMods Nothing allMods


exposedMods :: Maybe PackageComponent 
exposedMods = Just $ PackageComponent [fromComponents ["Numeric","AD"],fromComponents ["Numeric","AD","Halley"],fromComponents ["Numeric","AD","Internal","Dense"],fromComponents ["Numeric","AD","Internal","Forward"],fromComponents ["Numeric","AD","Internal","Forward","Double"],fromComponents ["Numeric","AD","Internal","Identity"],fromComponents ["Numeric","AD","Internal","Kahn"],fromComponents ["Numeric","AD","Internal","On"],fromComponents ["Numeric","AD","Internal","Or"],fromComponents ["Numeric","AD","Internal","Reverse"],fromComponents ["Numeric","AD","Internal","Sparse"],fromComponents ["Numeric","AD","Internal","Tower"],fromComponents ["Numeric","AD","Internal","Type"],fromComponents ["Numeric","AD","Jacobian"],fromComponents ["Numeric","AD","Jet"],fromComponents ["Numeric","AD","Mode"],fromComponents ["Numeric","AD","Mode","Forward"],fromComponents ["Numeric","AD","Mode","Forward","Double"],fromComponents ["Numeric","AD","Mode","Kahn"],fromComponents ["Numeric","AD","Mode","Reverse"],fromComponents ["Numeric","AD","Mode","Sparse"],fromComponents ["Numeric","AD","Mode","Tower"],fromComponents ["Numeric","AD","Newton"],fromComponents ["Numeric","AD","Newton","Double"],fromComponents ["Numeric","AD","Rank1","Forward"],fromComponents ["Numeric","AD","Rank1","Forward","Double"],fromComponents ["Numeric","AD","Rank1","Halley"],fromComponents ["Numeric","AD","Rank1","Kahn"],fromComponents ["Numeric","AD","Rank1","Newton"],fromComponents ["Numeric","AD","Rank1","Newton","Double"],fromComponents ["Numeric","AD","Rank1","Sparse"],fromComponents ["Numeric","AD","Rank1","Tower"]] ["src"]

allLibMods :: Maybe PackageComponent 
allLibMods = Just $ PackageComponent [fromComponents ["Numeric","AD"],fromComponents ["Numeric","AD","Halley"],fromComponents ["Numeric","AD","Internal","Dense"],fromComponents ["Numeric","AD","Internal","Forward"],fromComponents ["Numeric","AD","Internal","Forward","Double"],fromComponents ["Numeric","AD","Internal","Identity"],fromComponents ["Numeric","AD","Internal","Kahn"],fromComponents ["Numeric","AD","Internal","On"],fromComponents ["Numeric","AD","Internal","Or"],fromComponents ["Numeric","AD","Internal","Reverse"],fromComponents ["Numeric","AD","Internal","Sparse"],fromComponents ["Numeric","AD","Internal","Tower"],fromComponents ["Numeric","AD","Internal","Type"],fromComponents ["Numeric","AD","Jacobian"],fromComponents ["Numeric","AD","Jet"],fromComponents ["Numeric","AD","Mode"],fromComponents ["Numeric","AD","Mode","Forward"],fromComponents ["Numeric","AD","Mode","Forward","Double"],fromComponents ["Numeric","AD","Mode","Kahn"],fromComponents ["Numeric","AD","Mode","Reverse"],fromComponents ["Numeric","AD","Mode","Sparse"],fromComponents ["Numeric","AD","Mode","Tower"],fromComponents ["Numeric","AD","Newton"],fromComponents ["Numeric","AD","Newton","Double"],fromComponents ["Numeric","AD","Rank1","Forward"],fromComponents ["Numeric","AD","Rank1","Forward","Double"],fromComponents ["Numeric","AD","Rank1","Halley"],fromComponents ["Numeric","AD","Rank1","Kahn"],fromComponents ["Numeric","AD","Rank1","Newton"],fromComponents ["Numeric","AD","Rank1","Newton","Double"],fromComponents ["Numeric","AD","Rank1","Sparse"],fromComponents ["Numeric","AD","Rank1","Tower"], fromComponents ["Numeric","AD","Internal","Combinators"]] ["src"]

testMod :: PackageComponent
testMod = PackageComponent {mods = [], roots = [ComponentRoot "tests"]}

benchMod :: PackageComponent
benchMod = PackageComponent {mods = [], roots = [ComponentRoot "bench"]}

allMods :: [PackageComponent]
allMods = testMod : benchMod : maybeToList allLibMods
