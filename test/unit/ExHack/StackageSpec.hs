{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module ExHack.StackageSpec (spec) where

import Data.FileEmbed
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Encoding (decodeUtf8)
import Test.Hspec (describe, it, Spec, shouldBe)

import ExHack.Stackage.StackageParser (parseStackageYaml)
import ExHack.Stackage.StackageTypes  (Packages(..), PackagePlan(..))

spec :: Spec
spec = describe "parseStackageYaml" $ 
          it "should parse a text" $
            parseStackageYaml (decodeUtf8 $(embedFile "test/unit/fixtures/stack-fixture.yaml")) `shouldBe` stackFixturePackages
 
stackFixturePackages :: Maybe Packages
stackFixturePackages = Just $ Packages m
  where
    m = Map.singleton "drawille" (PackagePlan "0.1.2.0" Nothing (Set.singleton "yamadapc"))
