{-# LANGUAGE TemplateHaskell #-}
module ExHack.Hackage.IntegrationHackageSpec (spec) where

import Data.Maybe (isNothing)
import Data.FileEmbed (embedFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import ExHack.Config (config)
import ExHack.Hackage.Hackage (unpackHackageTarball)
import ExHack.Stackage.Stack(build)



spec :: Spec
spec = describe "hackage" $ do
        it "should extract the content of a tarball" $ do
          r <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r `shouldBe` "./test/integration/workdir/timeit-1.0.0.0/"
        it "should build a tarball" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r <- build config tbp
          r `shouldSatisfy` isNothing
