{-# LANGUAGE TemplateHaskell #-}
module ExHack.Hackage.IntegrationHackageSpec (spec) where

import Data.FileEmbed (embedFile)
import Test.Hspec (Spec, describe, it, shouldBe)

import ExHack.Hackage.Hackage (unpackHackageTarball)



spec :: Spec
spec = describe "hackage" $
        it "should extract the content of a tarball" $ do
          r <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r `shouldBe` "./test/integration/workdir/timeit-1.0.0.0/"
