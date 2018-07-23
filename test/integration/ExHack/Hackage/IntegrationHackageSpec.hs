{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ExHack.Hackage.IntegrationHackageSpec (spec) where

import Data.Maybe (isNothing, fromJust)
import Data.FileEmbed (embedFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import ExHack.Cabal.CabalParser(parseCabalFile, getSuccParse)
import ExHack.Config (StackConfig(..))
import ExHack.Ghc (getModExports)
import ExHack.Hackage.Hackage (unpackHackageTarball, getTarballDesc,
                              loadExposedModules)
import ExHack.Stackage.Stack(build)

config :: StackConfig
config = StackConfig {_nbJobs=4, _workDir=".stack-work/", _stackRoot="/home/ninjatrappeur/exhst", _stackBin=Just "/nix/store/4s5l0p1s906prwsapm2g083cp5z4haxl-stack-1.6.5/bin/stack", _tarballsDir="/home/ninjatrappeur/exhst/tb/"}

spec :: Spec
spec = describe "hackage" $ do
        it "should extract the content of a tarball" $ do
          r <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r `shouldBe` "./test/integration/workdir/timeit-1.0.0.0/"
        it "should build a tarball" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r <- build config tbp
          r `shouldSatisfy` isNothing
        it "should retrieve a package exports" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          _ <- build config tbp
          tbdm <- fromJust <$> getTarballDesc tbp
          let p = head $ getSuccParse [parseCabalFile tbdm]
          mods <- loadExposedModules p
          let exports = getModExports <$> mods
          exports `shouldBe` []
          -- 1: Parse cabal file.
          -- 2: Load file in GHC
          -- 3: Get a desugared modules.
          -- 4: Get package exports. -}
