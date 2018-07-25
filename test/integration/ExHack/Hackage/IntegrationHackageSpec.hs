{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ExHack.Hackage.IntegrationHackageSpec (spec) where

import Data.List  (isSuffixOf)
import Data.Maybe (isNothing, fromJust)
import Data.FileEmbed (embedFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import System.Directory (makeAbsolute)

import ExHack.Cabal.CabalParser(parseCabalFile, getSuccParse)
import ExHack.Config (StackConfig(..))
import ExHack.Hackage.Hackage (unpackHackageTarball, getTarballDesc,
                              getPackageExports, PackageExports(..))
import ExHack.Stackage.Stack(build)

config :: StackConfig
config = StackConfig {_nbJobs=4, _workDir=".stack-work/", _stackRoot="/home/ninjatrappeur/exhst", _stackBin=Just "/nix/store/4s5l0p1s906prwsapm2g083cp5z4haxl-stack-1.6.5/bin/stack", _tarballsDir="/home/ninjatrappeur/exhst/tb/", _gccBin = Just "/nix/store/gqg2vrcq7krqi9rrl6pphvsg81sb8pjw-gcc-wrapper-7.3.0/bin/gcc"}

absWorkdir :: IO FilePath
absWorkdir = makeAbsolute "./test/integration/workdir"

spec :: Spec
spec = describe "hackage" $ do
        it "should extract the content of a tarball" $ do
          r <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r `shouldSatisfy` isSuffixOf "test/integration/workdir/timeit-1.0.0.0/"
        it "should build a tarball" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r <- build config tbp
          r `shouldSatisfy` isNothing
        it "should retrieve timeIt exports" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          _ <- build config tbp
          tbdm <- fromJust <$> getTarballDesc tbp
          let p = head $ getSuccParse [parseCabalFile tbdm]
          exports <- getPackageExports p
          exports `shouldBe` PackageExports [("System.TimeIt", ["timeIt", "timeItT"])]
        it "should retrieve text exports" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/text-1.2.3.0.tar.gz")
          _ <- build config tbp
          putStrLn $ "Gougere " <> tbp
          tbdm <- fromJust <$> getTarballDesc tbp
          let p = head $ getSuccParse [parseCabalFile tbdm]
          exports <- getPackageExports p
          exports `shouldBe` PackageExports []
