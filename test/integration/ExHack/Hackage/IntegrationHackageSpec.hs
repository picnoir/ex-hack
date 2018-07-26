{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module ExHack.Hackage.IntegrationHackageSpec (spec) where

import Data.List  (isSuffixOf)
import Data.Maybe (isNothing, fromJust)
import Data.FileEmbed (embedFile)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import System.Directory (makeAbsolute)

import ExHack.Cabal.CabalParser(parseCabalFile, getSuccParse)
import ExHack.Cabal.Cabal(build, installDeps)
import ExHack.Hackage.Hackage (unpackHackageTarball, getTarballDesc,
                              getPackageExports, PackageExports(..))

absWorkdir :: IO FilePath
absWorkdir = makeAbsolute "./test/integration/workdir"

spec :: Spec
spec = describe "hackage" $ do
        it "should extract the content of a tarball" $ do
          r <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          r `shouldSatisfy` isSuffixOf "test/integration/workdir/timeit-1.0.0.0/"
        it "should build a tarball" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          d <- installDeps
          d `shouldSatisfy` isNothing
          r <- build 
          r `shouldSatisfy` isNothing
        it "should retrieve timeIt exports" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/timeit.tar.gz")
          _ <- installDeps
          _ <- build
          tbdm <- fromJust <$> getTarballDesc tbp
          let p = head $ getSuccParse [parseCabalFile tbdm]
          exports <- getPackageExports p
          exports `shouldBe` PackageExports [("System.TimeIt", ["timeIt", "timeItT"])]
          {-
        it "should retrieve text exports" $ do
          tbp <- unpackHackageTarball "./test/integration/workdir/" $(embedFile "./test/integration/fixtures/text-1.2.3.0.tar.gz")
          _ <- build config tbp
          putStrLn $ "Gougere " <> tbp
          tbdm <- fromJust <$> getTarballDesc tbp
          let p = head $ getSuccParse [parseCabalFile tbdm]
          exports <- getPackageExports p
          exports `shouldBe` PackageExports [] -}
