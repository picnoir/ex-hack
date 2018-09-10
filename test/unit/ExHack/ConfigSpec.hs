{-# LANGUAGE OverloadedStrings #-}
module ExHack.ConfigSpec (
    spec 
) where

import           Test.Hspec   (Spec, describe, it, shouldBe, expectationFailure)

import           ExHack.Types (parseConfig, Config(..), StackageFile(..), newDatabaseHandle, TarballsDir(..), CabalFilesDir(..), WorkDir(..))


spec :: Spec
spec = describe "parseConfig" $
          it "should parse a valid config file" $ do
            c <- parseConfig "test/unit/fixtures/config-fixture.yaml" 
            either (expectationFailure . show) 
                   (\conf -> conf `shouldBe` 
                        Config (newDatabaseHandle "/path/to/db.db") 
                               (StackageFile "/path/to/stackage.yaml") 
                               (TarballsDir "/path/to/tarball/dir/") 
                               (CabalFilesDir "/path/to/cabal/dir/") 
                               (WorkDir "/path/to/work/dir")) 
                   c
