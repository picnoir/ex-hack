{-# LANGUAGE OverloadedStrings #-}
module ExHack.Stack.IntegrationStackSpec (spec) where

import Data.Maybe (isNothing)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

import qualified ExHack.Stackage.Stack as Stack (setup)
import ExHack.Config (StackConfig(..))

config :: StackConfig
config = StackConfig {_nbJobs=4, _workDir=".stack-work/", _stackRoot="/home/ninjatrappeur/exhst", _stackBin=Just "/nix/store/4s5l0p1s906prwsapm2g083cp5z4haxl-stack-1.6.5/bin/stack", _tarballsDir="/home/ninjatrappeur/exhst/tb/", _gccBin = Just "/nix/store/gqg2vrcq7krqi9rrl6pphvsg81sb8pjw-gcc-wrapper-7.3.0/bin/gcc"}

spec :: Spec
spec = describe "stack" $ 
         it "should bootstrap a GHC install" $ do
           r <- Stack.setup config
           r `shouldSatisfy` isNothing
