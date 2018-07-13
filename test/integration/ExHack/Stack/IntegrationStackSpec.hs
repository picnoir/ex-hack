module ExHack.Stack.IntegrationStackSpec (spec) where

import Data.Maybe (isNothing)
import Test.Hspec (Spec, describe, it, shouldSatisfy, shouldBe)

import qualified ExHack.Stackage.Stack as Stack (setup)
import ExHack.Config (config)

spec :: Spec
spec = describe "stack" $ 
         it "should bootstrap a GHC install" $ do
           r <- Stack.setup config
           r `shouldSatisfy` isNothing
