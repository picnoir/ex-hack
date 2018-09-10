module ExHack.ModulePathSpec (spec) where

import           Test.Hspec

import           ExHack.ModulePaths (toModFilePath)
import           ExHack.Types       (ComponentRoot (..), PackageFilePath (..),
                                     fromComponents)

spec :: Spec
spec = describe "toModFilePath" $ do
          it "should create a regular path" $
            toModFilePath (PackageFilePath "/home/mar/Code/text/") 
                          (ComponentRoot "src") 
                          (fromComponents ["Data", "Text", "Test"])
            `shouldBe` "/home/mar/Code/text/src/Data/Text/Test.hs"
          it "be somehow robust and handle missing /" $
            toModFilePath (PackageFilePath "/home/mar/Code/text") 
                          (ComponentRoot "src") 
                          (fromComponents ["Data", "Text", "Test"])
            `shouldBe` "/home/mar/Code/text/src/Data/Text/Test.hs"
          it "be somehow robust and handle unecessary trailing /" $
            toModFilePath (PackageFilePath "/home/mar/Code/text/") 
                          (ComponentRoot "src/") 
                          (fromComponents ["Data", "Text", "Test"])
            `shouldBe` "/home/mar/Code/text/src/Data/Text/Test.hs"
