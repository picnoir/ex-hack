{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ExHack.GhcSpec where 


import           Test.Hspec               (Spec, before, describe, it, shouldBe,
                                           shouldSatisfy)
import HieBin
import HieUtils 
import Data.FileEmbed (embedFile)
import qualified Data.Set as Set
import           UniqSupply
import           NameCache                      ( initNameCache )
import ExHack.Types
import ExHack.Ghc (collectSymbols)
import           GHC                            (GenLocated (..), SrcSpan)
import Data.List (nub, sort)

spec :: Spec
spec = describe "ghc" $ do 
    it "should retrieve references from hie file" $ do 
        uniq_supply <- mkSplitUniqSupply 'z'
        let nc = initNameCache uniq_supply []
        (hf, _) <- readHieFile nc "./test/unit/fixtures/simple.hie"
        let symbols = collectSymbols somePackage hf
            symbolNames = nub $ sort $ map getSymName symbols
        -- This is a little hacky and should probably be replaced
        show symbolNames `shouldBe` "[SymName \"GHC.Base.$\",SymName \"GHC.Base.return\",SymName \"GHC.Float.pi\",SymName \"GHC.Num.*\",SymName \"GHC.Tuple.()\",SymName \"GHC.Types.Double\",SymName \"GHC.Types.IO\",SymName \"Main.areaOfCircle\",SymName \"Main.main\",SymName \"Main.myPi\",SymName \"System.Environment.lookupEnv\",SymName \"System.IO.print\"]"

        where
            somePackage = Package
             (error "get a better package identifier")
                (Set.fromList []) "" (PackageFilePath "" ) Nothing Nothing []
getSymName :: LocatedSym -> SymName 
getSymName (LocatedSym (_, _,  (L l e))) = e 
