{-# LANGUAGE OverloadedStrings #-}
module ExHack.Stackage.StackageParser (
  parseStackageYaml,
  getHackageUrls
) where

import           Data.Map                       (foldlWithKey)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             as E
import           Data.Yaml                      (decodeEither')
import qualified Distribution.Types.PackageName as C

import           ExHack.Stackage.StackageTypes  (PackagePlan (..),
                                                 Packages (..))
import           ExHack.Types                   (PackageDlDesc (..))

parseStackageYaml :: Text -> Maybe Packages
parseStackageYaml t = case decodeEither' $ E.encodeUtf8 t of
                      Right p -> Just p
                      _ -> Nothing

getHackageUrls :: Packages -> [PackageDlDesc]
getHackageUrls (Packages m) = foldlWithKey getCabal [] m
  where getCabal xs k e = PackageDlDesc(packName,
                           mconcat [base,packName,".cabal"],
                           mconcat [base,packName, "-",ppVersion e, ".tar.gz"]) : xs
          where
            !packName = T.pack $ C.unPackageName k
            !base = mconcat ["https://hackage.haskell.org/package/", 
                                packName, "-", ppVersion e, "/"]
