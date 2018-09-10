{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ExHack.Stackage.StackageTypes (
  PackagePlan(..),
  Packages(..)
) where

import           Data.Aeson                     (FromJSON (..), withObject,
                                                 (.!=), (.:), (.:?))
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Distribution.Types.PackageName (PackageName)
import qualified Distribution.Types.PackageName as C (mkPackageName)

newtype Packages = Packages (Map.Map PackageName PackagePlan) deriving (Show, Eq)

data PackagePlan = PackagePlan
    { ppVersion     :: Text
    , ppSourceUrl   :: Maybe Text
    , ppGithubPings :: Set.Set Text
    }
    deriving (Show, Eq)

instance FromJSON Packages where
    parseJSON = withObject "BuildPlan" $ \o ->
        Packages . Map.mapKeysWith const C.mkPackageName <$> (o .: "packages")

instance FromJSON PackagePlan where
    parseJSON = withObject "PackageBuild" $ \o -> do
        ppVersion <- o .: "version"
        ppGithubPings <- o .:? "github-pings" .!= mempty
        ppSourceUrl <- o .:? "source-url"
        return PackagePlan{..}
