{-|
Module      : ExHack.StackageTypes
Description : Stackage-related types.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ExHack.Stackage.StackageTypes (
  PackagePlan(..),
  Packages(..)
) where

import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Data.Yaml                      (FromJSON (..), withObject,
                                                 (.!=), (.:), (.:?))
import           Distribution.Types.PackageName (PackageName)
import qualified Distribution.Types.PackageName as C (mkPackageName)

-- | Packages defined in a stackage plan.
newtype Packages = Packages (Map.Map PackageName PackagePlan) deriving (Show, Eq)

-- | Package plan used by stackage de uniquely identify
--   a package.
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
