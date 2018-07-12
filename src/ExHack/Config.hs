{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module ExHack.Config (
  StackConfig(..),
  config,
  nbJobs,
  workDir,
  stackRoot
) where

import Data.Text (Text)
import Control.Lens.TH (makeLenses)

-- TODO: write proper config parser
config :: StackConfig
config = StackConfig{_nbJobs=4, _workDir=d <> ".stack-work/", _stackRoot=d}
     where
       d = "/home/minoulefou/Code/Haskell/ExHack/"

data StackConfig = StackConfig {
  _nbJobs :: Int,
  _workDir :: Text,
  _stackRoot :: Text
}

makeLenses ''StackConfig
