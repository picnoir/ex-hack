{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module ExHack.Config (
  StackConfig(..),
  config,
  nbJobs,
  workDir,
  stackBin,
  gccBin,
  stackRoot,
  tarballsDir
) where

import Data.Text (Text)
import Control.Lens.TH (makeLenses)

-- TODO: write proper config parser
config :: StackConfig
config = StackConfig {_nbJobs=4, _workDir=".stack-work/", _stackRoot=d, _stackBin=Just "/nix/store/4s5l0p1s906prwsapm2g083cp5z4haxl-stack-1.6.5/bin/stack", _tarballsDir="/home/minoulefou/exhst/tb/", _gccBin = Just "/nix/store/gqg2vrcq7krqi9rrl6pphvsg81sb8pjw-gcc-wrapper-7.3.0/bin/gcc"}
     where
       d = "/home/minoulefou/exhst/"

data StackConfig = StackConfig {
  _nbJobs :: Int,
  _workDir :: Text,
  _stackRoot :: Text,
  _tarballsDir :: FilePath,
  _stackBin :: Maybe FilePath,
-- ^ When using Nix, you may not have 
-- stack in your path and might want to use
-- the proper store path. Change this line with the
-- actual stack store path using the substituteInPlace function
-- in the postConfigure hook.
--
-- See default.nix for the actual implementation.
  _gccBin :: Maybe FilePath
}

makeLenses ''StackConfig
