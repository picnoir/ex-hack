module ExHack.Stackage.Stack (
  setup,
  build
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Control.Lens ((^.))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode )

import ExHack.Config (StackConfig, workDir, nbJobs, 
                      stackRoot, stackBin)

setup :: MonadIO m => StackConfig -> m (Maybe (Int, String))
setup c = do
  (ec, _, err) <- liftIO $ readProcessWithExitCode 
      sb
      ["setup",
       "--stack-root", unpack (c ^. stackRoot),
       "--work-dir", unpack (c ^. workDir),
       "-j",show (c ^. nbJobs)] 
      ""
  case ec of
    ExitSuccess -> pure Nothing
    ExitFailure i -> pure $ Just (i, err)
  where
    sb = fromMaybe "stack" $ c ^. stackBin

build :: MonadIO m => StackConfig -> m (Maybe (Int, String))
build = undefined
-- 1. Unzip content
-- 2. Change dir
-- 3. Run command
