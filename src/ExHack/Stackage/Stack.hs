module ExHack.Stackage.Stack (
  setup
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (unpack)
import Control.Lens ((^.))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode )

import ExHack.Config (StackConfig, workDir, nbJobs, stackRoot)

setup :: MonadIO m => StackConfig -> m (Maybe Int)
setup c = do
  (ec, _, _) <- liftIO $ readProcessWithExitCode 
      ("stack setup --work-dir=" 
        <> unpack (c ^. workDir)
        <> " --stack-root=" <> unpack (c ^. stackRoot)
        <>  " -j" <> show (c ^. nbJobs))
      [] ""
  case ec of
    ExitSuccess -> pure Nothing
    ExitFailure i -> pure $ Just i
