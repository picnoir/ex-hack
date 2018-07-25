module ExHack.Stackage.Stack (
  setup,
  build
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Control.Lens ((^.))
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode )

import ExHack.Config (StackConfig, workDir, nbJobs, 
                      stackRoot, stackBin, gccBin)

setup :: MonadIO m => StackConfig -> m (Maybe (Int, String))
setup c = runStackCommand c ["setup"]

build :: MonadIO m => StackConfig -> FilePath -> m (Maybe (Int, String))
build c fp = do
  liftIO $ putStrLn $ "cd " <> fp
  liftIO $ setCurrentDirectory fp
  runStackCommand c ["build"]

runStackCommand :: MonadIO m => StackConfig -> [String] -> m (Maybe (Int, String))
runStackCommand c cmd = do
  liftIO . putStrLn $ stackPath <> " " <> concat cmd <> " --stack-root " <> unpack (c ^. stackRoot) <> " " <> "--work-dir " <> unpack (c ^. workDir) <> " " <> "-j " <> show (c ^. nbJobs) <> " --with-gcc " <> gccPath
  (ec, _, err) <- liftIO $ readProcessWithExitCode 
      stackPath
      (cmd <> ["--stack-root", unpack (c ^. stackRoot),
       "--work-dir", unpack (c ^. workDir),
       "-j",show (c ^. nbJobs),
       "--with-gcc", gccPath])
      ""
  case ec of
    ExitSuccess -> pure Nothing
    ExitFailure i -> pure $ Just (i, err)
  where
    stackPath =  fromMaybe "stack" $ c ^. stackBin
    gccPath = fromMaybe "gcc" $ c ^. gccBin
