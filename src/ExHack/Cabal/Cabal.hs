module ExHack.Cabal.Cabal (
  buildPackage
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (isJust)
import           System.Directory       (withCurrentDirectory)
import           System.Environment     (unsetEnv)
import           System.Exit            (ExitCode (..))
import           System.Process         (readProcessWithExitCode)

import           ExHack.Types           (PackageFilePath (..))

buildPackage :: MonadIO m => PackageFilePath -> m (Maybe (Int, String))
buildPackage (PackageFilePath pfp) = liftIO $ withCurrentDirectory pfp cabalBuild
  where
    cabalBuild = do
        rid <- installDeps
        if isJust rid
            then pure rid
            else build

installDeps :: MonadIO m => m (Maybe (Int, String))
installDeps = runCabalCommand ["install","--dependencies-only"]

build :: MonadIO m => m (Maybe (Int, String))
build = runCabalCommand ["build"]

runCabalCommand :: MonadIO m => [String] -> m (Maybe (Int, String))
runCabalCommand cmd = do
  liftIO $ unsetEnv "GHC_PACKAGE_PATH"
  (ec, _, err) <- liftIO $ readProcessWithExitCode
      cabalPath cmd ""
  case ec of
    ExitSuccess -> pure Nothing
    ExitFailure i -> pure $ Just (i, err)
  where
    cabalPath = "cabal"
