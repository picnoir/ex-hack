{-|
Module      : ExHack.Cabal.Cabal
Description : Cabal install wrapper.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
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

-- | Build a package after installing its dependencies using cabal-install.
--
--   Cabal install should be available in the binary path.
--
--   Returns both the exit code and the detailed error message in case
--   of error.
buildPackage :: MonadIO m => PackageFilePath -> m (Maybe (Int, String))
buildPackage (PackageFilePath pfp) = liftIO $ withCurrentDirectory pfp cabalBuild
  where
    cabalBuild = do
        rid <- installDeps
        if isJust rid
            then pure rid
            else build

-- TODO: do not hardcode resolver
installDeps :: MonadIO m => m (Maybe (Int, String))
installDeps = runCabalCommand ["init", "--resolver", "lts-12.11", "--omit-packages"]

build :: MonadIO m => m (Maybe (Int, String))
build = runCabalCommand ["build", "--resolver", "lts-12.11"]

runCabalCommand :: MonadIO m => [String] -> m (Maybe (Int, String))
runCabalCommand cmd = do
  liftIO $ unsetEnv "GHC_PACKAGE_PATH"
  (ec, _, err) <- liftIO $ readProcessWithExitCode
      cabalPath cmd ""
  case ec of
    ExitSuccess -> pure Nothing
    ExitFailure i -> pure $ Just (i, err)
  where
    cabalPath = "stack"
