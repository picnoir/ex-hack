{-|
Module      : ExHack.ModulePaths
Description : Helpers related to modules filepaths.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}

module ExHack.ModulePaths (
    modName,
    findComponentRoot,
    toModFilePath
) where

import           Control.Monad           (filterM)
import           Control.Monad.Catch     (MonadThrow, throwM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.List               (intercalate)
import           Distribution.ModuleName (ModuleName, components, toFilePath)
import           System.Directory        (doesPathExist, withCurrentDirectory)
import           System.FilePath         ((<.>), (</>))

import           ExHack.Types            (ComponentRoot (..),
                                          PackageFilePath (..),
                                          PackageLoadError (..))

-- | Convert a `ModuleName` to its canonical form, eg. @Data.Text@ .
modName :: ModuleName -> String
modName mn = intercalate "." $ components mn

-- | Generates the module filepath according to a package fp, a component root
--   and a module name.
toModFilePath :: PackageFilePath -> ComponentRoot -> ModuleName -> FilePath
toModFilePath (PackageFilePath pfp) (ComponentRoot cr) mn = 
    pfp </> cr </> toFilePath mn <.> "hs"

-- | Look for a module file in a list of component roots and return
--   the `ComponentRoot` containing the actual module file.
findComponentRoot :: (MonadIO m, MonadThrow m) => PackageFilePath -> [ComponentRoot] -> ModuleName -> m ComponentRoot  
findComponentRoot (PackageFilePath pfp) croots mn = do
    let acroots = (\(ComponentRoot cr') -> ComponentRoot (pfp </> cr')) <$> croots
    xs <- liftIO $ withCurrentDirectory pfp $ filterM testPath ("./" : acroots)
    if length xs == 1
       then pure $ head xs
       else throwM $ CannotFindModuleFile mn croots
  where
    testPath (ComponentRoot p) = liftIO $ doesPathExist (p </> toFilePath mn <.> "hs")  
