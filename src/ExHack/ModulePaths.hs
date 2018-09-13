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

modName :: ModuleName -> String
modName mn = intercalate "." $ components mn

toModFilePath :: PackageFilePath -> ComponentRoot -> ModuleName -> FilePath
toModFilePath (PackageFilePath pfp) (ComponentRoot cr) mn = 
    pfp </> cr </> toFilePath mn <> ".hs"

findComponentRoot :: (MonadIO m, MonadThrow m) => PackageFilePath -> [ComponentRoot] -> ModuleName -> m ComponentRoot  
findComponentRoot (PackageFilePath pfp) croots mn = do
    -- We need to append the package basePath to the roots
    let acroots = (\(ComponentRoot cr') -> ComponentRoot (pfp <> cr')) <$> croots
    xs <- liftIO $ withCurrentDirectory pfp $ filterM testPath ("./." : acroots)
    if length xs == 1
       then pure $ head xs
       else throwM $ CannotFindModuleFile mn croots
  where
    testPath (ComponentRoot p) = liftIO $ doesPathExist (p </> toFilePath mn <.> "hs")  
