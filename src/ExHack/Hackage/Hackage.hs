{-# LANGUAGE LambdaCase #-}
module ExHack.Hackage.Hackage (
    unpackHackageTarball,
    loadExposedModules,
    getTarballDesc,
    getPackageExports,
    PackageExports(..)
) where

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T (readFile)
import Distribution.ModuleName (ModuleName)
import Codec.Compression.GZip (decompress)
import qualified Codec.Archive.Tar as Tar (Entries(..), unpack, read, entryPath)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (fromStrict)
import System.Directory (listDirectory, makeAbsolute, withCurrentDirectory)
import System.FilePath (FilePath, (</>))

import ExHack.Ghc (DesugaredModule, getDesugaredMod, getModExports)
import ExHack.Types (Package(exposedModules), TarballDesc(..))

newtype PackageExports = PackageExports [(ModuleName, [String])]
  deriving (Show, Eq)


-- | Unpack a tarball to a specified directory.
unpackHackageTarball :: (MonadIO m) => 
  FilePath -- ^ 'FilePath' pointing to the directory we want to extract the tarball to. 
  -> BS.ByteString -- ^ Tarball in the 'ByteString'.
  -> m FilePath -- ^ Newly created directory containing the extracted tarball.
unpackHackageTarball dir tb = do
  let rp = Tar.read . decompress $ BL.fromStrict tb
  liftIO $ Tar.unpack dir rp 
  adir <- liftIO $ makeAbsolute dir
  pure $ adir </> getRootPath rp 
  where
    getRootPath (Tar.Next e _) = Tar.entryPath e
    getRootPath _ = error "Cannot find tar's root directory."

-- | Get the tarball description of a directory. Returns Nothing if
-- the specified directory is empty.
getTarballDesc :: (MonadIO m) => 
  FilePath -- ^ 'FilePath' that may contain a tarball. 
  -> m (Maybe TarballDesc)
getTarballDesc fp = do
  f <- Just <$> liftIO (listDirectory fp)
  let mcfp = f >>= getCabalFp >>= \case
        [] -> Nothing
        a  -> Just $ head a
  case mcfp of
    Nothing -> pure Nothing
    Just cfp -> do
      fcontent <- liftIO $ T.readFile (fp </> cfp)
      pure . Just $ TarballDesc (fp, fcontent)
  where
    -- Filters .cabal files out of a list of files.
    getCabalFp = Just <$> filter (isSuffixOf ".cabal")

-- | Retrieve the exported symbols of a cabal package.
getPackageExports :: (MonadIO m) => FilePath -> Package -> m PackageExports
getPackageExports pfp p = do
  em <- liftIO $ withCurrentDirectory pfp (loadExposedModules pfp p)
  pure $ PackageExports $ getExports <$> em
    where
      getExports (mn, dm) = (mn, getModExports dm) 

loadExposedModules :: (MonadIO m) => FilePath -> Package -> m [(ModuleName, DesugaredModule)] 
loadExposedModules pfp p = (loadModule pfp) `mapM` fromMaybe mempty (exposedModules p)

loadModule :: (MonadIO m) => FilePath -> ModuleName -> m (ModuleName, DesugaredModule)
loadModule pfp mn = getDesugaredMod pfp mn >>= \m -> pure (mn,m)
