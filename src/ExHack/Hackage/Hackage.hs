{-# LANGUAGE LambdaCase #-}
module ExHack.Hackage.Hackage (
    unpackHackageTarball,
    loadExposedModules,
    getTarballDesc
) where

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T (readFile)
import Distribution.ModuleName (ModuleName, toFilePath)
import Codec.Compression.GZip (decompress)
import qualified Codec.Archive.Tar as Tar (Entries(..), unpack, read, entryPath)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (fromStrict)
import System.Directory (listDirectory)
import System.FilePath (FilePath, (</>))

import ExHack.Ghc (DesugaredModule, getDesugaredMod)
import ExHack.Types (Package(exposedModules), TarballDesc(..))

-- | Unpack a tarball to a specified directory.
unpackHackageTarball :: (MonadIO m) => 
  FilePath -- ^ 'FilePath' pointing to the directory we want to extract the tarball to. 
  -> BS.ByteString -- ^ Tarball in the 'ByteString'.
  -> m FilePath -- ^ Newly created directory containing the extracted tarball.
unpackHackageTarball dir tb = do
  let rp = Tar.read . decompress $ BL.fromStrict tb
  liftIO $ Tar.unpack dir rp 
  pure $ dir </> getRootPath rp 
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
      fcontent <- liftIO $ T.readFile cfp
      pure . Just $ TarballDesc (fp, fcontent)
  where
    -- Filters .cabal files out of a list of files.
    getCabalFp = Just <$> filter (\x -> ".cabal" `isSuffixOf` x) 


loadExposedModules :: (MonadIO m) => Package -> m [DesugaredModule] 
loadExposedModules xs = loadModule `mapM` fromMaybe mempty (exposedModules xs)

loadModule :: (MonadIO m) => ModuleName -> m DesugaredModule
loadModule p = getDesugaredMod fn modn
  where
    modn = show p
    fn = "./" <> toFilePath p
