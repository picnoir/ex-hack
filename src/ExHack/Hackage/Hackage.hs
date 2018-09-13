{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExHack.Hackage.Hackage (
    unpackHackageTarball,
    loadExposedModules,
    getTarballDesc,
    getPackageExports,
    findComponentRoot,
    PackageExports(..)
) where

import qualified Codec.Archive.Tar      as Tar (Entries (..), entryPath, read,
                                                unpack)
import           Codec.Compression.GZip (decompress)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.ByteString.Lazy   as BL (fromStrict)
import           Data.List              (isSuffixOf)
import qualified Data.Text.IO           as T (readFile)
import           System.Directory       (listDirectory, makeAbsolute,
                                         withCurrentDirectory)
import           System.FilePath        (FilePath, (</>))

import           ExHack.Ghc             (DesugaredModule, getDesugaredMod,
                                         getModExports)
import           ExHack.ModulePaths     (findComponentRoot)
import           ExHack.Types           (ComponentRoot (..), ModuleName,
                                         Package (exposedModules),
                                         PackageComponent (..),
                                         PackageExports (..),
                                         PackageFilePath (..), TarballDesc (..))

-- | Unpack a tarball to a specified directory.
unpackHackageTarball :: (MonadIO m) => 
  FilePath -- ^ 'FilePath' pointing to the directory we want to extract the tarball to. 
  -> BS.ByteString -- ^ Tarball in the 'ByteString'.
  -> m PackageFilePath -- ^ Newly created directory containing the extracted tarball.
unpackHackageTarball dir tb = do
  let rp = Tar.read . decompress $ BL.fromStrict tb
  liftIO $ Tar.unpack dir rp 
  adir <- liftIO $ makeAbsolute dir
  pure $ PackageFilePath $ adir </> getRootPath rp 
  where
    getRootPath (Tar.Next e _) = Tar.entryPath e
    getRootPath _ = error "Cannot find tar's root directory."

-- | Get the tarball description of a directory. Returns Nothing if
-- the specified directory is empty.
getTarballDesc :: (MonadIO m) => 
  PackageFilePath -- ^ 'FilePath' that may contain a tarball. 
  -> m (Maybe TarballDesc)
getTarballDesc (PackageFilePath fp) = do
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
getPackageExports :: (MonadIO m) => PackageFilePath -> Package -> m PackageExports
getPackageExports pfp@(PackageFilePath pfps) p = do
  em <- liftIO $ withCurrentDirectory pfps (loadExposedModules pfp p)
  pure $ PackageExports (p, pfp, getExports <$> em)
    where
      getExports (mn, dm) = (mn, getModExports dm) 

loadExposedModules :: (MonadIO m, MonadThrow m) => PackageFilePath -> Package -> m [(ModuleName, DesugaredModule)] 
loadExposedModules pfp p = loadModule pfp croots `mapM` maybe mempty mods exMods
    where
        !exMods = exposedModules p 
        croots :: [ComponentRoot]
        !croots = maybe [ComponentRoot "./"] roots exMods

loadModule :: forall m. (MonadIO m, MonadThrow m) => PackageFilePath -> [ComponentRoot] -> ModuleName -> m (ModuleName, DesugaredModule)
loadModule pfp croots mn = do
    cr <- findComponentRoot pfp croots mn
    getDesugaredMod pfp cr mn >>= \m -> pure (mn,m)
