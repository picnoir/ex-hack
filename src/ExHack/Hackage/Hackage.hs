{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExHack.Hackage.Hackage (
    findComponentRoot,
    getModExports,
    getModNames,
    getPackageExports,
    unpackHackageTarball,
    PackageExports(..)
) where

import qualified Codec.Archive.Tar      as Tar (Entries (..), entryPath, read,
                                                unpack)
import           Codec.Compression.GZip (decompress)
import           Control.DeepSeq        (force)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.ByteString.Lazy   as BL (fromStrict)
import           System.Directory       (makeAbsolute, withCurrentDirectory)
import           System.FilePath        (FilePath, (</>))

import qualified ExHack.Ghc             as GHC (getDesugaredMod, getModExports)
import           ExHack.ModulePaths     (findComponentRoot)
import           ExHack.Types           (ComponentRoot (..), ModuleExports,
                                         ModuleName, MonadLog,
                                         Package (exposedModules),
                                         PackageComponent (..),
                                         PackageExports (..),
                                         PackageFilePath (..))

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

getModNames :: Package -> [ModuleName]
getModNames p = maybe mempty mods exMods
    where
        exMods :: Maybe PackageComponent
        !exMods = exposedModules p 

-- | Retrieve the exported symbols of a module.
getModExports :: forall m. (MonadIO m, MonadThrow m, MonadLog m) 
            => PackageFilePath -> [ComponentRoot] -> ModuleName -> m ModuleExports
getModExports pfp@(PackageFilePath pfps) croots mn = 
    liftIO $ withCurrentDirectory pfps $ do
        cr <- findComponentRoot pfp croots mn
        ds <- GHC.getDesugaredMod pfp cr mn 
        let !exps = force $ GHC.getModExports ds
        pure (mn, exps)

-- | Retrieve the exported symbols of a package, module by module.
getPackageExports :: forall m. (MonadIO m, MonadThrow m, MonadLog m)
            => PackageFilePath -> Package -> m [ModuleExports]
getPackageExports pfp@(PackageFilePath pfps) p = 
    liftIO $ withCurrentDirectory pfps $ getModExports pfp croots `mapM` mns
  where
    pcs :: Maybe PackageComponent
    pcs = exposedModules p 
    croots :: [ComponentRoot]
    !croots = maybe [ComponentRoot "./"] roots pcs
    mns :: [ModuleName]
    mns = getModNames p 
