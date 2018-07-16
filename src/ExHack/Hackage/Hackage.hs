module ExHack.Hackage.Hackage (
    unpackHackageTarball
) where

import Codec.Compression.GZip (decompress)
import qualified Codec.Archive.Tar as Tar (Entries(..), unpack, read, entryPath)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (fromStrict)
import System.FilePath (FilePath, (</>))

unpackHackageTarball :: (MonadIO m) => FilePath -> BS.ByteString -> m FilePath
unpackHackageTarball dir tb = do
  let rp = Tar.read . decompress $ BL.fromStrict tb
  liftIO $ Tar.unpack dir rp 
  pure $ dir </> getRootPath rp 
  where
    getRootPath (Tar.Next e _) = Tar.entryPath e
    getRootPath _ = error "Cannot find tar's root directory."
