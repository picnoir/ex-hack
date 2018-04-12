module Config (
  cabalFilesDir,
  tarballsDir,
  dbFilePath
) where

cabalFilesDir :: String
cabalFilesDir = "cabal/"

tarballsDir :: String
tarballsDir = "tarballs/"

dbFilePath :: String
dbFilePath = "test.db"
