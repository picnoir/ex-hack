module Config (
  cabalFilesDir,
  tarballsDir,
  dbFilePath,
  dataDir
) where

dataDir :: String
dataDir = "data"

cabalFilesDir :: String
cabalFilesDir = dataDir ++ "/cabal/"

tarballsDir :: String
tarballsDir = dataDir ++ "/tarballs/"

dbFilePath :: String
dbFilePath = dataDir ++ "/data.db"
