module Config (
  cabalFilesDir,
  tarballsDir,
  hoogleFilesDir,
  dbFilePath,
  dataDir
) where

dataDir :: String
dataDir = "data"

cabalFilesDir :: String
cabalFilesDir = dataDir ++ "/cabal/"

tarballsDir :: String
tarballsDir = dataDir ++ "/tarballs/"

hoogleFilesDir :: String
hoogleFilesDir = dataDir ++ "/hoogle/"

dbFilePath :: String
dbFilePath = dataDir ++ "/data.db"
