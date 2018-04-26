module ExHack.Types (
  Package(..),
  PackageIdentifier(..),
  PackageName,
  mkPackageName,
  mkVersion,
  getName,
  depsNames
) where

import Data.Set (Set, toList)
import Data.Text (Text, pack)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Version
import System.FilePath (FilePath)

data Package = Package {
  name :: PackageIdentifier,
  deps :: Set PackageName,
  cabalFile :: Text,
  tarballPath :: FilePath
} deriving (Eq, Show)

getName :: Package -> Text
getName = pack . unPackageName . pkgName . name

depsNames :: Package -> [String]
depsNames pkg = unPackageName <$> toList (deps pkg)
