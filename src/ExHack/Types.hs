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
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Version

data Package = Package {
  name :: PackageIdentifier,
  deps :: Set PackageName
} deriving (Eq, Show)

getName :: Package -> String
getName = unPackageName . pkgName . name

depsNames :: Package -> [String]
depsNames pkg = unPackageName <$> toList (deps pkg)
