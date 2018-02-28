module ExHack.Types (
  Package(..),
  PackageIdentifier(..),
  PackageName,
  mkPackageName,
  mkVersion
) where

import Data.Set (Set)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Version

data Package = Package {
  name :: PackageIdentifier,
  deps :: Set PackageName
} deriving (Eq, Show)
