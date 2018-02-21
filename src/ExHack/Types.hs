module ExHack.Types (
  Package(..),
  PackageIdentifier(..),
  mkVersion
) where

import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Version

data Package = Package {
  name :: PackageIdentifier,
  deps :: [PackageName]
} deriving (Eq, Show)


