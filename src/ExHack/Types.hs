module ExHack.Types (
  Package(..),
  PackageIdentifier(..),
  PackageName,
  PackageDlDesc(..),
  mkPackageName,
  mkVersion,
  getName,
  depsNames,
  packagedlDescName
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

-- | Intermediate package description
--   used till we parse the data necessary
--   to generate the proper package description.
newtype PackageDlDesc = PackageDlDesc (Text,Text,Text,Text)

packagedlDescName :: PackageDlDesc -> Text
packagedlDescName (PackageDlDesc (n, _, _, _)) = n

getName :: Package -> Text
getName = pack . unPackageName . pkgName . name

depsNames :: Package -> [String]
depsNames pkg = unPackageName <$> toList (deps pkg)
