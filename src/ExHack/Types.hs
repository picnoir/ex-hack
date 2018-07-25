module ExHack.Types (
  Package(..),
  PackageIdentifier(..),
  PackageName,
  PackageDlDesc(..),
  TarballDesc(..),
  ModuleName(..),
  mkPackageName,
  mkVersion,
  getName,
  depsNames,
  packagedlDescName,
  fromComponents
) where

import Data.Set (Set, toList)
import Data.Text (Text, pack)
import Distribution.ModuleName (ModuleName(..), fromComponents)
import Distribution.Types.PackageId (PackageIdentifier(..), pkgName)
import Distribution.Types.PackageName (PackageName, unPackageName, mkPackageName)
import Distribution.Version (mkVersion)
import System.FilePath (FilePath)

data Package = Package {
  name :: PackageIdentifier,
  deps :: Set PackageName,
  cincludes :: Maybe FilePath,
  cabalFile :: Text,
  tarballPath :: FilePath,
  exposedModules :: Maybe [ModuleName]
} deriving (Eq, Show)

-- | Intermediate package description used till we parse the data necessary
--   to generate the proper package description.
--
newtype PackageDlDesc = PackageDlDesc (Text,Text,Text,Text)

-- | Informations extracted from a package entry not yet extracted from its tarball.
--
-- Two elements:
--
--   * Filepath to the tarball.
--   * The cabal file of this package.
--
newtype TarballDesc = TarballDesc (FilePath, Text)

packagedlDescName :: PackageDlDesc -> Text
packagedlDescName (PackageDlDesc (n, _, _, _)) = n

getName :: Package -> Text
getName = pack . unPackageName . pkgName . name

depsNames :: Package -> [String]
depsNames pkg = unPackageName <$> toList (deps pkg)
