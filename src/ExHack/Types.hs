{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExHack.Types (
    Config(..),
    Package(..),
    PackageIdentifier(..),
    PackageName,
    PackageDlDesc(..),
    TarballDesc(..),
    ModuleName(..),
    SymbolName(..),
    DatabaseHandle,
    DatabaseStatus(..),
    StackageFile(..),
    TarballsDir(..),
    CabalFilesDir(..),
    WorkDir(..),
    PackageExports(..),
    MonadStep,
    Step,
    tarballsDir,
    cabalFilesDir,
    workDir,
    runStep,
    mkPackageName,
    mkVersion,
    getName,
    getModName,
    depsNames,
    packagedlDescName,
    fromComponents
) where

import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (ReaderT, MonadReader, 
                             runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Data.Set (Set, toList)
import Data.Text (Text, pack, intercalate)
import Data.String (IsString)
import Database.Selda (RowID)
import Distribution.ModuleName (ModuleName(..), fromComponents, components)
import Distribution.Types.PackageId (PackageIdentifier(..), pkgName)
import Distribution.Types.PackageName (PackageName, unPackageName, mkPackageName)
import Distribution.Version (mkVersion)
import System.FilePath (FilePath)

import ExHack.Utils (Has(..))


data Package = Package {
  name :: PackageIdentifier,
  deps :: Set PackageName,
  cabalFile :: Text,
  tarballPath :: FilePath,
  exposedModules :: Maybe [ModuleName],
  dbId :: Maybe RowID
} deriving (Eq, Show)

type DatabaseHandle (a :: DatabaseStatus) = FilePath

newtype TarballsDir = TarballsDir FilePath
newtype CabalFilesDir = CabalFilesDir FilePath
newtype WorkDir = WorkDir FilePath

data DatabaseStatus = New | Initialized | DepsGraph | PkgExports

newtype StackageFile = StackageFile Text

data Config a = Config {
    _dbHandle :: DatabaseHandle a,
    _stackageFile :: StackageFile,
    _tarballsDir :: TarballsDir,
    _cabalFilesDir :: CabalFilesDir,
    _workDir :: WorkDir
}

makeLenses ''Config

instance Has (Config 'New) (DatabaseHandle 'New) where
    hasLens = dbHandle

instance Has (Config 'Initialized) (DatabaseHandle 'Initialized) where
    hasLens = dbHandle

instance Has (Config a) StackageFile where
    hasLens = stackageFile

instance Has (Config a) TarballsDir where
    hasLens = tarballsDir

instance Has (Config a) CabalFilesDir where
    hasLens = cabalFilesDir

instance Has (Config a) WorkDir where
    hasLens = workDir

-- | Intermediate package description used till we parse the data necessary
--   to generate the proper package description.
-- 
--   (packageName, cabalUrl, tarballUrl)
newtype PackageDlDesc = PackageDlDesc (Text,Text,Text)

-- | Informations extracted from a package entry not yet extracted from its tarball.
--
-- Two elements:
--
--   * Filepath to the tarball.
--   * The cabal file of this package.
--
newtype TarballDesc = TarballDesc (FilePath, Text)

newtype SymbolName = SymbolName Text
    deriving (Show, Eq, IsString)

type MonadStep c m = (MonadIO m, MonadMask m, MonadReader c m)

type Step c a = ReaderT c IO a

-- | Type containing a package exported symbols.
--
-- Three elements:
--
-- * A Package database id.
-- * For each module:
--     * A name.
--     * A list containing the exported symbols.
newtype PackageExports = PackageExports (Package, [(ModuleName, [SymbolName])])
  deriving (Show, Eq)

runStep :: Step c a -> c -> IO a
runStep = runReaderT

packagedlDescName :: PackageDlDesc -> Text
packagedlDescName (PackageDlDesc (n, _, _)) = n

getName :: Package -> Text
getName = pack . unPackageName . pkgName . name

depsNames :: Package -> [String]
depsNames pkg = unPackageName <$> toList (deps pkg)

getModName :: ModuleName -> Text
getModName x = intercalate "." (pack <$> components x)
