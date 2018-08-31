{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ExHack.Types (
    Config(..),
    ComponentRoot(..),
    PackageComponent(..),
    Package(..),
    PackageIdentifier(..),
    PackageName,
    PackageDlDesc(..),
    TarballDesc(..),
    ModuleName(..),
    ModuleNameT(..),
    SymbolName(..),
    DatabaseHandle,
    DatabaseStatus(..),
    StackageFile(..),
    TarballsDir(..),
    CabalFilesDir(..),
    WorkDir(..),
    PackageExports(..),
    MonadLog(..),
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
    getModNameT,
    depsNames,
    packagedlDescName,
    fromComponents
) where

import Prelude hiding (replicate, length)

import Control.Lens.TH (makeLenses)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Hashable (Hashable)
import Data.Set (Set, toList)
import Data.Text (Text, pack, intercalate, replicate, length)
import qualified Data.Text.IO as TIO (putStrLn, hPutStrLn) 
import Data.String (IsString)
import Database.Selda (RowID, SeldaM)
import Distribution.ModuleName (ModuleName(..), fromComponents, components)
import Distribution.Types.PackageId (PackageIdentifier(..), pkgName)
import Distribution.Types.PackageName (PackageName, unPackageName, mkPackageName)
import Distribution.Version (mkVersion)
import System.FilePath (FilePath)
import System.IO (stderr)

import ExHack.Utils (Has(..))

-- | Cabal component root filepath. The module names should be
--   resolved using this filepath as root.
newtype ComponentRoot = ComponentRoot FilePath
    deriving (IsString, Eq, Show)

-- | Cabal package component.
data PackageComponent = PackageComponent {
    mods :: [ModuleName],
    root :: [ComponentRoot]
} deriving (Eq, Show)

-- | Package main datatype.
data Package = Package {
  name :: PackageIdentifier,
  deps :: Set PackageName,
  cabalFile :: Text,
  tarballPath :: FilePath,
  exposedModules :: Maybe PackageComponent,
  dbId :: Maybe RowID,
  allModules :: [PackageComponent]
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

-- Needed: cabal's ModuleName is not hashable but we still
-- want to use it as a hashmap key
newtype ModuleNameT = ModuleNameT Text
    deriving (Show, Eq, IsString, Hashable)

newtype SymbolName = SymbolName Text
    deriving (Show, Eq, IsString, Hashable)

class (Monad m) => MonadLog m where
    logInfo, logError, logTitle :: Text -> m ()
    logTitle txt = line >> logInfo ("* " <> txt <> " *") >> line
        where 
            line :: m ()
            !line = logInfo (replicate (length txt + 4) "*")

instance MonadLog IO where
    logInfo = TIO.putStrLn
    logError = TIO.hPutStrLn stderr

instance MonadLog (Step c s)  where
    logInfo = liftIO . TIO.putStrLn
    logError = liftIO . TIO.hPutStrLn stderr

instance MonadLog SeldaM where
    logInfo = liftIO . TIO.putStrLn
    logError = liftIO . TIO.hPutStrLn stderr

type MonadStep c m = (MonadIO m, MonadMask m, MonadReader c m, MonadLog m)

newtype Step c s a = Step (ReaderT (Config s) IO a)
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadReader (Config s), MonadCatch, MonadThrow, MonadMask)

runStep :: Step c s a -> Config s -> IO a
runStep = undefined
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

packagedlDescName :: PackageDlDesc -> Text
packagedlDescName (PackageDlDesc (n, _, _)) = n

getName :: Package -> Text
getName = pack . unPackageName . pkgName . name

depsNames :: Package -> [String]
depsNames pkg = unPackageName <$> toList (deps pkg)

getModName :: ModuleName -> Text
getModName x = intercalate "." (pack <$> components x)

getModNameT :: ModuleName -> ModuleNameT
getModNameT x = ModuleNameT $ intercalate "." (pack <$> components x)
