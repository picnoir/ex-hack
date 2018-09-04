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
    PackageFilePath(..),
    PackageName,
    PackageDlDesc(..),
    TarballDesc(..),
    ModuleName(..),
    ModuleNameT(..),
    IndexedModuleNameT(..),
    SymName(..),
    LocatedSym(..),
    IndexedSym(..),
    UnifiedSym(..),
    DatabaseHandle,
    DatabaseStatus(..),
    StackageFile(..),
    TarballsDir(..),
    CabalFilesDir(..),
    WorkDir(..),
    PackageExports(..),
    MonadLog(..),
    ImportsScope,
    MonadStep,
    Step,
    ModuleFileError(..),
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
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask, Exception)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Hashable (Hashable)
import Data.Set (Set, toList)
import GHC (GenLocated(..), SrcSpan)
import qualified Data.HashMap.Strict as HM (HashMap)
import qualified Data.HashSet as HS (HashSet)
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
    roots :: [ComponentRoot]
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

-- | Kind of cabal's ModuleName duplicate internally using Text. We want to be able
--   to hash the module name, cabal one's cannot, hence this datatype.
newtype ModuleNameT = ModuleNameT Text
    deriving (Show, Eq, IsString, Hashable)

-- | Module being indexed in the dabase.
newtype IndexedModuleNameT = IndexedModuleNameT (ModuleNameT, Int)
    deriving (Show, Eq, Hashable)

-- | Source code symbol.
newtype SymName = SymName Text
    deriving (Show, Eq, IsString, Hashable)

-- | Symbol having been indexed in the database.
--   Contains both a symbol name as well as its database identifier.
newtype IndexedSym = IndexedSym (SymName, Int)
    deriving (Show, Eq, Hashable)

-- | Occurence of a symbol in a source code file.
newtype LocatedSym = LocatedSym (Package, FilePath, GenLocated SrcSpan SymName)
    deriving (Eq)

newtype UnifiedSym = UnifiedSym (IndexedSym, LocatedSym)
    deriving (Eq)


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
runStep (Step r) = runReaderT r

-- | Directory where a Haskell package has been extracted to.
newtype PackageFilePath = PackageFilePath FilePath
    deriving (Show, Eq)

-- | Type containing a package exported symbols.
--
-- Three elements:
--
-- * A Package.
-- * A filepath pointing to the directory where the tarball is extracted.
-- * For each module:
--     * A name.
--     * A list containing the exported symbols.
newtype PackageExports = PackageExports (Package, PackageFilePath, [(ModuleName, [SymName])])
  deriving (Show, Eq)

type ImportsScope = HM.HashMap IndexedModuleNameT (HS.HashSet IndexedSym) 

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

-- Exceptions
-- =================

data ModuleFileError = CannotFindModuleFile String
    deriving (Show, Eq)
instance Exception ModuleFileError
