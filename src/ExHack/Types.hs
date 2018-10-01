{-|
Module      : ExHack.Types
Description : ExHack data types collection.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module ExHack.Types (
    AlterDatabase,
    CabalBuildError(..),
    CabalFilesDir(..),
    ComponentRoot(..),
    Config(..),
    DatabaseHandle,
    DatabaseStatus(..),
    HtmlDir(..),
    ImportsScope,
    IndexedModuleNameT(..),
    IndexedSym(..),
    LocatedSym(..),
    ModuleName(..),
    ModuleNameT(..),
    MonadLog(..),
    MonadStep,
    Package(..),
    PackageComponent(..),
    PackageDlDesc(..),
    PackageExports(..),
    PackageFilePath(..),
    PackageIdentifier(..),
    PackageLoadError(..),
    PackageName,
    PackageNameT(..),
    SourceCodeFile(..),
    StackageFile(..),
    Step,
    SymName(..),
    TarballDesc(..),
    TarballsDir(..),
    UnifiedSym(..),
    WorkDir(..),
    cabalFilesDir,
    createDirs,
    depsNames,
    fromComponents,
    getDatabaseHandle,
    getModName,
    getModNameT,
    getName,
    getPackageNameT,
    htmlDir,
    mkPackageName,
    mkVersion,
    newDatabaseHandle,
    packagedlDescName,
    runStep,
    tarballsDir,
    workDir
) where

import           Prelude                        hiding (length, replicate)

import           Control.Lens.TH                (makeLenses)
import           Control.Monad.Catch            (Exception, MonadCatch,
                                                 MonadMask, MonadThrow)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as HM (HashMap)
import qualified Data.HashSet                   as HS (HashSet)
import           Data.Set                       (Set, toList)
import           Data.String                    (IsString)
import           Data.Text                      (Text, intercalate, length,
                                                 pack, replicate)
import qualified Data.Text.IO                   as TIO (hPutStrLn, putStrLn)
import           Database.Selda                 (RowID, SeldaM)
import           Distribution.ModuleName        (ModuleName (..), components,
                                                 fromComponents)
import           Distribution.Types.PackageId   (PackageIdentifier (..),
                                                 pkgName)
import           Distribution.Types.PackageName (PackageName, mkPackageName,
                                                 unPackageName)
import           Distribution.Version           (mkVersion)
import           GHC                            (GenLocated (..), SrcSpan)
import           System.Console.ANSI            (Color (Blue, Red),
                                                 ColorIntensity (Vivid),
                                                 ConsoleLayer (Foreground),
                                                 SGR (Reset, SetColor), setSGR)
import           System.FilePath                (FilePath)
import           System.IO                      (stderr)

import           ExHack.Utils                   (Has (..))

-- | Cabal component root filepath. 
--
--   Module names should be resolved using this filepath as root.
newtype ComponentRoot = ComponentRoot FilePath
    deriving (IsString, Eq, Show)

-- | Cabal package component.
--
--   In cabal's terms, a package component is a collection of modules
--   which can materialize either a library, a test suite or an application.
--
--   A same package component source code can be splitted into several
--   directories AKA. component roots.
data PackageComponent = PackageComponent {
    mods  :: [ModuleName],
    roots :: [ComponentRoot]
} deriving (Eq, Show)

-- | Package main datatype.
data Package = Package {
  name :: PackageIdentifier,
  -- ^ A package identified contains both a unique identifier
  --   as well as a version.
  deps :: Set PackageName,
  -- ^ Dependancies needed to build this package.
  cabalFile :: Text,
  -- ^ Cabal file providing a detailed description of this package.
  tarballPath :: FilePath,
  -- ^ Local path to the tarball containing the source code of this package.
  -- 
  -- Invariant: this filepath should __/always/__ point to a valid tarball of this package.
  exposedModules :: Maybe PackageComponent,
  -- ^ Exposed module list if this package exposes a library.
  dbId :: Maybe RowID,
  -- ^ Index of this package in ex-hack's database.
  --
  -- Nothing if the package has not been inserted yet in the database.
  allComponents :: [PackageComponent]
  -- ^ Components of the package, including the one not exporting any symbols.
  --
  --   These modules are used as corpus during the symbol occurence search.
} deriving (Eq, Show)

-- | Local directory containing the downloaded tarballs.
newtype TarballsDir = TarballsDir FilePath deriving (Eq, Show)

-- | Local directory containing the downloaded cabal files.
newtype CabalFilesDir = CabalFilesDir FilePath deriving (Eq, Show)

-- | Local directory in which the tarballs are extracted and the 
--   packages built.
newtype WorkDir = WorkDir FilePath deriving (Eq, Show)

-- | Local directory in which the HTML examples database is generated.
newtype HtmlDir = HtmlDir FilePath deriving (Eq, Show)

-- | Algebraic data type representing the database status.
--
--   This type should be used to parametrize the `DatabaseHandle`
--   dataype using the DataKinds GHC extension.
data DatabaseStatus = New | Initialized | DepsGraph | PkgExports | IndexedSyms

-- | Database file handle wrapper. 
--
--   Allows us to bring back a tiny bit of type safety to the database interactions.
newtype DatabaseHandle (a :: DatabaseStatus) = DatabaseHandle FilePath
    deriving (Eq, Show)

-- | Used in conjunction with `DatabaseHandle` and `getDatabaseHandle`, this type family allow us 
--   to materialize the application of a database operation.
--
--   This is obviously a poor way to design this kind of interaction, but
--   this is the best effort/refactoring cost ratio I could find.
--
--   TODO: delete this family, move the database-related types to the database module
--   and materialize the database state evolution directly in the database operations functions.
type family AlterDatabase (a :: DatabaseStatus) :: DatabaseStatus where
    AlterDatabase 'New         = 'Initialized
    AlterDatabase 'Initialized = 'DepsGraph
    AlterDatabase 'DepsGraph   = 'PkgExports
    AlterDatabase 'PkgExports  = 'IndexedSyms

-- | Extract the database file filepath from the `DatabaseHandle` and assume
--   an operation will be performed on this handle thus steps the handle's
--   `DatabaseStatus`.
getDatabaseHandle :: DatabaseHandle (a :: DatabaseStatus) -> (FilePath, DatabaseHandle (AlterDatabase a))
getDatabaseHandle (DatabaseHandle fp) = (fp, DatabaseHandle fp)


-- | Generates a new database handle given the `FilePath` pointing to the ex-hack
--   database.
newDatabaseHandle :: FilePath -> DatabaseHandle 'New
newDatabaseHandle = DatabaseHandle

-- | YAML file describing a stackage release.
newtype StackageFile = StackageFile FilePath deriving (Eq, Show)

-- | ExHack general configuration.
data Config a = Config {
    _dbHandle      :: DatabaseHandle a,
    _stackageFile  :: StackageFile,
    _tarballsDir   :: TarballsDir,
    _cabalFilesDir :: CabalFilesDir,
    _workDir       :: WorkDir,
    _htmlDir       :: HtmlDir,
    _createDirs    :: Bool
} deriving (Eq, Show)

makeLenses ''Config

instance Has (Config 'New) (DatabaseHandle 'New) where
    hasLens = dbHandle

instance Has (Config 'Initialized) (DatabaseHandle 'Initialized) where
    hasLens = dbHandle

instance Has (Config 'DepsGraph) (DatabaseHandle 'DepsGraph) where
    hasLens = dbHandle

instance Has (Config 'PkgExports) (DatabaseHandle 'PkgExports) where
    hasLens = dbHandle

instance Has (Config 'IndexedSyms) (DatabaseHandle 'IndexedSyms) where
    hasLens = dbHandle

instance Has (Config a) StackageFile where
    hasLens = stackageFile

instance Has (Config a) TarballsDir where
    hasLens = tarballsDir

instance Has (Config a) CabalFilesDir where
    hasLens = cabalFilesDir

instance Has (Config a) WorkDir where
    hasLens = workDir

instance Has (Config a) HtmlDir where
    hasLens = htmlDir

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

newtype PackageNameT = PackageNameT Text
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

-- | Symbol having been unified.
--
--   Meaning we linked a `LocatedSym` back to an exported `IndexedSym`.
newtype UnifiedSym = UnifiedSym (IndexedSym, LocatedSym)
    deriving (Eq)

-- | Source code of a module in which we found some occurences
--   of an `IndexedSym`.
data SourceCodeFile = SourceCodeFile !Text !ModuleNameT !PackageNameT


class (Monad m) => MonadLog m where
    logInfo, logError, logInfoTitle :: Text -> m ()
    logInfoProgress :: Int -> Int -> Int -> Text -> m ()
    logInfoProgress stepNb tot cur t = logInfo $ "[Step " 
        <> pack (show stepNb) <> "][" <> pack (show cur) <> "/" 
        <> pack (show tot) <> "] " <> t
    logInfoTitle txt = line >> logInfo ("* " <> txt <> " *") >> line
        where 
            line :: m ()
            !line = logInfo (replicate (length txt + 4) "*")

instance MonadLog IO where
    logInfo t = setSGR [SetColor Foreground Vivid Blue] >> TIO.putStrLn t >> setSGR [Reset]
    logError t = setSGR [SetColor Foreground Vivid Red] >> TIO.hPutStrLn stderr t >> setSGR [Reset]

instance MonadLog (Step c s)  where
    logInfo t = liftIO $ do
        setSGR [SetColor Foreground Vivid Blue]
        TIO.putStrLn t
        setSGR [Reset]
    logError t = liftIO $ do
        setSGR [SetColor Foreground Vivid Red] 
        TIO.hPutStrLn stderr t 
        setSGR [Reset]

instance MonadLog SeldaM where
    logInfo t = liftIO $ do
        setSGR [SetColor Foreground Vivid Blue] 
        TIO.putStrLn t 
        setSGR [Reset]
    logError t = liftIO $ do 
        setSGR [SetColor Foreground Vivid Red] 
        TIO.hPutStrLn stderr t 
        setSGR [Reset]

type MonadStep c m = (MonadIO m, MonadMask m, MonadReader c m, MonadLog m)

-- | Processing step monad realization. Used to encapsulate a processing step
--   together with an application `Config`.
--
--   This is basically a RIO monad.
newtype Step c s a = Step (ReaderT (Config s) IO a)
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadReader (Config s), MonadCatch, MonadThrow, MonadMask)

-- | Runs a processing `Step`.
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

-- | Symbols imported in a module file.
--
--   This datastructure is about optimizing the lookups, allowing us to
--   release a bit of database pressure.
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

getPackageNameT :: Package -> PackageNameT
getPackageNameT p = PackageNameT (getName p)

-- Exceptions
-- =================

-- | Exception throwned by the GHC-API code when it cannot locate a
--   module source file.
data PackageLoadError = CannotFindModuleFile ModuleName [ComponentRoot]
    deriving (Show)

instance Exception PackageLoadError

-- | Exception throwned when cabal is unable to build a package.
data CabalBuildError = CabalBuildError Int String
    deriving (Show)

instance Exception CabalBuildError
