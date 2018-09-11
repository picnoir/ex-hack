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

module ExHack.Types (
    Config(..),
    ComponentRoot(..),
    PackageComponent(..),
    Package(..),
    PackageLoadError(..),
    PackageIdentifier(..),
    PackageFilePath(..),
    PackageName,
    PackageNameT(..),
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
    SourceCodeFile(..),
    tarballsDir,
    cabalFilesDir,
    workDir,
    runStep,
    mkPackageName,
    mkVersion,
    getName,
    getModName,
    getModNameT,
    getPackageNameT,
    depsNames,
    packagedlDescName,
    fromComponents,
    parseConfig,
    newDatabaseHandle
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
import           Data.Yaml                      (FromJSON (..),
                                                 ParseException (..),
                                                 decodeFileEither, withObject,
                                                 (.:))
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


newtype TarballsDir = TarballsDir FilePath deriving (Eq, Show)
newtype CabalFilesDir = CabalFilesDir FilePath deriving (Eq, Show)
newtype WorkDir = WorkDir FilePath deriving (Eq, Show)

data DatabaseStatus = New | Initialized | DepsGraph | PkgExports
type DatabaseHandle (a :: DatabaseStatus) = FilePath

newDatabaseHandle :: FilePath -> DatabaseHandle 'New
newDatabaseHandle = id

newtype StackageFile = StackageFile Text deriving (Eq, Show)

data Config a = Config {
    _dbHandle :: DatabaseHandle a,
    _stackageFile :: StackageFile,
    _tarballsDir :: TarballsDir,
    _cabalFilesDir :: CabalFilesDir,
    _workDir :: WorkDir
} deriving (Eq, Show)

instance FromJSON (Config 'New) where
    parseJSON = withObject "config" $ \v -> Config 
        <$> (newDatabaseHandle <$> (v .: "db-file"))
        <*> (StackageFile <$> v .: "stackage-file")
        <*> (TarballsDir <$> v .: "tarballs-dir")
        <*> (CabalFilesDir <$> v .: "cabal-files-dir")
        <*> (WorkDir <$> v .: "work-dir")

parseConfig :: FilePath -> IO (Either ParseException (Config 'New))
parseConfig = decodeFileEither

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

newtype UnifiedSym = UnifiedSym (IndexedSym, LocatedSym)
    deriving (Eq)

data SourceCodeFile = SourceCodeFile Text ModuleNameT PackageNameT

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

-- | Symbols imported in a module file.
--
--   This datastructure is optimizing the lookups, allowing 
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

data PackageLoadError = CannotFindModuleFile ModuleName [ComponentRoot]
    deriving (Show)
instance Exception PackageLoadError
