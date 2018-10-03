{-|
Module      : ExHack.Cabal.CabalParser
Description : Cabal files parsers collection.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
module ExHack.Cabal.CabalParser (
  parseCabalFile,
  runParseResult
) where

import           Data.Maybe                                   (maybeToList)
import           Data.Set                                     (Set, fromList)
import qualified Data.Set                                     as S (filter)
import           Data.Text.Encoding                           (encodeUtf8)
import           Distribution.PackageDescription.Parsec       (ParseResult, parseGenericPackageDescription,
                                                               runParseResult)
import           Distribution.Types.Benchmark                 (Benchmark, benchmarkBuildInfo,
                                                               benchmarkModules)
import           Distribution.Types.BuildInfo                 (hsSourceDirs)
import           Distribution.Types.CondTree                  (condTreeConstraints,
                                                               condTreeData)
import           Distribution.Types.Dependency                (Dependency,
                                                               depPkgName)
import           Distribution.Types.Executable                (Executable,
                                                               buildInfo,
                                                               exeModules)
import           Distribution.Types.GenericPackageDescription (condBenchmarks,
                                                               condExecutables,
                                                               condLibrary,
                                                               condSubLibraries,
                                                               condTestSuites,
                                                               packageDescription)
import           Distribution.Types.Library                   (Library,
                                                               libBuildInfo)
import qualified Distribution.Types.Library                   as Lib (explicitLibModules,
                                                                      exposedModules)
import           Distribution.Types.PackageDescription        (package)
import           Distribution.Types.TestSuite                 (TestSuite,
                                                               testBuildInfo,
                                                               testModules)

import           ExHack.Types                                 (ComponentRoot (..),
                                                               Package (..),
                                                               PackageComponent (..),
                                                               PackageName,
                                                               TarballDesc (..),
                                                               pkgName)

-- | Parse a cabalFile into a `Package`
--
-- TODO: some benchs and test suites are not exposing any modules but instead
--       are directly exposing a single .hs file. It's a bit too tricky to implement
--       for V1, but we probably should find a way to list those files later on.
parseCabalFile :: TarballDesc -> ParseResult Package
parseCabalFile (TarballDesc (tp, cf)) = 
    Package <$> packN <*> filteredPackDep <*> pure cf <*> pure tp <*> expMainLibMods <*> pure Nothing <*> allMods
  where
    !gpackageDesc = parseGenericPackageDescription $ encodeUtf8 cf
    !packN = package .  packageDescription <$> gpackageDesc
--  We want deps for both the app and the potential libs.
--  The following code is messy as hell but necessary. Deps are quite heavily burried
--  in Cabal's packages data structures...
--
--  I made types explicits to document a bit this black magic.
    allMods :: ParseResult [PackageComponent]
    !allMods = (testToPackageComponent <$$> tMods) <++> 
        (libToPackageComponentInternal <$$> lMods) <++>
        (benchToPackageComponent <$$> bMods) <++>
        (exeToPackageComponent <$$> execMods) <++>
        (maybeToList <$> allMainLibMods)
    expMainLibMods :: ParseResult (Maybe PackageComponent)
    !expMainLibMods = (libToPackageComponent . condTreeData) <$$> (condLibrary <$> gpackageDesc)
    allMainLibMods :: ParseResult (Maybe PackageComponent)
    !allMainLibMods = (libToPackageComponentInternal . condTreeData) <$$> (condLibrary <$> gpackageDesc) 
    tMods :: ParseResult [TestSuite]
    !tMods = (condTreeData . snd) <$$> condTestSuites <$> gpackageDesc
    bMods :: ParseResult [Benchmark]
    !bMods = (condTreeData . snd) <$$> condBenchmarks <$> gpackageDesc
    lMods :: ParseResult [Library]
    !lMods = (condTreeData . snd) <$$> condSubLibraries <$> gpackageDesc
    execMods :: ParseResult [Executable]
    !execMods = (condTreeData . snd) <$$> condExecutables <$> gpackageDesc
    packDeps :: ParseResult (Set PackageName)
    !packDeps = fromList <$> (fmap . fmap) depPkgName allDeps
--  The package should not be a dependancy to itself.
    !filteredPackDep = do
      pd <- packDeps 
      pn <- pkgName <$> packN
      return $ S.filter (/= pn) pd
    allDeps :: ParseResult [Dependency]
    !allDeps = mainLibDep `prApp` subLibDep `prApp` execDep `prApp` testDep `prApp` benchDep
    mainLibDep :: ParseResult [Dependency]
    !mainLibDep = treeToDep (maybeToList . condLibrary) <$> gpackageDesc
    !subLibDep = treeToDep $ getTree condSubLibraries
    !execDep = treeToDep $ getTree condExecutables
    !testDep = treeToDep $ getTree condTestSuites
    !benchDep = treeToDep $ getTree condBenchmarks
    -- Helper functions
    -- ================
    getTree st = (fmap . fmap) snd (st <$> gpackageDesc)
    treeToDep t = concat <$> (fmap . fmap) condTreeConstraints t
    prApp :: ParseResult [a] -> ParseResult [a] -> ParseResult [a]
    prApp a b = (++) <$> a <*> b
    (<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b) 
    (<$$>) = fmap . fmap
    -- Lifted list constructor
    (<++>) :: ParseResult [a] -> ParseResult [a] -> ParseResult [a]
    (<++>) a b= (<>) <$> a <*> b
    infixr 5 <++>

-- | Do not return internal modules.
libToPackageComponent :: Library -> PackageComponent
libToPackageComponent lib = PackageComponent (Lib.exposedModules lib) 
                                             (ComponentRoot <$> hsSourceDirs (libBuildInfo lib))

-- | Return both exposed and internal modules.
libToPackageComponentInternal :: Library -> PackageComponent
libToPackageComponentInternal lib = PackageComponent (Lib.explicitLibModules lib) 
                                             (ComponentRoot <$> hsSourceDirs (libBuildInfo lib))

exeToPackageComponent :: Executable -> PackageComponent
exeToPackageComponent exe = PackageComponent (exeModules exe) 
                                             (ComponentRoot <$> hsSourceDirs (buildInfo exe)) 

testToPackageComponent :: TestSuite -> PackageComponent
testToPackageComponent t = PackageComponent (testModules t) 
                                            (ComponentRoot <$> hsSourceDirs (testBuildInfo t))

benchToPackageComponent :: Benchmark -> PackageComponent
benchToPackageComponent b = PackageComponent (benchmarkModules b) 
                                             (ComponentRoot <$> hsSourceDirs (benchmarkBuildInfo b))
