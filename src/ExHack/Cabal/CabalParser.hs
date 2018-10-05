{-|
Module      : ExHack.Cabal.CabalParser
Description : Cabal files parsers collection.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns #-}
module ExHack.Cabal.CabalParser (
  parseCabalFile
) where

import Control.DeepSeq (force)
import           Data.Maybe                                   (fromMaybe,
                                                               maybeToList)
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
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription,
                                                               condBenchmarks,
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
parseCabalFile :: TarballDesc -> Maybe Package
parseCabalFile (TarballDesc (tp, cf)) = force $ extractPack <$> gpackageDesc
  where
    gpackageDesc :: Maybe GenericPackageDescription
    !gpackageDesc = parseResultToMaybe . parseGenericPackageDescription $ encodeUtf8 cf
    parseResultToMaybe :: ParseResult GenericPackageDescription -> Maybe GenericPackageDescription
    parseResultToMaybe !pr = 
        let !r = runParseResult pr in 
        case r of
            (_, Left _)   -> Nothing
            (_, Right x)  -> Just x
    extractPack :: GenericPackageDescription -> Package
    extractPack !gp = Package packN filteredPackDep cf tp expMainLibMods Nothing allMods
        where
            !packN = package .  packageDescription $ gp
            --  The package should not be a dependancy to itself.
            !filteredPackDep = S.filter (/= pkgName packN) packDeps
            packDeps :: Set PackageName
            !packDeps = fromList (depPkgName <$> allDeps)
            allDeps :: [Dependency]
            !allDeps = mainLibDep <> subLibDep <> execDep <> testDep <> benchDep
            mainLibDep :: [Dependency]
            !mainLibDep = treeToDep (maybeToList . condLibrary) gp 
            !subLibDep = fromMaybe [] $ treeToDep $ getTree condSubLibraries
            !execDep = fromMaybe [] $ treeToDep $ getTree condExecutables
            !testDep = fromMaybe [] $ treeToDep $ getTree condTestSuites
            !benchDep = fromMaybe [] $ treeToDep $ getTree condBenchmarks
        --  We want deps for both the app and the potential libs.
        --  The following code is messy as hell but necessary. Deps are quite heavily burried
        --  in Cabal's packages data structures...
        --
        --  I made types explicits to document a bit this black magic.
            allMods :: [PackageComponent]
            !allMods = (testToPackageComponent <$> tMods) <> 
                (libToPackageComponentInternal <$> lMods) <>
                (benchToPackageComponent <$> bMods) <>
                (exeToPackageComponent <$> execMods) <>
                maybeToList allMainLibMods
            expMainLibMods :: Maybe PackageComponent
            !expMainLibMods = libToPackageComponent . condTreeData <$> condLibrary gp
            allMainLibMods :: Maybe PackageComponent
            !allMainLibMods = libToPackageComponentInternal . condTreeData <$> condLibrary gp 
            tMods :: [TestSuite]
            !tMods = condTreeData . snd <$> condTestSuites gp
            bMods :: [Benchmark]
            !bMods = condTreeData . snd <$> condBenchmarks gp
            lMods :: [Library]
            !lMods = condTreeData . snd <$> condSubLibraries gp
            execMods :: [Executable]
            !execMods = condTreeData . snd <$> condExecutables gp
            -- Helper functions
            -- ================
            getTree st = (fmap . fmap) snd (st <$> gpackageDesc)
            treeToDep t = concat <$> (fmap . fmap) condTreeConstraints t

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
