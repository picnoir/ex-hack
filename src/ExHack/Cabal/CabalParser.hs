module ExHack.Cabal.CabalParser (
  parseCabalFile,
  getSuccParse,
  runParseResult
) where

import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (maybeToList)
import Data.Set (Set, fromList)
import qualified Data.Set as S (filter)
import Distribution.Types.CondTree (condTreeConstraints, condTreeData)
import Distribution.Types.GenericPackageDescription (condExecutables, condSubLibraries,
                                                     condTestSuites, condBenchmarks,
                                                     condLibrary, packageDescription)
import qualified Distribution.Types.Library as Lib (exposedModules)                                                     
import Distribution.Types.PackageDescription (package)
import Distribution.PackageDescription.Parsec (ParseResult, runParseResult,
                                               parseGenericPackageDescription)
import Distribution.Types.Dependency (Dependency, depPkgName) 

import ExHack.Types (Package(..), UnparsedPackage(..), PackageName,
                     ModuleName(..), pkgName)

getSuccParse :: [ParseResult Package] -> [Package]
getSuccParse = foldr appendParseResult [] 
    where
      appendParseResult pr xs = case runParseResult pr of
                                    (_, Left _) -> xs
                                    (_, Right x) -> x:xs

parseCabalFile :: UnparsedPackage -> ParseResult Package
parseCabalFile (UnparsedPackage (tp, cf, hf)) = Package <$> packN <*> filteredPackDep <*> pure cf <*> pure tp <*> pure hf  <*> expMods 
    where
      gpackageDesc = parseGenericPackageDescription $ encodeUtf8 cf
      packN = package .  packageDescription <$> gpackageDesc
--    We want deps for both the app and the potential libs.
--    The following code is messy as hell but necessary. Deps are quite heavily burried
--    in Cabal's packages data structures...
--
--    I made types explicits to document a bit this black magic.
      expMods :: ParseResult (Maybe [ModuleName])
      expMods = (Lib.exposedModules . condTreeData) <$$> condLibrary <$> gpackageDesc
      packDeps :: ParseResult (Set PackageName)
      packDeps = fromList <$> (fmap . fmap) depPkgName allDeps
--    The package should not be a dependancy to itself.
      filteredPackDep = do
        pd <- packDeps 
        pn <- pkgName <$> packN
        return $ S.filter (/= pn) pd
      allDeps :: ParseResult [Dependency]
      allDeps = mainLibDep `prApp` subLibDep `prApp` execDep `prApp` testDep `prApp` benchDep
      mainLibDep :: ParseResult [Dependency]
      mainLibDep = treeToDep (maybeToList . condLibrary) <$> gpackageDesc
      subLibDep = treeToDep $ getTree condSubLibraries
      execDep = treeToDep $ getTree condExecutables
      testDep = treeToDep $ getTree condTestSuites
      benchDep = treeToDep $ getTree condBenchmarks
      -- Helper functions
      -- ================
      getTree st = (fmap . fmap) snd (st <$> gpackageDesc)
      treeToDep t = concat <$> (fmap . fmap) condTreeConstraints t
      prApp :: ParseResult [a] -> ParseResult [a] -> ParseResult [a]
      prApp a b = (++) <$> a <*> b
      (<$$>) = fmap . fmap
