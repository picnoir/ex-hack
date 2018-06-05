{-# LANGUAGE OverloadedStrings #-}
module ExHack.Cabal.CabalParser (
  parseCabalFile,
  getSuccParse,
  ParseResult(..)
) where

import Data.Text (Text, pack, unpack)
import Data.Maybe 
import Data.Set (Set, fromList)
import qualified Data.Set as S (filter)
import Data.Monoid ((<>))
import Distribution.Types.PackageName
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Types.Dependency

import ExHack.Types

getSuccParse :: [ParseResult Package] -> [Package]
getSuccParse = foldr appendParseResult [] 
    where
      appendParseResult (ParseFailed _) xs = xs
      appendParseResult (ParseOk _ a) xs = a:xs

parseCabalFile :: String -> Text -> ParseResult Package
parseCabalFile tarballsDir cstr = Package <$> packN <*> filteredPackDep <*> tPath <*> pure (unpack cstr)
    where
      gpackageDesc = parseGenericPackageDescription (unpack cstr)
      packN = package .  packageDescription <$> gpackageDesc
--    We want deps for both the app and the potential libs.
--    The following code is messy as hell but necessary. Deps are quite heavily burried
--    in Cabal's packages data structures...
--
--    I made types explicits to document a bit this black magic.
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
      tPath = fmap (\n -> pack tarballsDir <> (pack . unPackageName $ pkgName n) <> "tar.gz") packN
