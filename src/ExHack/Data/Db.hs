{-|
Module      : ExHack.Data.Db
Description : Database-related operations.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module ExHack.Data.Db (
    getHomePagePackages,
    getModulePageSyms,
    getPackagePageMods,
    getPkgImportScopes,
    initDb,
    saveModuleExports,
    saveModuleUnifiedSymbols,
    savePackageDeps,
    savePackageMods,
    savePackages
) where

import           Control.Monad          (unless)
import           Control.Monad.Catch    (Exception, MonadMask, throwM)
import qualified Data.HashMap.Strict    as HM (fromList)
import qualified Data.HashSet           as HS (HashSet, fromList)
import           Data.Maybe             (listToMaybe, maybe)
import           Data.Text              (Text, pack)
import qualified Data.Text              as T (lines, unlines)
import           Database.Selda         ((:*:) (..), RowID, Selector, Table,
                                         aggregate, autoPrimary, count, def, fk,
                                         fromRowId, fromSql, groupBy, innerJoin,
                                         insertWithPK, insert_, literal, query,
                                         required, restrict, select,
                                         tableWithSelectors, text,
                                         tryCreateTable, (!), (.==))
import           Database.Selda.Backend (MonadSelda (..), SqlValue (SqlInt))
import           GHC                    (SrcSpan (..), getLoc, srcSpanStartCol,
                                         srcSpanStartLine)

import           ExHack.Renderer.Types  (HomePagePackage (..), ModuleName (..),
                                         PackageName (..), SymbolName,
                                         SymbolOccurs (..))
import           ExHack.Types           (ImportsScope, IndexedModuleNameT (..),
                                         IndexedSym (..), LocatedSym (..),
                                         ModuleNameT (..), PackageExports (..),
                                         PackageNameT (..), SourceCodeFile (..),
                                         SourceCodeFile (..), SymName (..),
                                         UnifiedSym (..), depsNames, getModName,
                                         getName)
import qualified ExHack.Types           as ET (Package (..))

packageId   :: Selector (RowID :*: Text :*: Text :*: Text) RowID
packageName :: Selector (RowID :*: Text :*: Text :*: Text) Text
packages    ::  Table (RowID :*: Text :*: Text :*: Text)
(packages, packageId :*: packageName :*: _ :*: _) 
  = tableWithSelectors "packages" $
              autoPrimary "packageId"
              :*: required "name"
              :*: required "tarball_path"
              :*: required "cabal_file"

dependancies :: Table (RowID :*: RowID :*: RowID)
depPack :: Selector (RowID :*: RowID :*: RowID) RowID
depId :: Selector (RowID :*: RowID :*: RowID) RowID
(dependancies, _ :*: depPack :*: depId) = tableWithSelectors "dependancies" $
                   autoPrimary "id"
                   :*: required "packID" `fk` (packages, packageId)
                   :*: required "depID" `fk` (packages, packageId)

exposedModules :: Table (RowID :*: Text :*: RowID)
modId          :: Selector (RowID :*: Text :*: RowID) RowID
modName        :: Selector (RowID :*: Text :*: RowID) Text
modPack        :: Selector (RowID :*: Text :*: RowID) RowID
(exposedModules, modId :*: modName :*: modPack) = tableWithSelectors "exposedModules" $
                   autoPrimary "id"
                   :*: required "name"
                   :*: required "packID" `fk` (packages, packageId)

exposedSymbols :: Table (RowID :*: Text :*: RowID)
symId          :: Selector (RowID :*: Text :*: RowID) RowID
symName        :: Selector (RowID :*: Text :*: RowID) Text
symModId       :: Selector (RowID :*: Text :*: RowID) RowID
(exposedSymbols, symId :*: symName :*: symModId) = tableWithSelectors "exposedSymbols" $
                   autoPrimary "id"
                   :*: required "name"
                   :*: required "modId" `fk` (exposedModules, modId)

sourceFiles :: Table (RowID :*: Text :*: Text :*: Text)
fileId      :: Selector (RowID :*: Text :*: Text :*: Text) RowID
fileContent :: Selector (RowID :*: Text :*: Text :*: Text) Text
fileModule  :: Selector (RowID :*: Text :*: Text :*: Text) Text
filePackage :: Selector (RowID :*: Text :*: Text :*: Text) Text
(sourceFiles, fileId :*: fileContent :*: fileModule :*: filePackage)
  = tableWithSelectors "sourceFiles" $
    autoPrimary "id"
    :*: required "fileContent"
    :*: required "modName"
    :*: required "packName"

symbolOccurences :: Table    (RowID :*: Int :*: Int :*: RowID :*: RowID)
occCol           :: Selector (RowID :*: Int :*: Int :*: RowID :*: RowID) Int
occLine          :: Selector (RowID :*: Int :*: Int :*: RowID :*: RowID) Int
occFileId        :: Selector (RowID :*: Int :*: Int :*: RowID :*: RowID) RowID
occSymId         :: Selector (RowID :*: Int :*: Int :*: RowID :*: RowID) RowID
(symbolOccurences, _ :*: occCol :*: occLine :*: occFileId :*: occSymId) 
  = tableWithSelectors "symbolOccurences" $
    autoPrimary "id"
    :*: required "column"
    :*: required "line"
    :*: required "sourceFileId" `fk` (sourceFiles, fileId)
    :*: required "importedSymID" `fk` (exposedSymbols, symId)


-- | Create the internal database schema.
initDb :: (MonadSelda m) => m ()
initDb = do
    tryCreateTable packages 
    tryCreateTable dependancies 
    tryCreateTable exposedModules
    tryCreateTable exposedSymbols
    tryCreateTable symbolOccurences
    tryCreateTable sourceFiles

-- | Save a package dependancies.
--
-- Note that if we can't find a dependancy in the
-- packages table, we'll ignore it.
--
-- You should make sure your package database is already
-- populated before using this.
savePackageDeps :: (MonadSelda m) => ET.Package -> m ()
savePackageDeps p = do
    mpid <- queryPkg p
    let resPackDeps = depsNames p 
    mapM_ (\rowId -> saveDep rowId `mapM_` resPackDeps) mpid
  where
    saveDep pid d = do
      mdid <- query $ do 
        pks <- select packages
        restrict (pks ! packageName .== text (pack d))
        return $ pks ! packageId
      mapM_ (\did -> insert_ dependancies [ def :*: pid :*: did]) (listToMaybe mdid)

-- | Save a package list in the DB.
savePackages :: (MonadSelda m) => [ET.Package] -> m ()
savePackages xs = insert_ packages $
    (\p -> def :*: getName p :*: ET.cabalFile p :*: (pack . ET.tarballPath) p) <$> xs 

data SaveModuleException = PackageNotInDatabase | ModuleNotInDatabase Text 
    deriving (Show)

instance Exception SaveModuleException

-- | Potentially confusing:
--   * If we have a package id in the Package type, use it
--   * Otherwise retrieve the package id from the DB
--   * If the package is not in the DB, something weird happened...
--     Throw an error
getPackageId :: forall m. (MonadSelda m, MonadMask m)
             => ET.Package -> m RowID
getPackageId p = maybe
    (queryPkg p >>= maybe (throwM PackageNotInDatabase) pure)
    pure
    (ET.dbId p)

-- | Save the exposed modules as well as their exposed symbols.
savePackageMods :: forall m. (MonadSelda m, MonadMask m) 
                => PackageExports -> m ()
savePackageMods (PackageExports (p, _, xs)) = do
    pid <- getPackageId p
    saveMod pid `mapM_` xs
  where
    saveMod pid (m, syms) = do
        mid <- insertWithPK exposedModules [def :*: getModName m :*: pid]
        insert_  exposedSymbols $ (\(SymName sn) -> def :*: sn :*: mid) <$> syms

-- | Given a module database ID, saves the exported symbols of this
--   module in ExHack's database.
saveModuleExports :: (MonadSelda m) => Int -> [SymName] -> m ()
saveModuleExports midi xs = insert_ exposedSymbols $ 
    (\(SymName s) -> def :*: s :*: fromSql (SqlInt midi)) <$> xs

queryPkg :: (MonadSelda m) => ET.Package -> m (Maybe RowID)
queryPkg p = do
    let r = query $ do
            pks <- select packages
            restrict (pks ! packageName .== (text . getName) p)
            return $ pks ! packageId 
    listToMaybe <$> r

-- | Query ExHack database to retrieve the available symbols to be imported
--   from within this package.
--
--   This scope should be filtered on a per-module basis, depending on the module
--   imports, before being used in a symbol unification process.
getPkgImportScopes :: forall m. (MonadSelda m, MonadMask m) => ET.Package -> m ImportsScope
getPkgImportScopes p = do
    mods <- getScopeModules p
    o <- sequence (wrapSyms <$> mods)
    pure $ HM.fromList o
  where
    wrapSyms :: IndexedModuleNameT -> m (IndexedModuleNameT, HS.HashSet IndexedSym)
    wrapSyms mnt@(IndexedModuleNameT (_, i)) = do
        let mid = fromSql $ SqlInt i :: RowID
        q <- query $ do
            mods <- select exposedModules 
            restrict (mods ! modId .== literal mid)
            syms <- innerJoin (\s -> s ! symModId .== mods ! modId) $ select exposedSymbols
            pure $ syms ! symId :*: syms ! symName
        pure (mnt, HS.fromList (wrapResult <$> q)) 
    wrapResult (i :*: n) = IndexedSym (SymName n, fromRowId i)

getScopeModules :: (MonadSelda m, MonadMask m) => ET.Package -> m [IndexedModuleNameT]
getScopeModules p = do
    pid <- getPackageId p
    q <- query $ do
        deps <- select dependancies
        restrict (deps ! depPack .== literal pid)
        mods <- innerJoin (\m -> m ! modPack .== deps ! depId) $ select exposedModules
        return (mods ! modId :*: mods ! modName)
    -- Here, we also want to look for occurences in current's package module.
    -- Not sure if it's a really good idea: we'll find occurences for sure, but we also
    -- probably consider the symbol definition as an occurence...
    qp <- query $ do
        mods <- select exposedModules
        restrict $ (mods ! modPack .== literal pid)
        return (mods ! modId :*: mods ! modName)
    pure $ (wrapResult <$> q) <> (wrapResult <$> qp) 
  where
    wrapResult (i :*: n) = IndexedModuleNameT (ModuleNameT n, fromRowId i)


-- | Insert both the source file in which some symbols have been unified as well as 
--   the symbols occurences in ExHack's database.
saveModuleUnifiedSymbols :: forall m. (MonadSelda m, MonadMask m) => [UnifiedSym] -> SourceCodeFile -> m ()
saveModuleUnifiedSymbols xs (SourceCodeFile f (ModuleNameT mnt) (PackageNameT pnt)) = 
    unless (null xs) $ do
        fid <- insertWithPK sourceFiles [def :*:  f :*: mnt :*: pnt]
        insert_ symbolOccurences $ generateLine fid <$> xs
  where
      generateLine fid (UnifiedSym (IndexedSym (_, sidi), LocatedSym (_, _, gloc))) = 
          def :*: col :*: line :*: fid :*: sid 
        where
          (RealSrcSpan loc) = getLoc gloc
          !line = srcSpanStartLine loc
          !col = srcSpanStartCol loc
          !sid = fromSql (SqlInt sidi)

-- | Retrieve the data necessary to render the HTML home page.
getHomePagePackages :: forall m. (MonadSelda m, MonadMask m) => m [HomePagePackage]
getHomePagePackages = do 
    res <- query $ aggregate $ do
        pkgs <- select packages
        mods <- innerJoin (\m -> pkgs ! packageId .== m ! modPack) $ select exposedModules
        pid <- groupBy (pkgs ! packageId) 
        pn <- groupBy (pkgs ! packageName)
        pure $ pid :*: pn :*: count (mods ! modId)   
    pure $ wrapResult <$> res 
  where
    wrapResult (i :*: n :*: c) = HomePagePackage (PackageName (i,n)) c

-- | Retrieve the data necessary to render the HTML package page.
getPackagePageMods :: forall m. (MonadSelda m, MonadMask m) => PackageName -> m [ModuleName]
getPackagePageMods (PackageName (pid, _)) = do
    res <- query $ do
        pkgs <- select packages
        restrict $ pkgs ! packageId .== literal pid 
        mods <- innerJoin (\m -> pkgs ! packageId .== m ! modPack) $ select exposedModules
        pure $ mods ! modId :*: mods ! modName
    pure $ wrapResult <$> res
  where
    wrapResult (i :*: n) = ModuleName (i,n)

-- | Retrieve the data necessary to render the HTML module page.
getModulePageSyms :: forall m. (MonadSelda m, MonadMask m) => PackageName -> ModuleName -> m [SymbolOccurs]
getModulePageSyms _ (ModuleName (mid,_)) = do
    sids <- query $ do
        syms <- select exposedSymbols
        restrict $ syms ! symModId .== literal mid
        pure $ syms ! symId :*: syms ! symName
    mapM (\(sid :*: sn) -> wrapResult sn <$> querySym sid) sids
  where
    querySym :: RowID -> m [Int :*: Int :*: Text :*: Text :*: Text]
    querySym sid = query $ do
        syms <- select exposedSymbols
        restrict $ syms ! symId .== literal sid 
        occs  <- innerJoin (\o -> o ! occSymId .== syms ! symId) $ select symbolOccurences
        files <- innerJoin (\f -> f ! fileId .== occs ! occFileId) $ select sourceFiles
        pure $ (occs ! occCol) :*: (occs ! occLine) :*: 
               (files ! fileContent) :*: (files ! fileModule) :*:
               (files ! filePackage)
    wrapResult :: SymbolName -> [Int :*: Int :*: Text :*: Text :*: Text] -> SymbolOccurs
    wrapResult sname occs = SymbolOccurs sname (wrapOcc occs)
    wrapOcc = fmap 
                (\(col :*: line :*: content :*: mname :*: pname) -> 
                    let (nLine, nContent) = extractSample line content
                     in (col, nLine, 
                         SourceCodeFile nContent
                                       (ModuleNameT mname) 
                                       (PackageNameT pname)))

-- Ahum, not typesafe at all. TODO: create sample-associated datatypes.
extractSample :: Int -> Text -> (Int, Text)
extractSample line t = (nLine, T.unlines nText)
    where
       !tLines = T.lines t
       linesBefore = 15
       linesAfter = 5
       -- Nb lines to ignore.
       !toIgnore = max 0 (line - linesBefore)
       -- Intermediate length, ie init length - ignored lines.
       !iLength = length tLines - toIgnore
       -- New line number.
       !nLine  = line - toIgnore
       -- Nb lines to take
       !toTake = min (nLine + linesAfter) iLength
       !nText  = take toTake $ drop toIgnore tLines
