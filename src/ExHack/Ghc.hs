{-# LANGUAGE OverloadedStrings #-}
module ExHack.Ghc (
  UnitId(..),
  TypecheckedSource,
  DesugaredModule(..),
  getDesugaredMod,
  getModUnitId,
  getModName,
  getModExports,
  getModImports,
  getModSymbols,
  getContent,
  ) where

import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (pack)
import Distribution.ModuleName (ModuleName, components, toFilePath)
import qualified Distribution.Helper as H (mkQueryEnv, runQuery, 
                                           ghcOptions, components, 
                                           ChComponentName(ChLibName))
import Safe (headMay)
import System.FilePath((</>))
-- GHC modules
import GHC (runGhc, getSessionDynFlags,
            setSessionDynFlags, guessTarget,
            setTargets, load, LoadHowMuch(..),
            getModSummary, mkModuleName,
            parseModule, typecheckModule,
            desugarModule, DesugaredModule,
            moduleUnitId, dm_core_module,
            moduleNameString, moduleName,
            dm_typechecked_module, tm_typechecked_source,
            parseDynamicFlags, noLoc, TypecheckedSource,
            ModSummary, Ghc, unLoc, ms_textual_imps, 
            GenLocated(..), findModule, getTokenStream)
import GHC.Paths (libdir)
import FastString (unpackFS)
import Module (UnitId(..))
import HscTypes (ModGuts(..))
import Avail (AvailInfo(..))
import Name (getOccString)
import Lexer (Token(ITqvarid, ITvarid))

import ExHack.Types (SymName(..), ComponentRoot(..), 
                     ModuleNameT(..), LocatedSym(..),
                     Package(..))

getDesugaredMod :: (MonadIO m) => FilePath -> ComponentRoot -> ModuleName -> m DesugaredModule 
getDesugaredMod pfp cr mn = 
    onModSum pfp cr mn (\modSum -> 
        parseModule modSum >>= typecheckModule >>= desugarModule)

getModImports :: (MonadIO m) => FilePath -> ComponentRoot -> ModuleName -> m [ModuleNameT]
getModImports pfp cr mn = 
    onModSum pfp cr mn (\modSum ->
        pure $ ModuleNameT . pack . moduleNameString . unLoc . snd <$> ms_textual_imps modSum)

getModSymbols :: (MonadIO m) => Package -> FilePath -> ComponentRoot -> ModuleName -> m [LocatedSym] 
getModSymbols p pfp cr@(ComponentRoot crs) mn =
    withGhcEnv pfp cr mn $ do
        m <- findModule (mkModuleName modName) Nothing 
        ts <- getTokenStream m
        let sns = (SymName . pack . unpackFS . getTNames) <$$> filter filterTokenTypes ts
        pure $ (\sn -> LocatedSym (p, fileName, sn)) <$> sns 
        where
            modName = intercalate "." $ components mn
            filterTokenTypes (L _ (ITqvarid _)) = True   
            filterTokenTypes (L _ (ITvarid _)) = True   
            filterTokenTypes _ = False
            getTNames (ITqvarid (_,n)) = n
            getTNames (ITvarid n) = n
            getTNames _ = error "The impossible happened."
            (<$$>) = fmap . fmap
            fileName = crs <> toFilePath mn

getCabalDynFlagsLib :: (MonadIO m) => FilePath -> m (Maybe [String])
getCabalDynFlagsLib fp = do
    let qe = H.mkQueryEnv fp (fp </> "dist")
    cs <- H.runQuery qe $ H.components $ (,) <$> H.ghcOptions
    pure $ fst <$> headMay (filter getLib cs)
  where
    getLib (_,H.ChLibName) = True
    getLib _ = False

getModUnitId :: DesugaredModule -> UnitId
getModUnitId = moduleUnitId . mg_module . dm_core_module

getModName :: DesugaredModule -> String
getModName = moduleNameString . moduleName . mg_module . dm_core_module

getModExports :: DesugaredModule -> [SymName]
getModExports = fmap getAvName . mg_exports . dm_core_module

getContent :: DesugaredModule -> TypecheckedSource
getContent = tm_typechecked_source . dm_typechecked_module

getAvName :: AvailInfo -> SymName
getAvName (Avail n) = SymName $ pack $ getOccString n
getAvName (AvailTC n _ _) = SymName $ pack $ getOccString n

withGhcEnv :: (MonadIO m) => FilePath -> ComponentRoot -> ModuleName -> Ghc a -> m a
withGhcEnv pfp (ComponentRoot cr) mn a = do 
    dflagsCM <- getCabalDynFlagsLib pfp
    -- TODO: Setup better logging
    when (isNothing dflagsCM) . liftIO . putStrLn $ "Cannot retrieve cabal flags for " <> pfp <> "."
    let dflagsC = fromMaybe [] dflagsCM 
    liftIO . runGhc (Just libdir) $ do
        dflagsS <- getSessionDynFlags
        (dflags, _, _) <- parseDynamicFlags dflagsS (noLoc <$> dflagsC)
        _ <- setSessionDynFlags dflags
        target <- guessTarget fileName Nothing
        setTargets [target]
        _ <- load LoadAllTargets
        a
  where
    fileName = cr <> toFilePath mn

onModSum :: (MonadIO m) => FilePath -> ComponentRoot -> ModuleName -> (ModSummary -> Ghc a) -> m a
onModSum pfp cr mn f = withGhcEnv pfp cr mn 
                        (getModSummary (mkModuleName modName) >>= f)
    where
        modName = intercalate "." $ components mn 
