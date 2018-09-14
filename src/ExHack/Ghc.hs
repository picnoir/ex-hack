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
  unLoc
  ) where

import           Avail                   (AvailInfo (..))
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Maybe              (fromMaybe, isNothing)
import           Data.Text               (pack)
import qualified Distribution.Helper     as H (ChComponentName (ChLibName),
                                               components, ghcOptions,
                                               mkQueryEnv, runQuery)
import           Distribution.ModuleName (ModuleName, toFilePath)
import           FastString              (unpackFS)
import           GHC                     (DesugaredModule, GenLocated (..), Ghc,
                                          LoadHowMuch (..), ModSummary,
                                          TypecheckedSource, desugarModule,
                                          dm_core_module, dm_typechecked_module,
                                          findModule, getModSummary,
                                          getSessionDynFlags, getTokenStream,
                                          guessTarget, load, mkModuleName,
                                          moduleName, moduleNameString,
                                          moduleUnitId, ms_textual_imps, noLoc,
                                          parseDynamicFlags, parseModule,
                                          runGhc, setSessionDynFlags,
                                          setTargets, tm_typechecked_source,
                                          typecheckModule, unLoc)
import           GHC.Paths               (libdir)
import           HscTypes                (ModGuts (..))
import           Lexer                   (Token (ITqvarid, ITvarid))
import           Module                  (UnitId (..))
import           Name                    (getOccString)
import           Safe                    (headMay)
import           System.Directory        (makeAbsolute)
import           System.FilePath         ((<.>), (</>))

import           ExHack.ModulePaths      (modName)
import           ExHack.Types            (ComponentRoot (..), LocatedSym (..),
                                          ModuleNameT (..), Package (..),
                                          PackageFilePath (..), SymName (..))

getDesugaredMod :: (MonadIO m) => PackageFilePath -> ComponentRoot -> ModuleName -> m DesugaredModule 
getDesugaredMod pfp cr mn = 
    onModSum pfp cr mn (\modSum -> 
        parseModule modSum >>= typecheckModule >>= desugarModule)

getModImports :: (MonadIO m) => PackageFilePath -> ComponentRoot -> ModuleName -> m [ModuleNameT]
getModImports pfp cr mn = 
    onModSum pfp cr mn (\modSum ->
        pure $ ModuleNameT . pack . moduleNameString . unLoc . snd <$> ms_textual_imps modSum)

getModSymbols :: (MonadIO m) => Package -> PackageFilePath -> ComponentRoot -> ModuleName -> m [LocatedSym] 
getModSymbols p pfp cr@(ComponentRoot crs) mn = do
    modPath <- liftIO $ makeAbsolute $ toFilePath mn <.> "hs"
    withGhcEnv pfp cr mn $ do
        m <- findModule (mkModuleName $ modPath) Nothing 
        ts <- getTokenStream m
        let sns = (SymName . pack . unpackFS . getTNames) <$$> filter filterTokenTypes ts
        pure $ (\sn -> LocatedSym (p, fileName, sn)) <$> sns 
        where
            filterTokenTypes (L _ (ITqvarid _)) = True   
            filterTokenTypes (L _ (ITvarid _)) = True   
            filterTokenTypes _ = False
            getTNames (ITqvarid (_,n)) = n
            getTNames (ITvarid n) = n
            getTNames _ = error "The impossible happened."
            (<$$>) = fmap . fmap
            fileName = crs </> toFilePath mn <.> "hs"

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

withGhcEnv :: (MonadIO m) => PackageFilePath -> ComponentRoot -> ModuleName -> Ghc a -> m a
withGhcEnv (PackageFilePath pfp) (ComponentRoot cr) mn a = do 
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
    fileName = cr </> toFilePath mn

onModSum :: (MonadIO m) => PackageFilePath -> ComponentRoot -> ModuleName -> (ModSummary -> Ghc a) -> m a
onModSum pfp cr mn f = withGhcEnv pfp cr mn 
                        (getModSummary (mkModuleName $ modName mn) >>= f)
