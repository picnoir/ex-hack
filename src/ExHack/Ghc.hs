module ExHack.Ghc (
  UnitId(..),
  TypecheckedSource,
  DesugaredModule(..),
  getDesugaredMod,
  getModUnitId,
  getModName,
  getModExports,
  getModImports,
  getContent,
  ) where

import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)
import Distribution.ModuleName (ModuleName, components, toFilePath)
import qualified Distribution.Helper as H (mkQueryEnv, runQuery, 
                                           ghcOptions, components, 
                                           ChComponentName(ChLibName))
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
            ModSummary, Ghc, unLoc, ms_textual_imps)
import GHC.Paths (libdir)
import Module (UnitId(..))
import HscTypes (ModGuts(..))
import Avail (AvailInfo(..))
import Name (getOccString)
import Safe (headMay)
import System.FilePath((</>))

getDesugaredMod :: (MonadIO m) => FilePath -> ModuleName -> m DesugaredModule 
getDesugaredMod pfp mn = 
    onModSum pfp mn (\modSum -> 
        parseModule modSum >>= typecheckModule >>= desugarModule)

getModImports :: (MonadIO m) => FilePath -> ModuleName -> m [String]
getModImports pfp mn = 
    onModSum pfp mn (\modSum ->
        pure $ moduleNameString . unLoc . snd <$> ms_textual_imps modSum)

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

getModExports :: DesugaredModule -> [String]
getModExports = fmap getAvName . mg_exports . dm_core_module

getContent :: DesugaredModule -> TypecheckedSource
getContent = tm_typechecked_source . dm_typechecked_module

getAvName :: AvailInfo -> String
getAvName (Avail n) = getOccString n
getAvName (AvailTC n _ _) = getOccString n

onModSum :: (MonadIO m) => FilePath -> ModuleName -> (ModSummary -> Ghc a) -> m a
onModSum pfp mn f = do 
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
        modSum <- getModSummary $ mkModuleName modName
        f modSum
  where
    modName = intercalate "." $ components mn 
    fileName = "./" <> toFilePath mn 
