{-# LANGUAGE TupleSections #-}
module ExHack.Ghc (
  UnitId(..),
  TypecheckedSource,
  DesugaredModule(..),
  getDesugaredMod,
  getModUnitId,
  getModName,
  getModExports,
  getContent,
  ) where

import Data.List (intercalate)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
            parseDynamicFlags, noLoc, TypecheckedSource)
import GHC.Paths (libdir)
import Module (UnitId(..))
import HscTypes (ModGuts(..))
import Avail (AvailInfo(..))
import Name (getOccString)
import System.FilePath((</>))


getDesugaredMod :: (MonadIO m) => FilePath -> ModuleName -> m DesugaredModule 
getDesugaredMod pfp mn = 
  liftIO . runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    dflags1 <- getCabalDynFlagsLib pfp
    (dflags, _, _) <- parseDynamicFlags dflags0 (noLoc <$> dflags1)
    _ <- setSessionDynFlags dflags
    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary $ mkModuleName modName
    parseModule modSum >>= typecheckModule >>= desugarModule
  where
    modName = intercalate "." $ components mn 
    fileName = "./" <> toFilePath mn 

-- TODO: API-Rework: how to err if we have no lib?
getCabalDynFlagsLib :: (MonadIO m) => FilePath -> m [String] 
getCabalDynFlagsLib fp = do
  let qe = H.mkQueryEnv fp (fp </> "dist")
  cs <- H.runQuery qe $ H.components $ (,) <$> H.ghcOptions
  --TODO: rewrite /w error handling.
  pure . fst . head $ filter getLib cs
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
