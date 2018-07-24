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

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC (runGhc, getSessionDynFlags,
            setSessionDynFlags, guessTarget,
            setTargets, load, LoadHowMuch(..),
            getModSummary, mkModuleName,
            parseModule, typecheckModule,
            desugarModule, DesugaredModule,
            moduleUnitId, dm_core_module,
            moduleNameString, moduleName,
            dm_typechecked_module, tm_typechecked_source,
            TypecheckedSource)
import GHC.Paths (libdir)
import Module (UnitId(..))
import HscTypes (ModGuts(..))
import Avail (AvailInfo(..))
import Name ( getOccString)


-- | TODO: make parameters type-safe
getDesugaredMod :: (MonadIO m) => FilePath -> String -> m DesugaredModule 
getDesugaredMod fileName modName = do
  liftIO $ putStrLn $ "Desugaring mod " <> modName <> " from " <> fileName
  liftIO . runGhc (Just libdir) $ do
    -- TODO: chdir??
    dflags <- getSessionDynFlags
    -- TODO: add Stack lib path in dyn flags.
    _ <- setSessionDynFlags dflags
    target <- guessTarget fileName Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modSum <- getModSummary $ mkModuleName modName
    parseModule modSum >>= typecheckModule >>= desugarModule

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
