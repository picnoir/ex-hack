{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module ProcessingSteps (
    generateDb    
) where

import Control.Lens (view)
import Control.Monad.Reader.Class (asks)
import Database.Selda.SQLite (withSQLite)

import ExHack.Utils (Has(..))
import ExHack.Types (MonadStep, DatabaseHandle,
                     DatabaseStatus(..))
import ExHack.Data.Db (initDb)

generateDb :: forall c m. 
    (Has c (DatabaseHandle 'New), 
     MonadStep c m) 
    => m (DatabaseHandle 'Initialized)
generateDb = do
    fp <- asks (view hasLens)
    withSQLite fp initDb
    pure fp
