{-|
Module      : ExHack.Utils
Description : Garbage module. If something ended up here, it's
              surely not for a good reason.
Copyright   : (c) Félix Baylac-Jacqué, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ExHack.Utils (
    Has(..),
    foldM'
) where

import           Control.Lens (Lens')

-- | Ad-Hoc typeclass used to decorelate
--   a `Reader` content from its realization.
class Has a b where
   hasLens :: Lens' a b

-- | Strict version of Control.Monad's `foldM`.
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs
