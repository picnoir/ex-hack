{-# LANGUAGE MultiParamTypeClasses #-}

module ExHack.Utils (
    Has(..),
    foldM'
) where

import           Control.Lens (Lens')

class Has a b where
   hasLens :: Lens' a b

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs
