{-# LANGUAGE MultiParamTypeClasses #-}

module ExHack.Utils (
    Has(..)
) where

import Control.Lens (Lens')

class Has a b where
    hasLens :: Lens' a b

