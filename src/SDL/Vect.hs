{-# LANGUAGE CPP #-}
module SDL.Vect
  ( module Vect
  ) where

#if defined(nolinear)
import SDL.Internal.Vect as Vect
#else
import Linear as Vect
import Linear.Affine as Vect
#endif
