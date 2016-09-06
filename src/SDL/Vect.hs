{-# LANGUAGE CPP #-}

-- | SDL's vector representation.
--
-- By default, re-exports the "Linear" and "Linear.Affine" modules from the
-- 'linear' package. With the @no-linear@ Cabal flag, instead exports a
-- duplicate implementation of the 'V2', 'V3', 'V4' and 'Point' types from
-- "SDL.Internal.Vect", which provides as many instances as possible for those
-- types while avoiding any additional dependencies.
module SDL.Vect
  ( module Vect
  -- * Point
  , Point (..)
  -- * Vectors
  , V2 (..)
  , V3 (..)
  , V4 (..)
  ) where

#if defined(nolinear)
import SDL.Internal.Vect as Vect
#else
import Linear as Vect
import Linear.Affine as Vect
#endif
