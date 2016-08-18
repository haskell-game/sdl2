{-# LANGUAGE CPP #-}

-- | SDL's vector representation.
--
-- By default, re-exports the vector types from the 'linear' package, but this can be changed via the @-no-linear@
-- build flag to export SDL's internal vector types from "SDL.Internal.Vect".
-- This is useful if one does not want to incur the 'lens' dependency.
module SDL.Vect
  ( module Vect
  ) where

#if defined(nolinear)
import SDL.Internal.Vect as Vect
#else
import Linear as Vect
import Linear.Affine as Vect
#endif
