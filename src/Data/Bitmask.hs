module Data.Bitmask (foldFlags) where

import Prelude hiding (foldl)

import Data.Bits
import Data.Foldable

foldFlags :: (Bits b, Foldable f, Num b) => (flag -> b) -> f flag -> b
foldFlags f = foldl (\a b -> a .|. f b) 0
