{-# LANGUAGE FunctionalDependencies #-}
module SDL.Internal.Numbered
  ( Numbered(..)
  ) where

class (Integral b) => Numbered a b | a -> b where
	toNumber :: a -> b
