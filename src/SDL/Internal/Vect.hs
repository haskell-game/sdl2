{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- 2-D, 3-D and 4-D Vectors.
-- The interface is compatible with that of the 'linear' package.
module SDL.Internal.Vect
    ( V2(..)
    , V3(..)
    , V4(..)
    , Point(..)
    ) where

-- From the 'linear' package, (c) Edward Kmett.

import           Control.Applicative
import           Foreign.Storable
import           Foreign.Ptr (castPtr)

-- | A handy wrapper to help distinguish points from vectors at the
-- type level.
newtype Point f a = P (f a)
  deriving (Show, Read, Ord, Eq, Functor, Applicative, Num, Storable)

-- | A 2-dimensional vector
--
-- >>> pure 1 :: V2 Int
-- V2 1 1
--
-- >>> V2 1 2 + V2 3 4
-- V2 4 6
--
-- >>> V2 1 2 * V2 3 4
-- V2 3 8
--
-- >>> sum (V2 1 2)
-- 3
data V2 a = V2 !a !a
    deriving (Show, Read, Ord, Eq)

-- | A 3-dimensional vector
data V3 a = V3 !a !a !a
    deriving (Show, Read, Ord, Eq)

-- | A 4-dimensional vector
data V4 a = V4 !a !a !a !a
    deriving (Show, Read, Ord, Eq)

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)
  {-# INLINE fmap #-}
  a <$ _ = V2 a a
  {-# INLINE (<$) #-}

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
  {-# INLINE fmap #-}
  a <$ _ = V3 a a a
  {-# INLINE (<$) #-}

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)
  {-# INLINE fmap #-}
  a <$ _ = V4 a a a a
  {-# INLINE (<$) #-}

instance Applicative V2 where
    pure a = V2 a a
    V2 a b <*> V2 d e = V2 (a d) (b e)

instance Storable a => Storable (V4 a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V4 x y z w) = do poke ptr' x
                             pokeElemOff ptr' 1 y
                             pokeElemOff ptr' 2 z
                             pokeElemOff ptr' 3 w
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V4 <$> peek ptr' <*> peekElemOff ptr' 1
                <*> peekElemOff ptr' 2 <*> peekElemOff ptr' 3
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable a => Storable (V2 a) where
  sizeOf _ = 2 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V2 x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V2 <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Num a => Num (V2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
