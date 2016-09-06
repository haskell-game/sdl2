{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeFamilies               #-}

#ifndef MIN_VERSION_vector
#define MIN_VERSION_vector(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------

module SDL.Internal.Vect
  ( Point (..)
  , V2 (..)
  , V3 (..)
  , V4 (..)
  ) where

import           Control.Applicative
import           Control.Monad               (liftM)
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Data
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed.Base    as U
import           Foreign.Ptr                 (castPtr)
import           Foreign.Storable            (Storable (..))
import           GHC.Arr                     (Ix (..))
import           GHC.Generics                (Generic, Generic1)
import           Prelude
-- Explicit Prelude import suppresses warnings about redundant imports.
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Use fmap" #-}


-- | A handy wrapper to help distinguish points from vectors at the
-- type level
newtype Point f a = P (f a)
  deriving ( Eq, Ord, Show, Read, Monad, Functor, Applicative, Foldable
           , Traversable, Fractional , Num, Ix, Storable, Generic, Generic1
           , Typeable, Data
           )

data instance U.Vector    (Point f a) =  V_P !(U.Vector    (f a))
data instance U.MVector s (Point f a) = MV_P !(U.MVector s (f a))
instance U.Unbox (f a) => U.Unbox (Point f a)

instance U.Unbox (f a) => M.MVector U.MVector (Point f a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_P v) = M.basicLength v
  basicUnsafeSlice m n (MV_P v) = MV_P (M.basicUnsafeSlice m n v)
  basicOverlaps (MV_P v) (MV_P u) = M.basicOverlaps v u
  basicUnsafeNew n = MV_P `liftM` M.basicUnsafeNew n
  basicUnsafeRead (MV_P v) i = P `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_P v) i (P x) = M.basicUnsafeWrite v i x
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_P v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox (f a) => G.Vector U.Vector (Point f a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_P v) = V_P `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw   ( V_P v) = MV_P `liftM` G.basicUnsafeThaw   v
  basicLength       ( V_P v) = G.basicLength v
  basicUnsafeSlice m n (V_P v) = V_P (G.basicUnsafeSlice m n v)
  basicUnsafeIndexM (V_P v) i = P `liftM` G.basicUnsafeIndexM v i


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
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Functor V2 where
  fmap f (V2 a b) = V2 (f a) (f b)
  {-# INLINE fmap #-}
  a <$ _ = V2 a a
  {-# INLINE (<$) #-}

instance Foldable V2 where
  foldMap f (V2 a b) = f a `mappend` f b
  {-# INLINE foldMap #-}

instance Traversable V2 where
  traverse f (V2 a b) = V2 <$> f a <*> f b
  {-# INLINE traverse #-}

instance Applicative V2 where
  pure a = V2 a a
  {-# INLINE pure #-}
  V2 a b <*> V2 d e = V2 (a d) (b e)
  {-# INLINE (<*>) #-}

instance Monad V2 where
  return a = V2 a a
  {-# INLINE return #-}
  V2 a b >>= f = V2 a' b' where
    V2 a' _ = f a
    V2 _ b' = f b
  {-# INLINE (>>=) #-}

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V2 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V2 a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    tan = fmap tan
    {-# INLINE tan #-}
    cos = fmap cos
    {-# INLINE cos #-}
    asin = fmap asin
    {-# INLINE asin #-}
    atan = fmap atan
    {-# INLINE atan #-}
    acos = fmap acos
    {-# INLINE acos #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}

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

instance Ix a => Ix (V2 a) where
  {-# SPECIALISE instance Ix (V2 Int) #-}

  range (V2 l1 l2,V2 u1 u2) =
    [ V2 i1 i2 | i1 <- range (l1,u1), i2 <- range (l2,u2) ]
  {-# INLINE range #-}

  unsafeIndex (V2 l1 l2,V2 u1 u2) (V2 i1 i2) =
    unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2
  {-# INLINE unsafeIndex #-}

  inRange (V2 l1 l2,V2 u1 u2) (V2 i1 i2) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2
  {-# INLINE inRange #-}

data instance U.Vector    (V2 a) =  V_V2 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V2 a) = MV_V2 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V2 a)

instance U.Unbox a => M.MVector U.MVector (V2 a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_V2 n _) = n
  basicUnsafeSlice m n (MV_V2 _ v) = MV_V2 n (M.basicUnsafeSlice (2*m) (2*n) v)
  basicOverlaps (MV_V2 _ v) (MV_V2 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V2 n) (M.basicUnsafeNew (2*n))
  basicUnsafeRead (MV_V2 _ v) i =
    do let o = 2*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       return (V2 x y)
  basicUnsafeWrite (MV_V2 _ v) i (V2 x y) =
    do let o = 2*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_V2 _ v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox a => G.Vector U.Vector (V2 a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_V2 n v) = liftM ( V_V2 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V2 n v) = liftM (MV_V2 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V2 n _) = n
  basicUnsafeSlice m n (V_V2 _ v) = V_V2 n (G.basicUnsafeSlice (2*m) (2*n) v)
  basicUnsafeIndexM (V_V2 _ v) i =
    do let o = 2*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       return (V2 x y)

instance MonadZip V2 where
  mzipWith = liftA2

instance MonadFix V2 where
  mfix f = V2 (let V2 a _ = f a in a)
              (let V2 _ a = f a in a)

instance Bounded a => Bounded (V2 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}


-- | A 3-dimensional vector
data V3 a = V3 !a !a !a
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Functor V3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)
  {-# INLINE fmap #-}
  a <$ _ = V3 a a a
  {-# INLINE (<$) #-}

instance Foldable V3 where
  foldMap f (V3 a b c) = f a `mappend` f b `mappend` f c
  {-# INLINE foldMap #-}

instance Traversable V3 where
  traverse f (V3 a b c) = V3 <$> f a <*> f b <*> f c
  {-# INLINE traverse #-}

instance Applicative V3 where
  pure a = V3 a a a
  {-# INLINE pure #-}
  V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)
  {-# INLINE (<*>) #-}

instance Monad V3 where
  return a = V3 a a a
  {-# INLINE return #-}
  V3 a b c >>= f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c
  {-# INLINE (>>=) #-}

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V3 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V3 a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    tan = fmap tan
    {-# INLINE tan #-}
    cos = fmap cos
    {-# INLINE cos #-}
    asin = fmap asin
    {-# INLINE asin #-}
    atan = fmap atan
    {-# INLINE atan #-}
    acos = fmap acos
    {-# INLINE acos #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}

instance Storable a => Storable (V3 a) where
  sizeOf _ = 3 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V3 x y z) = do poke ptr' x
                           pokeElemOff ptr' 1 y
                           pokeElemOff ptr' 2 z
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Ix a => Ix (V3 a) where
  {-# SPECIALISE instance Ix (V3 Int) #-}

  range (V3 l1 l2 l3,V3 u1 u2 u3) =
      [V3 i1 i2 i3 | i1 <- range (l1,u1)
                   , i2 <- range (l2,u2)
                   , i3 <- range (l3,u3)
                   ]
  {-# INLINE range #-}

  unsafeIndex (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1)
  {-# INLINE unsafeIndex #-}

  inRange (V3 l1 l2 l3,V3 u1 u2 u3) (V3 i1 i2 i3) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3
  {-# INLINE inRange #-}

data instance U.Vector    (V3 a) =  V_V3 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V3 a) = MV_V3 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V3 a)

instance U.Unbox a => M.MVector U.MVector (V3 a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (MV_V3 n _) = n
  basicUnsafeSlice m n (MV_V3 _ v) = MV_V3 n (M.basicUnsafeSlice (3*m) (3*n) v)
  basicOverlaps (MV_V3 _ v) (MV_V3 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V3 n) (M.basicUnsafeNew (3*n))
  basicUnsafeRead (MV_V3 _ v) i =
    do let o = 3*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       return (V3 x y z)
  basicUnsafeWrite (MV_V3 _ v) i (V3 x y z) =
    do let o = 3*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_V3 _ v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance U.Unbox a => G.Vector U.Vector (V3 a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_V3 n v) = liftM ( V_V3 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V3 n v) = liftM (MV_V3 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V3 n _) = n
  basicUnsafeSlice m n (V_V3 _ v) = V_V3 n (G.basicUnsafeSlice (3*m) (3*n) v)
  basicUnsafeIndexM (V_V3 _ v) i =
    do let o = 3*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       return (V3 x y z)

instance MonadZip V3 where
  mzipWith = liftA2

instance MonadFix V3 where
  mfix f = V3 (let V3 a _ _ = f a in a)
              (let V3 _ a _ = f a in a)
              (let V3 _ _ a = f a in a)

instance Bounded a => Bounded (V3 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}


-- | A 4-dimensional vector.
data V4 a = V4 !a !a !a !a
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Generic1)

instance Functor V4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)
  {-# INLINE fmap #-}
  a <$ _ = V4 a a a a
  {-# INLINE (<$) #-}

instance Foldable V4 where
  foldMap f (V4 a b c d) = f a `mappend` f b `mappend` f c `mappend` f d
  {-# INLINE foldMap #-}

instance Traversable V4 where
  traverse f (V4 a b c d) = V4 <$> f a <*> f b <*> f c <*> f d
  {-# INLINE traverse #-}

instance Applicative V4 where
  pure a = V4 a a a a
  {-# INLINE pure #-}
  V4 a b c d <*> V4 e f g h = V4 (a e) (b f) (c g) (d h)
  {-# INLINE (<*>) #-}

instance Monad V4 where
  return a = V4 a a a a
  {-# INLINE return #-}
  V4 a b c d >>= f = V4 a' b' c' d' where
    V4 a' _ _ _ = f a
    V4 _ b' _ _ = f b
    V4 _ _ c' _ = f c
    V4 _ _ _ d' = f d
  {-# INLINE (>>=) #-}

instance Num a => Num (V4 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (-) #-}
  (-) = liftA2 (-)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V4 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Floating a => Floating (V4 a) where
    pi = pure pi
    {-# INLINE pi #-}
    exp = fmap exp
    {-# INLINE exp #-}
    sqrt = fmap sqrt
    {-# INLINE sqrt #-}
    log = fmap log
    {-# INLINE log #-}
    (**) = liftA2 (**)
    {-# INLINE (**) #-}
    logBase = liftA2 logBase
    {-# INLINE logBase #-}
    sin = fmap sin
    {-# INLINE sin #-}
    tan = fmap tan
    {-# INLINE tan #-}
    cos = fmap cos
    {-# INLINE cos #-}
    asin = fmap asin
    {-# INLINE asin #-}
    atan = fmap atan
    {-# INLINE atan #-}
    acos = fmap acos
    {-# INLINE acos #-}
    sinh = fmap sinh
    {-# INLINE sinh #-}
    tanh = fmap tanh
    {-# INLINE tanh #-}
    cosh = fmap cosh
    {-# INLINE cosh #-}
    asinh = fmap asinh
    {-# INLINE asinh #-}
    atanh = fmap atanh
    {-# INLINE atanh #-}
    acosh = fmap acosh
    {-# INLINE acosh #-}

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

instance Ix a => Ix (V4 a) where
  {-# SPECIALISE instance Ix (V4 Int) #-}

  range (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) =
    [V4 i1 i2 i3 i4 | i1 <- range (l1,u1)
                    , i2 <- range (l2,u2)
                    , i3 <- range (l3,u3)
                    , i4 <- range (l4,u4)
                    ]
  {-# INLINE range #-}

  unsafeIndex (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
    unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
    unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) *
    unsafeIndex (l1,u1) i1))
  {-# INLINE unsafeIndex #-}

  inRange (V4 l1 l2 l3 l4,V4 u1 u2 u3 u4) (V4 i1 i2 i3 i4) =
    inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
    inRange (l3,u3) i3 && inRange (l4,u4) i4
  {-# INLINE inRange #-}

data instance U.Vector    (V4 a) =  V_V4 {-# UNPACK #-} !Int !(U.Vector    a)
data instance U.MVector s (V4 a) = MV_V4 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V4 a)

instance U.Unbox a => M.MVector U.MVector (V4 a) where
  basicLength (MV_V4 n _) = n
  basicUnsafeSlice m n (MV_V4 _ v) = MV_V4 n (M.basicUnsafeSlice (4*m) (4*n) v)
  basicOverlaps (MV_V4 _ v) (MV_V4 _ u) = M.basicOverlaps v u
  basicUnsafeNew n = liftM (MV_V4 n) (M.basicUnsafeNew (4*n))
  basicUnsafeRead (MV_V4 _ v) i =
    do let o = 4*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       w <- M.basicUnsafeRead v (o+3)
       return (V4 x y z w)
  basicUnsafeWrite (MV_V4 _ v) i (V4 x y z w) =
    do let o = 4*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
       M.basicUnsafeWrite v (o+3) w
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_V4 _ v) = M.basicInitialize v
#endif

instance U.Unbox a => G.Vector U.Vector (V4 a) where
  basicUnsafeFreeze (MV_V4 n v) = liftM ( V_V4 n) (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_V4 n v) = liftM (MV_V4 n) (G.basicUnsafeThaw   v)
  basicLength       ( V_V4 n _) = n
  basicUnsafeSlice m n (V_V4 _ v) = V_V4 n (G.basicUnsafeSlice (4*m) (4*n) v)
  basicUnsafeIndexM (V_V4 _ v) i =
    do let o = 4*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       w <- G.basicUnsafeIndexM v (o+3)
       return (V4 x y z w)

instance MonadZip V4 where
  mzipWith = liftA2

instance MonadFix V4 where
  mfix f = V4 (let V4 a _ _ _ = f a in a)
              (let V4 _ a _ _ = f a in a)
              (let V4 _ _ a _ = f a in a)
              (let V4 _ _ _ a = f a in a)

instance Bounded a => Bounded (V4 a) where
  minBound = pure minBound
  {-# INLINE minBound #-}
  maxBound = pure maxBound
  {-# INLINE maxBound #-}
