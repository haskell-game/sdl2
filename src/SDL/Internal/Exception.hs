{-# LANGUAGE CPP #-}

module SDL.Internal.Exception
  ( fromC
  , getError
  , throwIf
  , throwIf_
  , throwIf0
  , throwIfNeg
  , throwIfNeg_
  , throwIfNot0
  , throwIfNot0_
  , throwIfNull
  ) where

import Control.Exception
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Foreign (Ptr, nullPtr)
import SDL.Exception
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

getError :: MonadIO m => m Text
getError = liftIO $ do
  cstr <- Raw.getError
  Text.decodeUtf8 <$> BS.packCString cstr

{-# INLINE throwIf #-}
throwIf :: MonadIO m => (a -> Bool) -> Text -> Text -> m a -> m a
throwIf f caller funName m = do
  a <- m
  liftIO $ when (f a) $
    (SDLCallFailed caller funName <$> getError) >>= throwIO
  return a

{-# INLINE throwIf_ #-}
throwIf_ :: MonadIO m => (a -> Bool) -> Text -> Text -> m a -> m ()
throwIf_ f caller funName m = throwIf f caller funName m >> return ()

{-# INLINE throwIfNeg #-}
throwIfNeg :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m a
throwIfNeg = throwIf (< 0)

{-# INLINE throwIfNeg_ #-}
throwIfNeg_ :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m ()
throwIfNeg_ = throwIf_ (< 0)

{-# INLINE throwIfNull #-}
throwIfNull :: (MonadIO m) => Text -> Text -> m (Ptr a) -> m (Ptr a)
throwIfNull = throwIf (== nullPtr)

{-# INLINE throwIf0 #-}
throwIf0 :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m a
throwIf0 = throwIf (== 0)

{-# INLINE throwIfNot0 #-}
throwIfNot0 :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m a
throwIfNot0 = throwIf (/= 0)

{-# INLINE throwIfNot0_ #-}
throwIfNot0_ :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m ()
throwIfNot0_ = throwIf_ (/= 0)

fromC :: Show a => Text -> Text -> (a -> Maybe b) -> a -> b
fromC caller funName f x =
  fromMaybe (throw (SDLUnexpectedArgument caller
                                          funName
                                          (show x)))
            (f x)
