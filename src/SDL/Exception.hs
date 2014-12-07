{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module SDL.Exception
  ( SDLException(..)
  , fromC
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

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Foreign (Ptr, nullPtr)
import GHC.Generics (Generic)

import qualified SDL.Raw as Raw
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text

data SDLException
  = SDLCallFailed {sdlExceptionCaller :: !Text
                  ,sdlFunction :: !Text
                  ,sdlExceptionError :: !Text}
  | SDLUnexpectedArgument {sdlExceptionCaller :: !Text
                          ,sdlFunction :: !Text
                          ,sdlUnknownValue :: !String}
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance Exception SDLException

getError :: MonadIO m => m Text
getError = liftIO $ do
  cstr <- Raw.getError
  Text.decodeUtf8 <$> BS.packCString cstr

throwIf :: MonadIO m => (a -> Bool) -> Text -> Text -> m a -> m a
throwIf f caller funName m = do
  a <- m
  liftIO $ when (f a) $
    (SDLCallFailed caller funName <$> getError) >>= throwIO
  return a

throwIf_ :: (Functor m, MonadIO m) => (a -> Bool) -> Text -> Text -> m a -> m ()
throwIf_ f caller funName m = void (throwIf f caller funName m)

throwIfNeg :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m a
throwIfNeg = throwIf (< 0)

throwIfNeg_ :: (Functor m, MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m ()
throwIfNeg_ = throwIf_ (< 0)

throwIfNull :: (MonadIO m) => Text -> Text -> m (Ptr a) -> m (Ptr a)
throwIfNull = throwIf (== nullPtr)

throwIf0 :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m a
throwIf0 = throwIf (== 0)

throwIfNot0 :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m a
throwIfNot0 = throwIf (/= 0)

throwIfNot0_ :: (Eq a, Functor m, MonadIO m, Num a) => Text -> Text -> m a -> m ()
throwIfNot0_ = throwIf_ (/= 0)

fromC :: Show a => Text -> Text -> (a -> Maybe b) -> a -> b
fromC caller funName f x =
  fromMaybe (throw (SDLUnexpectedArgument caller
                                          funName
                                          (show x)))
            (f x)
