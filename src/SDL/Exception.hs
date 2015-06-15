{-# LANGUAGE CPP #-}
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

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Foreign (Ptr, nullPtr)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Error details about a failure to call an SDL routine. Almost all functions in this library have the
-- ability to produce exceptions of this type. Inspection should help you learn more about what has
-- gone wrong.
data SDLException
  = -- | A call to a low-level SDL C function failed unexpectedly.
    SDLCallFailed
    {sdlExceptionCaller :: !Text
     -- ^ The Haskell routine that was trying to call a C function
    ,sdlFunction :: !Text
     -- ^ The C function that was called and produced an error
    ,sdlExceptionError :: !Text
    -- ^ SDL's understanding of what has gone wrong
    }
  | -- | An SDL C function was called with an unexpected argument.
    SDLUnexpectedArgument
    {sdlExceptionCaller :: !Text
     -- ^ The Haskell routine that was trying to call a C function
    ,sdlFunction :: !Text
     -- ^ The C function that was called and produced an error
    ,sdlUnknownValue :: !String
     -- ^ The argument that SDL failed to understand
    }
  | -- | A hint was attempted to be set, but SDL does not know about this hint.
    SDLUnknownHintValue
    {sdlHint :: !String
     -- ^ The hint that could not be set
    ,sdlUnknownValue :: !String
     -- ^ The value that could not be set
    }
  deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

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

throwIf_ :: MonadIO m => (a -> Bool) -> Text -> Text -> m a -> m ()
throwIf_ f caller funName m = throwIf f caller funName m >> return ()

throwIfNeg :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m a
throwIfNeg = throwIf (< 0)

throwIfNeg_ :: (MonadIO m, Num a, Ord a) => Text -> Text -> m a -> m ()
throwIfNeg_ = throwIf_ (< 0)

throwIfNull :: (MonadIO m) => Text -> Text -> m (Ptr a) -> m (Ptr a)
throwIfNull = throwIf (== nullPtr)

throwIf0 :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m a
throwIf0 = throwIf (== 0)

throwIfNot0 :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m a
throwIfNot0 = throwIf (/= 0)

throwIfNot0_ :: (Eq a, MonadIO m, Num a) => Text -> Text -> m a -> m ()
throwIfNot0_ = throwIf_ (/= 0)

fromC :: Show a => Text -> Text -> (a -> Maybe b) -> a -> b
fromC caller funName f x =
  fromMaybe (throw (SDLUnexpectedArgument caller
                                          funName
                                          (show x)))
            (f x)
