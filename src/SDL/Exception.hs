{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Foreign (Ptr, nullPtr)

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
  deriving (Eq, Ord, Read, Show, Typeable)

instance Exception SDLException

getError :: IO Text
getError = do
  cstr <- Raw.getError
  Text.decodeUtf8 <$> BS.packCString cstr

throwIf :: (a -> Bool) -> Text -> Text -> IO a -> IO a
throwIf f caller funName m = do
  a <- m
  when (f a) $
    (SDLCallFailed caller funName <$> getError) >>= throwIO
  return a

throwIf_ :: (a -> Bool) -> Text -> Text -> IO a -> IO ()
throwIf_ f caller funName m = void (throwIf f caller funName m)

throwIfNeg :: (Num a, Ord a) => Text -> Text -> IO a -> IO a
throwIfNeg = throwIf (< 0)

throwIfNeg_ :: (Num a, Ord a) => Text -> Text -> IO a -> IO ()
throwIfNeg_ = throwIf_ (< 0)

throwIfNull :: Text -> Text -> IO (Ptr a) -> IO (Ptr a)
throwIfNull = throwIf (== nullPtr)

throwIf0 :: (Eq a, Num a) => Text -> Text -> IO a -> IO a
throwIf0 = throwIf (== 0)

throwIfNot0 :: (Eq a, Num a) => Text -> Text -> IO a -> IO a
throwIfNot0 = throwIf (/= 0)

throwIfNot0_ :: (Eq a, Num a) => Text -> Text -> IO a -> IO a
throwIfNot0_ = throwIf (/= 0)

fromC :: Show a => Text -> Text -> (a -> Maybe b) -> a -> b
fromC caller funName f x =
  fromMaybe (throw (SDLUnexpectedArgument caller
                                          funName
                                          (show x)))
            (f x)
