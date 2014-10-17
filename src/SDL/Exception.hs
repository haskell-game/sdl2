{-# LANGUAGE DeriveDataTypeable #-}
module SDL.Exception
  ( SDLException(..)
  , getError
  , throwIfNeg
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Text (Text)
import Data.Typeable (Typeable)

import qualified SDL.Raw as Raw
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text

data SDLException = SDLException
  { sdlExceptionCaller :: !Text
  , sdlExceptionError :: !Text
  }
  deriving (Eq,Ord,Read,Show,Typeable)

instance Exception SDLException

getError :: IO Text
getError = do
  cstr <- Raw.getError
  Text.decodeUtf8 <$> BS.packCString cstr

throwIfNeg :: (Num a, Ord a) => Text -> IO a -> IO ()
throwIfNeg caller m = do
  a <- m
  when (a < 0) $
    (SDLException caller <$> getError) >>= throwIO
