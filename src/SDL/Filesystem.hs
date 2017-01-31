{-# LANGUAGE CPP#-}
{-# LANGUAGE OverloadedStrings #-}

module SDL.Filesystem
  ( -- * Filesystem Paths
    getBasePath
  , getPrefPath
) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Foreign.Marshal.Alloc
import SDL.Internal.Exception
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified SDL.Raw.Filesystem as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | An absolute path to the application data directory.
--
-- The path is guaranteed to end with a path separator.
--
-- Throws 'SDLException' on failure, or if the platform does not implement this
-- functionality.
getBasePath :: MonadIO m => m Text
getBasePath = liftIO $ mask_ $ do
  cpath <- throwIfNull "SDL.Filesystem.getBasePath" "SDL_GetBasePath"
    Raw.getBasePath
  finally (Text.decodeUtf8 <$> BS.packCString cpath) (free cpath)

-- | A path to a unique per user and per application directory for the given
-- organization and application name, intended for writing preferences and
-- other personal files.
--
-- The path is guaranteed to end with a path separator.
--
-- You should assume the path returned by this function is the only safe place
-- to write files to.
--
-- Throws 'SDLException' on failure.
getPrefPath :: MonadIO m => Text -> Text -> m Text
getPrefPath organization application = liftIO $ mask_ $ do
  cpath <- throwIfNull "SDL.Filesystem.getPrefPath" "SDL_GetPrefPath" $
    BS.useAsCString (Text.encodeUtf8 organization) $ \org ->
      BS.useAsCString (Text.encodeUtf8 application) $ \app ->
        Raw.getPrefPath org app
  finally (Text.decodeUtf8 <$> BS.packCString cpath) (free cpath)
