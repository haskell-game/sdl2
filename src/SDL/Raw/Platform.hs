module SDL.Raw.Platform (
  -- * Platform Detection
  getPlatform
) where

import Control.Monad.IO.Class
import Foreign.C.String

foreign import ccall "SDL.h SDL_GetPlatform" getPlatformFFI :: IO CString

getPlatform :: MonadIO m => m CString
getPlatform = liftIO getPlatformFFI
{-# INLINE getPlatform #-}
