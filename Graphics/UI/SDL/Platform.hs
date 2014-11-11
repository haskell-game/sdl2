module Graphics.UI.SDL.Platform (
  -- * Platform Detection
  getPlatform
) where

import Control.Monad.IO.Class
import Foreign.C.String

foreign import ccall "SDL.h SDL_GetPlatform" getPlatform' :: IO CString

getPlatform :: MonadIO m => m CString
getPlatform = liftIO getPlatform'
{-# INLINE getPlatform #-}
