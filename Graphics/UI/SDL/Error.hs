module Graphics.UI.SDL.Error (
  -- * Error Handling
  clearError,
  getError,
  setError
) where

import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "SDL.h SDL_ClearError" clearErrorFFI :: IO ()
foreign import ccall "SDL.h SDL_GetError" getErrorFFI :: IO CString
foreign import ccall "sdlhelper.c SDLHelper_SetError" setErrorFFI :: CString -> IO CInt

clearError :: MonadIO m => m ()
clearError = liftIO clearErrorFFI
{-# INLINE clearError #-}

getError :: MonadIO m => m CString
getError = liftIO getErrorFFI
{-# INLINE getError #-}

setError :: MonadIO m => CString -> m CInt
setError v1 = liftIO $ setErrorFFI v1
{-# INLINE setError #-}
