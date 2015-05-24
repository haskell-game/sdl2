module Graphics.UI.SDL.Error (
  -- * Error Handling
  clearError,
  getError,
  setError
) where

import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "SDL.h SDL_ClearError" clearError' :: IO ()
foreign import ccall "SDL.h SDL_GetError" getError' :: IO CString
foreign import ccall "sdlhelper.c SDLHelper_SetError" setError' :: CString -> IO CInt

clearError :: MonadIO m => m ()
clearError = liftIO clearError'
{-# INLINE clearError #-}

getError :: MonadIO m => m CString
getError = liftIO getError'
{-# INLINE getError #-}

setError :: MonadIO m => CString -> m CInt
setError v1 = liftIO $ setError' v1
{-# INLINE setError #-}
