{-# LANGUAGE DeriveDataTypeable #-}
module SDL.Raw.Error (
  -- * Error Handling
  SDLError(..),
  throwError,

  -- * Manual Error Handling
  clearError,
  getError,
  setError
) where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable
import Foreign.C.String
import Foreign.C.Types

-- | Note: the 'CString' is only valid until the next SDL function call. If you
-- need to preserve the error message, make a copy of it.
newtype SDLError = SDLError CString
  deriving (Eq, Show, Typeable)

instance Exception SDLError

foreign import ccall "SDL.h SDL_ClearError" clearErrorFFI :: IO ()
foreign import ccall "SDL.h SDL_GetError" getErrorFFI :: IO CString
foreign import ccall "sdlhelper.c SDLHelper_SetError" setErrorFFI :: CString -> IO CInt

throwError :: (MonadThrow m, MonadIO m) => m ()
throwError = getError >>= throwM . SDLError

clearError :: MonadIO m => m ()
clearError = liftIO clearErrorFFI
{-# INLINE clearError #-}

getError :: MonadIO m => m CString
getError = liftIO getErrorFFI
{-# INLINE getError #-}

setError :: MonadIO m => CString -> m CInt
setError v1 = liftIO $ setErrorFFI v1
{-# INLINE setError #-}
