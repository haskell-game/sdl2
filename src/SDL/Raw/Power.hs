module SDL.Raw.Power (
  -- * Power Management Status
  getPowerInfo
) where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum

foreign import ccall "SDL.h SDL_GetPowerInfo" getPowerInfoFFI :: Ptr CInt -> Ptr CInt -> IO PowerState

getPowerInfo :: MonadIO m => Ptr CInt -> Ptr CInt -> m PowerState
getPowerInfo v1 v2 = liftIO $ getPowerInfoFFI v1 v2
{-# INLINE getPowerInfo #-}
