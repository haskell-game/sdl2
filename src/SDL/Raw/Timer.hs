module SDL.Raw.Timer (
  -- * Timer Support
  addTimer,
  delay,
  getPerformanceCounter,
  getPerformanceFrequency,
  getTicks,
  removeTimer
) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_AddTimer" addTimerFFI :: Word32 -> TimerCallback -> Ptr () -> IO TimerID
foreign import ccall "SDL.h SDL_Delay" delayFFI :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_GetPerformanceCounter" getPerformanceCounterFFI :: IO Word64
foreign import ccall "SDL.h SDL_GetPerformanceFrequency" getPerformanceFrequencyFFI :: IO Word64
foreign import ccall "SDL.h SDL_GetTicks" getTicksFFI :: IO Word32
foreign import ccall "SDL.h SDL_RemoveTimer" removeTimerFFI :: TimerID -> IO Bool

addTimer :: MonadIO m => Word32 -> TimerCallback -> Ptr () -> m TimerID
addTimer v1 v2 v3 = liftIO $ addTimerFFI v1 v2 v3
{-# INLINE addTimer #-}

delay :: MonadIO m => Word32 -> m ()
delay v1 = liftIO $ delayFFI v1
{-# INLINE delay #-}

getPerformanceCounter :: MonadIO m => m Word64
getPerformanceCounter = liftIO getPerformanceCounterFFI
{-# INLINE getPerformanceCounter #-}

getPerformanceFrequency :: MonadIO m => m Word64
getPerformanceFrequency = liftIO getPerformanceFrequencyFFI
{-# INLINE getPerformanceFrequency #-}

getTicks :: MonadIO m => m Word32
getTicks = liftIO getTicksFFI
{-# INLINE getTicks #-}

removeTimer :: MonadIO m => TimerID -> m Bool
removeTimer v1 = liftIO $ removeTimerFFI v1
{-# INLINE removeTimer #-}
