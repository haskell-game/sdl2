module SDL.Raw.Timer (
	-- * Timer Support
	addTimer,
	delay,
	getPerformanceCounter,
	getPerformanceFrequency,
	getTicks,
	removeTimer
) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_AddTimer" addTimer :: Word32 -> TimerCallback -> Ptr () -> IO TimerID
foreign import ccall "SDL.h SDL_Delay" delay :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_GetPerformanceCounter" getPerformanceCounter :: IO Word64
foreign import ccall "SDL.h SDL_GetPerformanceFrequency" getPerformanceFrequency :: IO Word64
foreign import ccall "SDL.h SDL_GetTicks" getTicks :: IO Word32
foreign import ccall "SDL.h SDL_RemoveTimer" removeTimer :: TimerID -> IO Bool
