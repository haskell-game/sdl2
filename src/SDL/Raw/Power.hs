module SDL.Raw.Power (  --
	-- * Power Management Status
	getPowerInfo
) where

import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum

foreign import ccall "SDL.h SDL_GetPowerInfo" getPowerInfo :: Ptr CInt -> Ptr CInt -> IO PowerState
