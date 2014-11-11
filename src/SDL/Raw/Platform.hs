module SDL.Raw.Platform (
  -- * Platform Detection
  getPlatform
) where

import Foreign.C.String

foreign import ccall "SDL.h SDL_GetPlatform" getPlatform :: IO CString
