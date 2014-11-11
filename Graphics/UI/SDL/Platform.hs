module Graphics.UI.SDL.Platform (
  -- * Platform Detection
  getPlatform
) where

import Foreign.C.String

foreign import ccall "SDL.h SDL_GetPlatform" getPlatform :: IO CString
