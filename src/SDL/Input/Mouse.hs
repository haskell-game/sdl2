{-# LANGUAGE OverloadedStrings #-}
module SDL.Input.Mouse
  ( -- * Relative mouse mode
    setRelativeMouseMode
  , getRelativeMouseMode
  ) where

import SDL.Exception

import qualified SDL.Raw.Event as Raw

-- | Sets the current relative mouse mode.
--
-- When relative mouse mode is enabled, cursor is hidden and mouse position
-- will not change. However, you will be delivered relative mouse position
-- change events.
--
-- Throws 'SDLException' on failure.
setRelativeMouseMode :: Bool -> IO ()
setRelativeMouseMode enable =
    -- relative mouse mode can fail if it's not supported
    throwIfNeg_ "SDL.Input.Mouse" "SDL_SetRelativeMouseMode" $
        Raw.setRelativeMouseMode enable

-- | Check if relative mouse mode is enabled.
getRelativeMouseMode :: IO Bool
getRelativeMouseMode = Raw.getRelativeMouseMode
