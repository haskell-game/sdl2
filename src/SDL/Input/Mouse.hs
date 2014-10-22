{-# LANGUAGE OverloadedStrings #-}

module SDL.Input.Mouse
  ( -- * Relative mouse mode
    setRelativeMouseMode
  , getRelativeMouseMode
    -- * Mouse and touch input
  , MouseMotion(..)
  , MouseButton(..)
  , MouseDevice(..)
  ) where

import SDL.Exception
import qualified SDL.Raw.Event as Raw

-- | Identifies what kind of mouse-like device this is.
data MouseDevice
    = Mouse !Int  -- ^ An actual mouse. The number identifies which mouse.
    | Touch       -- ^ Some sort of touch device.
    deriving ( Eq, Ord, Show, Read )

-- | Are buttons being pressed or released?
data MouseMotion = MouseButtonUp | MouseButtonDown
                   deriving ( Eq, Ord, Show, Read )

data MouseButton
    = ButtonLeft
    | ButtonMiddle
    | ButtonRight
    | ButtonX1
    | ButtonX2
    | ButtonExtra !Int   -- ^ A mouse button that we don't know what it is.
    deriving ( Eq, Ord, Show, Read )

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

