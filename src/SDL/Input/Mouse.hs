{-# LANGUAGE OverloadedStrings #-}
module SDL.Input.Mouse
  ( -- * Relative Mouse Mode
    setRelativeMouseMode
  , getRelativeMouseMode

    -- * Mouse and Touch Input
  , MouseButton(..)
  , MouseDevice(..)
  , MouseMotion(..)
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

data MouseButton
  = ButtonLeft
  | ButtonMiddle
  | ButtonRight
  | ButtonX1
  | ButtonX2
  deriving (Eq, Show)

-- | Identifies what kind of mouse-like device this is.
data MouseDevice
  = Mouse !Int -- ^ An actual mouse. The number identifies which mouse.
  | Touch      -- ^ Some sort of touch device.
  deriving (Eq, Show)

-- | Are buttons being pressed or released?
data MouseMotion
  = MouseButtonUp
  | MouseButtonDown
  deriving (Eq, Show)
