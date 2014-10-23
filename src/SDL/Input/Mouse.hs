{-# LANGUAGE OverloadedStrings #-}
module SDL.Input.Mouse
  ( -- * Relative Mouse Mode
    setRelativeMouseMode
  , getRelativeMouseMode

    -- * Mouse and Touch Input
  , MouseButton(..)
  , MouseDevice(..)
  , MouseMotion(..)

    -- * Warping the Mouse
  , WarpMouseOrigin
  , warpMouse

    -- * Cursor Visibility
  , setCursorVisible
  , getCursorVisible
  ) where

import Control.Applicative
import Control.Monad (void)
import Foreign.C
import Foreign.Ptr
import Linear
import SDL.Exception
import SDL.Internal.Types (Window(Window))

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
  | ButtonExtra !Int -- ^ An unknown mouse button.
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

data WarpMouseOrigin = WarpInWindow Window | WarpCurrentFocus -- WarpGlobal -- Needs 2.0.4

warpMouse :: WarpMouseOrigin -> V2 CInt -> IO ()
warpMouse (WarpInWindow (Window w)) (V2 x y) = Raw.warpMouseInWindow w x y
warpMouse WarpCurrentFocus (V2 x y) = Raw.warpMouseInWindow nullPtr x y

-- The usage of 'void' is OK here - Raw.showCursor just returns the old state.
setCursorVisible :: Bool -> IO ()
setCursorVisible True = void $ Raw.showCursor 1
setCursorVisible False = void $ Raw.showCursor 0

getCursorVisible :: IO Bool
getCursorVisible = (== 1) <$> Raw.showCursor (-1)
