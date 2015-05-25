{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Input.Mouse
  ( -- * Relative Mouse Mode
    setRelativeMouseMode
  , getRelativeMouseMode

    -- * Mouse and Touch Input
  , MouseButton(..)
  , MouseDevice(..)
  , MouseMotion(..)

    -- * Mouse State
  , getMouseState
  , getMouseButtons

    -- * Warping the Mouse
  , WarpMouseOrigin
  , warpMouse

    -- * Cursor Visibility
  , setCursorVisible
  , getCursorVisible

    -- * Changing the cursor
  , createColorCursor
  , freeCursor
  , setCursor
  ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Data (Data)
import Data.Typeable
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import Linear
import Linear.Affine
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types (Window(Window))
import SDL.Video.Renderer (Surface(Surface))

import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

-- | Sets the current relative mouse mode.
--
-- When relative mouse mode is enabled, cursor is hidden and mouse position
-- will not change. However, you will be delivered relative mouse position
-- change events.
--
-- Throws 'SDLException' on failure.
setRelativeMouseMode :: (Functor m, MonadIO m) => Bool -> m ()
setRelativeMouseMode enable =
    -- relative mouse mode can fail if it's not supported
    throwIfNeg_ "SDL.Input.Mouse" "SDL_SetRelativeMouseMode" $
        Raw.setRelativeMouseMode enable

-- | Check if relative mouse mode is enabled.
getRelativeMouseMode :: MonadIO m => m Bool
getRelativeMouseMode = Raw.getRelativeMouseMode

data MouseButton
  = ButtonLeft
  | ButtonMiddle
  | ButtonRight
  | ButtonX1
  | ButtonX2
  | ButtonExtra !Int -- ^ An unknown mouse button.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Identifies what kind of mouse-like device this is.
data MouseDevice
  = Mouse !Int -- ^ An actual mouse. The number identifies which mouse.
  | Touch      -- ^ Some sort of touch device.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber MouseDevice Word32 where
  fromNumber n' = case n' of
    Raw.SDL_TOUCH_MOUSEID -> Touch
    n -> Mouse $ fromIntegral n

-- | Are buttons being pressed or released?
data MouseMotion
  = MouseButtonUp
  | MouseButtonDown
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data WarpMouseOrigin
  = WarpInWindow Window
  | WarpCurrentFocus
  -- WarpGlobal -- Needs 2.0.4
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

data Cursor = Cursor Raw.Cursor

warpMouse :: MonadIO m => WarpMouseOrigin -> V2 CInt -> m ()
warpMouse (WarpInWindow (Window w)) (V2 x y) = Raw.warpMouseInWindow w x y
warpMouse WarpCurrentFocus (V2 x y) = Raw.warpMouseInWindow nullPtr x y

-- The usage of 'void' is OK here - Raw.showCursor just returns the old state.
setCursorVisible :: (Functor m, MonadIO m) => Bool -> m ()
setCursorVisible True = void $ Raw.showCursor 1
setCursorVisible False = void $ Raw.showCursor 0

getCursorVisible :: (Functor m, MonadIO m) => m Bool
getCursorVisible = (== 1) <$> Raw.showCursor (-1)

getMouseState :: MonadIO m => m (Point V2 CInt)
getMouseState = liftIO $
  alloca $ \x ->
  alloca $ \y -> do
    _ <- Raw.getMouseState x y -- We don't deal with button states here
    P <$> (V2 <$> peek x <*> peek y)

getMouseButtons :: MonadIO m => m (MouseButton -> Bool)
getMouseButtons = liftIO $
  convert <$> Raw.getMouseState nullPtr nullPtr
  where
    convert w b = w `testBit` index
      where
      index = case b of
                ButtonLeft    -> 0
                ButtonMiddle  -> 1
                ButtonRight   -> 2
                ButtonX1      -> 3
                ButtonX2      -> 4
                ButtonExtra i -> i

createColorCursor :: (Functor m, MonadIO m) => Surface -> V2 CInt -> m Cursor
createColorCursor (Surface s _) (V2 x y) =
  fmap Cursor $
    throwIfNull "SDL.Input.Mouse.createColorCursor" "SDL_CreateColorCursor" $
      Raw.createColorCursor s x y

freeCursor :: MonadIO m => Cursor -> m ()
freeCursor (Cursor c) = Raw.freeCursor c

setCursor :: MonadIO m => Cursor -> m ()
setCursor (Cursor c) = Raw.setCursor c
