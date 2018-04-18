{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Input.Mouse
  ( -- * Relative Mouse Mode
    LocationMode(..)
  , setMouseLocationMode
  , getMouseLocationMode

    -- * Mouse and Touch Input
  , MouseButton(..)
  , MouseDevice(..)
  , MouseScrollDirection(..)

    -- * Mouse State
  , getModalMouseLocation
  , getAbsoluteMouseLocation
  , getRelativeMouseLocation
  , getMouseButtons

    -- * Warping the Mouse
  , WarpMouseOrigin(..)
  , warpMouse

    -- * Cursor Visibility
  , cursorVisible

    -- * Cursor Shape
  , Cursor
  , activeCursor
  , createCursor
  , freeCursor
  , createColorCursor
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Bool
import Data.Data (Data)
import Data.StateVar
import Data.Typeable
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import SDL.Vect
import SDL.Internal.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types (Window(Window))
import SDL.Video.Renderer (Surface(Surface))
import qualified Data.Vector.Storable as V
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

data LocationMode
  = AbsoluteLocation
  | RelativeLocation
  deriving (Bounded, Data, Eq, Enum, Generic, Ord, Read, Show, Typeable)

-- | Sets the current relative mouse mode.
--
-- When relative mouse mode is enabled, cursor is hidden and mouse position
-- will not change. However, you will be delivered relative mouse position
-- change events.
setMouseLocationMode :: (Functor m, MonadIO m) => LocationMode -> m LocationMode
setMouseLocationMode mode =
  Raw.setRelativeMouseMode (mode == RelativeLocation) >> getMouseLocationMode

-- | Check which mouse location mode is currently active.
getMouseLocationMode :: MonadIO m => m LocationMode
getMouseLocationMode = do
  relativeMode <- Raw.getRelativeMouseMode
  return $ if relativeMode then RelativeLocation else AbsoluteLocation

data ModalLocation
  = AbsoluteModalLocation (Point V2 CInt)
  | RelativeModalLocation (V2 CInt)
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

-- | Return proper mouse location depending on mouse mode
getModalMouseLocation :: MonadIO m => m ModalLocation
getModalMouseLocation = do
  mode <- getMouseLocationMode
  case mode of
    AbsoluteLocation -> do
      location <- getAbsoluteMouseLocation
      return (AbsoluteModalLocation location)
    RelativeLocation -> do
      location <- getRelativeMouseLocation
      return (RelativeModalLocation location)

data MouseButton
  = ButtonLeft
  | ButtonMiddle
  | ButtonRight
  | ButtonX1
  | ButtonX2
  | ButtonExtra !Int -- ^ An unknown mouse button.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber MouseButton Word8 where
  fromNumber Raw.SDL_BUTTON_LEFT   = ButtonLeft
  fromNumber Raw.SDL_BUTTON_MIDDLE = ButtonMiddle
  fromNumber Raw.SDL_BUTTON_RIGHT  = ButtonRight
  fromNumber Raw.SDL_BUTTON_X1     = ButtonX1
  fromNumber Raw.SDL_BUTTON_X2     = ButtonX2
  fromNumber buttonCode            = ButtonExtra $ fromIntegral buttonCode

instance ToNumber MouseButton Word8 where
  toNumber ButtonLeft      = Raw.SDL_BUTTON_LEFT
  toNumber ButtonMiddle    = Raw.SDL_BUTTON_MIDDLE
  toNumber ButtonRight     = Raw.SDL_BUTTON_RIGHT
  toNumber ButtonX1        = Raw.SDL_BUTTON_X1
  toNumber ButtonX2        = Raw.SDL_BUTTON_X2
  toNumber (ButtonExtra i) = fromIntegral i

-- | Identifies what kind of mouse-like device this is.
data MouseDevice
  = Mouse !Int -- ^ An actual mouse. The number identifies which mouse.
  | Touch      -- ^ Some sort of touch device.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber MouseDevice Word32 where
  fromNumber n' = case n' of
    Raw.SDL_TOUCH_MOUSEID -> Touch
    n -> Mouse $ fromIntegral n

-- | Identifies mouse scroll direction.
data MouseScrollDirection
  = ScrollNormal
  | ScrollFlipped
  deriving (Bounded, Data, Eq, Enum, Generic, Ord, Read, Show, Typeable)

instance FromNumber MouseScrollDirection Word32 where
  fromNumber n' = case n' of
    Raw.SDL_MOUSEWHEEL_NORMAL -> ScrollNormal
    Raw.SDL_MOUSEWHEEL_FLIPPED -> ScrollFlipped
    _ -> ScrollNormal

data WarpMouseOrigin
  = WarpInWindow Window
    -- ^ Move the mouse pointer within a given 'Window'.
  | WarpCurrentFocus
    -- ^ Move the mouse pointer within whichever 'Window' currently has focus.
  | WarpGlobal
    -- ^ Move the mouse pointer in global screen space.
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

-- | Move the current location of a mouse pointer. The 'WarpMouseOrigin' specifies the origin for the given warp coordinates.
warpMouse :: MonadIO m => WarpMouseOrigin -> Point V2 CInt -> m ()
warpMouse (WarpInWindow (Window w)) (P (V2 x y)) = Raw.warpMouseInWindow w x y
warpMouse WarpCurrentFocus (P (V2 x y)) = Raw.warpMouseInWindow nullPtr x y
warpMouse WarpGlobal (P (V2 x y)) = throwIfNeg_ "SDL.Mouse.warpMouse" "SDL_WarpMouseGlobal" $
  Raw.warpMouseGlobal x y

-- | Get or set whether the cursor is currently visible.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_ShowCursor SDL_ShowCursor>@ and @<https://wiki.libsdl.org/SDL_HideCursor SDL_HideCursor>@ for C documentation.
cursorVisible :: StateVar Bool
cursorVisible = makeStateVar getCursorVisible setCursorVisible
  where
  -- The usage of 'void' is OK here - Raw.showCursor just returns the old state.
  setCursorVisible :: (Functor m, MonadIO m) => Bool -> m ()
  setCursorVisible True = void $ Raw.showCursor 1
  setCursorVisible False = void $ Raw.showCursor 0

  getCursorVisible :: (Functor m, MonadIO m) => m Bool
  getCursorVisible = (== 1) <$> Raw.showCursor (-1)

-- | Retrieve the current location of the mouse, relative to the currently focused window.
getAbsoluteMouseLocation :: MonadIO m => m (Point V2 CInt)
getAbsoluteMouseLocation = liftIO $
  alloca $ \x ->
  alloca $ \y -> do
    _ <- Raw.getMouseState x y -- We don't deal with button states here
    P <$> (V2 <$> peek x <*> peek y)

-- | Retrieve mouse motion
getRelativeMouseLocation :: MonadIO m => m (V2 CInt)
getRelativeMouseLocation = liftIO $
  alloca $ \x ->
  alloca $ \y -> do
    _ <- Raw.getRelativeMouseState x y
    V2 <$> peek x <*> peek y


-- | Retrieve a mapping of which buttons are currently held down.
getMouseButtons :: MonadIO m => m (MouseButton -> Bool)
getMouseButtons = liftIO $
  convert <$> Raw.getMouseState nullPtr nullPtr
  where
    convert w b = w `testBit` fromIntegral (toNumber b - 1)

newtype Cursor = Cursor { unwrapCursor :: Raw.Cursor }
    deriving (Eq, Typeable)

-- | Get or set the currently active cursor. You can create new 'Cursor's with 'createCursor'.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetCursor SDL_SetCursor>@ and @<https://wiki.libsdl.org/SDL_GetCursor SDL_GetCursor>@ for C documentation.
activeCursor :: StateVar Cursor
activeCursor = makeStateVar getCursor setCursor
  where
  getCursor :: MonadIO m => m Cursor
  getCursor = liftIO . fmap Cursor $
      throwIfNull "SDL.Input.Mouse.getCursor" "SDL_getCursor"
          Raw.getCursor

  setCursor :: MonadIO m => Cursor -> m ()
  setCursor = Raw.setCursor . unwrapCursor

-- | Create a cursor using the specified bitmap data and mask (in MSB format).
--
--
createCursor :: MonadIO m
             => V.Vector Bool -- ^ Whether this part of the cursor is black. Use 'False' for white and 'True' for black.
             -> V.Vector Bool -- ^ Whether or not pixels are visible. Use 'True' for visible and 'False' for transparent.
             -> V2 CInt -- ^ The width and height of the cursor.
             -> Point V2 CInt -- ^ The X- and Y-axis location of the upper left corner of the cursor relative to the actual mouse position
             -> m Cursor
createCursor dta msk (V2 w h) (P (V2 hx hy)) =
    liftIO . fmap Cursor $
        throwIfNull "SDL.Input.Mouse.createCursor" "SDL_createCursor" $
            V.unsafeWith (V.map (bool 0 1) dta) $ \unsafeDta ->
            V.unsafeWith (V.map (bool 0 1) msk) $ \unsafeMsk ->
                Raw.createCursor unsafeDta unsafeMsk w h hx hy

-- | Free a cursor created with 'createCursor' and 'createColorCusor'.
--
-- See @<https://wiki.libsdl.org/SDL_FreeCursor SDL_FreeCursor>@ for C documentation.
freeCursor :: MonadIO m => Cursor -> m ()
freeCursor = Raw.freeCursor . unwrapCursor

-- | Create a color cursor.
--
-- See @<https://wiki.libsdl.org/SDL_CreateColorCursor SDL_CreateColorCursor>@ for C documentation.
createColorCursor :: MonadIO m
                  => Surface
                  -> Point V2 CInt -- ^ The location of the cursor hot spot
                  -> m Cursor
createColorCursor (Surface surfPtr _) (P (V2 hx hy)) =
    liftIO . fmap Cursor $
        throwIfNull "SDL.Input.Mouse.createColorCursor" "SDL_createColorCursor" $
            Raw.createColorCursor surfPtr hx hy
