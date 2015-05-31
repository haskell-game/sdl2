{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Video
  ( module SDL.Video.OpenGL
  , module SDL.Video.Renderer

  -- * Window Management
  , Window
  , createWindow
  , defaultWindow
  , WindowConfig(..)
  , WindowMode(..)
  , WindowPosition(..)
  , destroyWindow

  -- * Window Actions
  , hideWindow
  , raiseWindow
  , showWindow

  -- * Window Attributes
  , getWindowMinimumSize
  , getWindowMaximumSize
  , getWindowSize
  , setWindowBordered
  , getWindowBrightness
  , setWindowBrightness
  , setWindowGammaRamp
  , setWindowGrab
  , setWindowMode
  , setWindowMaximumSize
  , setWindowMinimumSize
  , getWindowPosition
  , setWindowPosition
  , setWindowSize
  , setWindowTitle

  -- * Renderer Management
  , createRenderer
  , destroyRenderer

  -- * Clipboard Handling
  , getClipboardText
  , hasClipboardText
  , setClipboardText

  -- * Display
  , getDisplays
  , Display(..)
  , DisplayMode(..)
  , VideoDriver(..)

  -- * Screen Savers
  -- | Screen savers should be disabled when the sudden enablement of the
  -- monitor's power saving features would be inconvenient for when the user
  -- hasn't provided any input for some period of time, such as during video
  -- playback.
  --
  -- Screen savers are disabled by default upon the initialization of the
  -- video subsystem.
  , disableScreenSaver
  , enableScreenSaver
  , isScreenSaverEnabled

  -- * Message Box
  , showSimpleMessageBox
  , MessageKind(..)
  ) where

import Prelude hiding (all, foldl, foldr, mapM_)

import Control.Applicative
import Control.Exception
import Control.Monad (forM, unless)
import Data.Data (Data)
import Data.Foldable
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Typeable
import Foreign hiding (void, throwIfNull, throwIfNeg, throwIfNeg_)
import Foreign.C
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point(P))
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types
import SDL.Video.OpenGL
import SDL.Video.Renderer

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as SV
import qualified SDL.Raw as Raw

-- | Create a window with the given title and configuration.
--
-- Throws 'SDLException' on failure.
createWindow :: Text -> WindowConfig -> IO Window
createWindow title config = do
  case windowOpenGL config of
    Just glcfg -> setGLAttributes glcfg
    Nothing    -> return ()

  BS.useAsCString (Text.encodeUtf8 title) $ \title' -> do
    let create = Raw.createWindow title'
    let create' (V2 w h) = case windowPosition config of
          Centered -> let u = Raw.SDL_WINDOWPOS_CENTERED in create u u w h
          Wherever -> let u = Raw.SDL_WINDOWPOS_UNDEFINED in create u u w h
          Absolute (P (V2 x y)) -> create x y w h
    create' (windowSize config) flags >>= return . Window
  where
    flags = foldr (.|.) 0
      [ if windowBorder config then 0 else Raw.SDL_WINDOW_BORDERLESS
      , if windowHighDPI config then Raw.SDL_WINDOW_ALLOW_HIGHDPI else 0
      , if windowInputGrabbed config then Raw.SDL_WINDOW_INPUT_GRABBED else 0
      , toNumber $ windowMode config
      , if isJust $ windowOpenGL config then Raw.SDL_WINDOW_OPENGL else 0
      , if windowResizable config then Raw.SDL_WINDOW_RESIZABLE else 0
      ]
    setGLAttributes (OpenGLConfig (V4 r g b a) d s p) = do
      let (msk, v0, v1, flg) = case p of
            Core Debug v0' v1' -> (Raw.SDL_GL_CONTEXT_PROFILE_CORE, v0', v1', Raw.SDL_GL_CONTEXT_DEBUG_FLAG)
            Core Normal v0' v1' -> (Raw.SDL_GL_CONTEXT_PROFILE_CORE, v0', v1', 0)
            Compatibility Debug v0' v1' -> (Raw.SDL_GL_CONTEXT_PROFILE_COMPATIBILITY, v0', v1', Raw.SDL_GL_CONTEXT_DEBUG_FLAG)
            Compatibility Normal v0' v1' -> (Raw.SDL_GL_CONTEXT_PROFILE_COMPATIBILITY, v0', v1', 0)
            ES Debug v0' v1' -> (Raw.SDL_GL_CONTEXT_PROFILE_ES, v0', v1', Raw.SDL_GL_CONTEXT_DEBUG_FLAG)
            ES Normal v0' v1' -> (Raw.SDL_GL_CONTEXT_PROFILE_ES, v0', v1', 0)
      mapM_ (throwIfNeg_ "SDL.Video.createWindow" "SDL_GL_SetAttribute" . uncurry Raw.glSetAttribute) $
        [ (Raw.SDL_GL_RED_SIZE, r)
        , (Raw.SDL_GL_GREEN_SIZE, g)
        , (Raw.SDL_GL_BLUE_SIZE, b)
        , (Raw.SDL_GL_ALPHA_SIZE, a)
        , (Raw.SDL_GL_DEPTH_SIZE, d)
        , (Raw.SDL_GL_STENCIL_SIZE, s)
        , (Raw.SDL_GL_CONTEXT_PROFILE_MASK, msk)
        , (Raw.SDL_GL_CONTEXT_MAJOR_VERSION, v0)
        , (Raw.SDL_GL_CONTEXT_MINOR_VERSION, v1)
        , (Raw.SDL_GL_CONTEXT_FLAGS, flg)
        ]

-- | Default configuration for windows. Use the record update syntax to
-- override any of the defaults.
defaultWindow :: WindowConfig
defaultWindow = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = False
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = False
  , windowSize         = V2 800 600
  }

data WindowConfig = WindowConfig
  { windowBorder       :: Bool               -- ^ Defaults to 'True'.
  , windowHighDPI      :: Bool               -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowInputGrabbed :: Bool               -- ^ Defaults to 'False'. Whether the mouse shall be confined to the window.
  , windowMode         :: WindowMode         -- ^ Defaults to 'Windowed'.
  , windowOpenGL       :: Maybe OpenGLConfig -- ^ Defaults to 'Nothing'. Can not be changed after window creation.
  , windowPosition     :: WindowPosition     -- ^ Defaults to 'Wherever'.
  , windowResizable    :: Bool               -- ^ Defaults to 'False'. Whether the window can be resized by the user. It is still possible to programatically change the size with 'setWindowSize'.
  , windowSize         :: V2 CInt            -- ^ Defaults to @(800, 600)@.
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

data WindowMode
  = Fullscreen        -- ^ Real fullscreen with a video mode change
  | FullscreenDesktop -- ^ Fake fullscreen that takes the size of the desktop
  | Maximized
  | Minimized
  | Windowed
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance ToNumber WindowMode Word32 where
  toNumber Fullscreen = Raw.SDL_WINDOW_FULLSCREEN
  toNumber FullscreenDesktop = Raw.SDL_WINDOW_FULLSCREEN_DESKTOP
  toNumber Maximized = Raw.SDL_WINDOW_MAXIMIZED
  toNumber Minimized = Raw.SDL_WINDOW_MINIMIZED
  toNumber Windowed = 0

data WindowPosition
  = Centered
  | Wherever -- ^ Let the window mananger decide where it's best to place the window.
  | Absolute (Point V2 CInt)
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

-- | Destroy the given window. The 'Window' handler may not be used
-- afterwards.
destroyWindow :: Window -> IO ()
destroyWindow (Window w) = Raw.destroyWindow w

-- | Set whether the window should have a border or not.
setWindowBordered :: Window -> Bool -> IO ()
setWindowBordered (Window w) = Raw.setWindowBordered w

-- | Set the window's brightness, where 0.0 is completely dark and 1.0 is
-- normal brightness.
--
-- Throws 'SDLException' if the hardware does not support gamma
-- correction, or if the system has run out of memory.
setWindowBrightness :: Window -> Float -> IO ()
setWindowBrightness (Window w) brightness = do
  throwIfNot0_ "SDL.Video.setWindowBrightness" "SDL_SetWindowBrightness" $
    Raw.setWindowBrightness w $ realToFrac brightness

-- | Get the gamma value for the display that owns the given window.
--
-- Returned value is in range [0,1] where 0 means completely dark and 1
-- corresponds to normal brightness.
getWindowBrightness :: Window -> IO Float
getWindowBrightness (Window w) =
    realToFrac <$> Raw.getWindowBrightness w

-- | Set whether the mouse shall be confined to the window.
setWindowGrab :: Window -> Bool -> IO ()
setWindowGrab (Window w) = Raw.setWindowGrab w

-- | Change between window modes.
--
-- Throws 'SDLException' on failure.
setWindowMode :: Window -> WindowMode -> IO ()
setWindowMode (Window w) mode =
  throwIfNot0_ "SDL.Video.setWindowMode" "SDL_SetWindowFullscreen" $
    case mode of
      Fullscreen -> Raw.setWindowFullscreen w Raw.SDL_WINDOW_FULLSCREEN
      FullscreenDesktop -> Raw.setWindowFullscreen w Raw.SDL_WINDOW_FULLSCREEN_DESKTOP
      Maximized -> Raw.setWindowFullscreen w 0 <* Raw.maximizeWindow w
      Minimized -> Raw.minimizeWindow w >> return 0
      Windowed -> Raw.restoreWindow w >> return 0

-- | Set the position of the window.
setWindowPosition :: Window -> WindowPosition -> IO ()
setWindowPosition (Window w) pos = case pos of
  Centered -> let u = Raw.SDL_WINDOWPOS_CENTERED in Raw.setWindowPosition w u u
  Wherever -> let u = Raw.SDL_WINDOWPOS_UNDEFINED in Raw.setWindowPosition w u u
  Absolute (P (V2 x y)) -> Raw.setWindowPosition w x y

-- | Get the position of the window.
getWindowPosition :: Window -> IO (V2 CInt)
getWindowPosition (Window w) =
    alloca $ \wPtr ->
    alloca $ \hPtr -> do
        Raw.getWindowPosition w wPtr hPtr
        V2 <$> peek wPtr <*> peek hPtr 


-- | Set the size of the window. Values beyond the maximum supported size are
-- clamped.
setWindowSize :: Window -> V2 CInt -> IO ()
setWindowSize (Window win) (V2 w h) = Raw.setWindowSize win w h

-- | Get the current size of the window.
getWindowSize :: Window -> IO (V2 CInt)
getWindowSize (Window w) =
  alloca $ \wptr ->
  alloca $ \hptr -> do
    Raw.getWindowSize w wptr hptr
    V2 <$> peek wptr <*> peek hptr

-- | Set the title of the window.
setWindowTitle :: Window -> Text -> IO ()
setWindowTitle (Window w) title =
  BS.useAsCString (Text.encodeUtf8 title) $
    Raw.setWindowTitle w

-- | Get the text from the clipboard.
--
-- Throws 'SDLException' on failure.
getClipboardText :: IO Text
getClipboardText = mask_ $ do
  cstr <- throwIfNull "SDL.Video.getClipboardText" "SDL_GetClipboardText"
    Raw.getClipboardText
  finally (Text.decodeUtf8 <$> BS.packCString cstr) (free cstr)

-- | Checks if the clipboard exists, and has some text in it.
hasClipboardText :: IO Bool
hasClipboardText = Raw.hasClipboardText

-- | Replace the contents of the clipboard with the given text.
--
-- Throws 'SDLException' on failure.
setClipboardText :: Text -> IO ()
setClipboardText str = do
  throwIfNot0_ "SDL.Video.setClipboardText" "SDL_SetClipboardText" $
    BS.useAsCString (Text.encodeUtf8 str) Raw.setClipboardText

hideWindow :: Window -> IO ()
hideWindow (Window w) = Raw.hideWindow w

-- | Raise the window above other windows and set the input focus.
raiseWindow :: Window -> IO ()
raiseWindow (Window w) = Raw.raiseWindow w

-- | Disable screen savers.
disableScreenSaver :: IO ()
disableScreenSaver = Raw.disableScreenSaver

-- | Enable screen savers.
enableScreenSaver :: IO ()
enableScreenSaver = Raw.enableScreenSaver

-- | Check whether screen savers are enabled.
isScreenSaverEnabled :: IO Bool
isScreenSaverEnabled = Raw.isScreenSaverEnabled

showWindow :: Window -> IO ()
showWindow (Window w) = Raw.showWindow w

setWindowGammaRamp :: Window -> Maybe (SV.Vector Word16) -> Maybe (SV.Vector Word16) -> Maybe (SV.Vector Word16) -> IO ()
setWindowGammaRamp (Window w) r g b = do
  unless (all ((== 256) . SV.length) $ catMaybes [r,g,b]) $
    error "setWindowGammaRamp requires 256 element in each colour channel"

  let withChan x f = case x of Just x' -> SV.unsafeWith x' f
                               Nothing -> f nullPtr

  withChan r $ \rPtr ->
    withChan b $ \bPtr ->
      withChan g $ \gPtr ->
        throwIfNeg_ "SDL.Video.setWindowGammaRamp" "SDL_SetWindowGammaRamp" $
          Raw.setWindowGammaRamp w rPtr gPtr bPtr

data Display = Display {
               displayName           :: String
             , displayBoundsPosition :: Point V2 CInt
                 -- ^ Position of the desktop area represented by the display,
                 -- with the primary display located at @(0, 0)@.
             , displayBoundsSize     :: V2 CInt
                 -- ^ Size of the desktop area represented by the display.
             , displayModes          :: [DisplayMode]
             }
             deriving (Eq, Generic, Ord, Read, Show, Typeable)

data DisplayMode = DisplayMode {
                   displayModeFormat      :: PixelFormat
                 , displayModeSize        :: V2 CInt
                 , displayModeRefreshRate :: CInt -- ^ Display's refresh rate in hertz, or @0@ if unspecified.
                 }
                 deriving (Eq, Generic, Ord, Read, Show, Typeable)

data VideoDriver = VideoDriver {
                   videoDriverName :: String
                 }
                 deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Throws 'SDLException' on failure.
getDisplays :: IO [Display]
getDisplays = do
  numDisplays <- throwIfNeg "SDL.Video.getDisplays" "SDL_GetNumVideoDisplays"
    Raw.getNumVideoDisplays

  forM [0..numDisplays - 1] $ \displayId -> do
    name <- throwIfNull "SDL.Video.getDisplays" "SDL_GetDisplayName" $
        Raw.getDisplayName displayId

    name' <- peekCString name

    Raw.Rect x y w h <- alloca $ \rect -> do
      throwIfNot0_ "SDL.Video.getDisplays" "SDL_GetDisplayBounds" $
        Raw.getDisplayBounds displayId rect
      peek rect

    numModes <- throwIfNeg "SDL.Video.getDisplays" "SDL_GetNumDisplayModes" $
      Raw.getNumDisplayModes displayId

    modes <- forM [0..numModes - 1] $ \modeId -> do
      Raw.DisplayMode format w' h' refreshRate _ <- alloca $ \mode -> do
        throwIfNot0_ "SDL.Video.getDisplays" "SDL_GetDisplayMode" $
          Raw.getDisplayMode displayId modeId mode
        peek mode

      return $ DisplayMode {
          displayModeFormat = fromNumber format
        , displayModeSize = V2 w' h'
        , displayModeRefreshRate = refreshRate
      }

    return $ Display {
        displayName = name'
      , displayBoundsPosition = P (V2 x y)
      , displayBoundsSize = V2 w h
      , displayModes = modes
    }

-- | Show a simple message box with the given title and a message. Consider
-- writing your messages to @stderr@ too.
--
-- Throws 'SDLException' if there are no available video targets.
showSimpleMessageBox :: Maybe Window -> MessageKind -> Text -> Text -> IO ()
showSimpleMessageBox window kind title message =
  throwIfNot0_ "SDL.Video.showSimpleMessageBox" "SDL_ShowSimpleMessageBox" $ do
    BS.useAsCString (Text.encodeUtf8 title) $ \title' ->
      BS.useAsCString (Text.encodeUtf8 message) $ \message' ->
        Raw.showSimpleMessageBox (toNumber kind) title' message' $
          windowId window
  where
    windowId (Just (Window w)) = w
    windowId Nothing = nullPtr

data MessageKind
  = Error
  | Warning
  | Information
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance ToNumber MessageKind Word32 where
  toNumber Error = Raw.SDL_MESSAGEBOX_ERROR
  toNumber Warning = Raw.SDL_MESSAGEBOX_WARNING
  toNumber Information = Raw.SDL_MESSAGEBOX_INFORMATION

setWindowMaximumSize :: Window -> V2 CInt -> IO ()
setWindowMaximumSize (Window win) (V2 w h) = Raw.setWindowMaximumSize win w h

setWindowMinimumSize :: Window -> V2 CInt -> IO ()
setWindowMinimumSize (Window win) (V2 w h) = Raw.setWindowMinimumSize win w h

getWindowMaximumSize :: Window -> IO (V2 CInt)
getWindowMaximumSize (Window w) =
  alloca $ \wptr ->
  alloca $ \hptr -> do
    Raw.getWindowMaximumSize w wptr hptr
    V2 <$> peek wptr <*> peek hptr

getWindowMinimumSize :: Window -> IO (V2 CInt)
getWindowMinimumSize (Window w) =
  alloca $ \wptr ->
  alloca $ \hptr -> do
    Raw.getWindowMinimumSize w wptr hptr
    V2 <$> peek wptr <*> peek hptr

createRenderer :: Window -> CInt -> RendererConfig -> IO Renderer
createRenderer (Window w) driver config =
  fmap Renderer $
    throwIfNull "SDL.Video.createRenderer" "SDL_CreateRenderer" $
    Raw.createRenderer w driver (toNumber config)

destroyRenderer :: Renderer -> IO ()
destroyRenderer (Renderer r) = Raw.destroyRenderer r
