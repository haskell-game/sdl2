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
  , windowMinimumSize
  , windowMaximumSize
  , windowSize
  , windowBordered
  , windowBrightness
  , windowGammaRamp
  , windowGrab
  , setWindowMode
  , getWindowAbsolutePosition
  , setWindowPosition
  , windowTitle
  , windowData
  , getWindowConfig
  , getWindowPixelFormat
  , PixelFormat(..)

  -- * Renderer Management
  , createRenderer
  , createSoftwareRenderer
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
  , screenSaverEnabled

  -- * Message Box
  , showSimpleMessageBox
  , MessageKind(..)
  ) where

import Prelude hiding (all, foldl, foldr, mapM_)

import Data.StateVar
import Control.Applicative
import Control.Exception
import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Data (Data)
import Data.Foldable
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (First(..))
import Data.Text (Text)
import Data.Typeable
import Foreign hiding (void, throwIfNull, throwIfNeg, throwIfNeg_)
import Foreign.C
import GHC.Generics (Generic)
import SDL.Vect
import SDL.Internal.Exception
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
createWindow :: MonadIO m => Text -> WindowConfig -> m Window
createWindow title config = liftIO $ do
  case windowOpenGL config of
    Just glcfg -> setGLAttributes glcfg
    Nothing    -> return ()

  BS.useAsCString (Text.encodeUtf8 title) $ \title' -> do
    let create = Raw.createWindow title'
    let create' (V2 w h) = case windowPosition config of
          Centered -> let u = Raw.SDL_WINDOWPOS_CENTERED in create u u w h
          Wherever -> let u = Raw.SDL_WINDOWPOS_UNDEFINED in create u u w h
          Absolute (P (V2 x y)) -> create x y w h
    create' (windowInitialSize config) flags >>= return . Window
  where
    flags = foldr (.|.) 0
      [ if windowBorder config then 0 else Raw.SDL_WINDOW_BORDERLESS
      , if windowHighDPI config then Raw.SDL_WINDOW_ALLOW_HIGHDPI else 0
      , if windowInputGrabbed config then Raw.SDL_WINDOW_INPUT_GRABBED else 0
      , toNumber $ windowMode config
      , if isJust $ windowOpenGL config then Raw.SDL_WINDOW_OPENGL else 0
      , if windowResizable config then Raw.SDL_WINDOW_RESIZABLE else 0
      , if windowVisible config then 0 else Raw.SDL_WINDOW_HIDDEN
      , if windowVulkan config then Raw.SDL_WINDOW_VULKAN else 0
      ]
    setGLAttributes (OpenGLConfig (V4 r g b a) d s ms p) = do
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
        , (Raw.SDL_GL_MULTISAMPLEBUFFERS, if ms > 1 then 1 else 0)
        , (Raw.SDL_GL_MULTISAMPLESAMPLES, if ms > 1 then ms else 0)
        , (Raw.SDL_GL_CONTEXT_PROFILE_MASK, msk)
        , (Raw.SDL_GL_CONTEXT_MAJOR_VERSION, v0)
        , (Raw.SDL_GL_CONTEXT_MINOR_VERSION, v1)
        , (Raw.SDL_GL_CONTEXT_FLAGS, flg)
        ]

-- | Default configuration for windows. Use the record update syntax to
-- override any of the defaults.
--
-- @
-- 'defaultWindow' = 'WindowConfig'
--   { 'windowBorder'       = True
--   , 'windowHighDPI'      = False
--   , 'windowInputGrabbed' = False
--   , 'windowMode'         = 'Windowed'
--   , 'windowOpenGL'       = Nothing
--   , 'windowPosition'     = 'Wherever'
--   , 'windowResizable'    = False
--   , 'windowInitialSize'  = V2 800 600
--   , 'windowVisible'      = True
--   , 'windowVulkan'       = False
--   }
-- @
defaultWindow :: WindowConfig
defaultWindow = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = False
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = False
  , windowInitialSize  = V2 800 600
  , windowVisible      = True
  , windowVulkan       = False
  }

data WindowConfig = WindowConfig
  { windowBorder       :: Bool               -- ^ Defaults to 'True'.
  , windowHighDPI      :: Bool               -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowInputGrabbed :: Bool               -- ^ Defaults to 'False'. Whether the mouse shall be confined to the window.
  , windowMode         :: WindowMode         -- ^ Defaults to 'Windowed'.
  , windowOpenGL       :: Maybe OpenGLConfig -- ^ Defaults to 'Nothing'. Can not be changed after window creation.
  , windowPosition     :: WindowPosition     -- ^ Defaults to 'Wherever'.
  , windowResizable    :: Bool               -- ^ Defaults to 'False'. Whether the window can be resized by the user. It is still possible to programatically change the size by changing 'windowSize'.
  , windowInitialSize  :: V2 CInt            -- ^ Defaults to @(800, 600)@. If you set 'windowHighDPI' flag, window size in screen coordinates may differ from the size in pixels. Use 'glGetDrawableSize' or 'SDL.Video.Vulkan.vkGetDrawableSize' to get size in pixels.
  , windowVisible      :: Bool               -- ^ Defaults to 'True'.
  , windowVulkan       :: Bool               -- ^ Defaults to 'False'. If 'True', function 'SDL.Video.Vulkan.vkLoadLibrary' 'Nothing' will be called automatically before first window creation, and 'SDL.Video.Vulkan.vkUnloadLibrary' will be called after last window destruction.
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

instance FromNumber WindowMode Word32 where
  fromNumber n = fromMaybe Windowed . getFirst $
    foldMap First [
        sdlWindowFullscreen
      , sdlWindowFullscreenDesktop
      , sdlWindowMaximized
      , sdlWindowMinimized
      ]
    where
      maybeBit val msk = if n .&. msk > 0 then Just val else Nothing
      sdlWindowFullscreen        = maybeBit Fullscreen Raw.SDL_WINDOW_FULLSCREEN
      sdlWindowFullscreenDesktop = maybeBit FullscreenDesktop Raw.SDL_WINDOW_FULLSCREEN_DESKTOP
      sdlWindowMaximized         = maybeBit Maximized Raw.SDL_WINDOW_MAXIMIZED
      sdlWindowMinimized         = maybeBit Minimized Raw.SDL_WINDOW_MINIMIZED

data WindowPosition
  = Centered
  | Wherever -- ^ Let the window mananger decide where it's best to place the window.
  | Absolute (Point V2 CInt)
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

-- | Destroy the given window. The 'Window' handler may not be used
-- afterwards.
destroyWindow :: MonadIO m => Window -> m ()
destroyWindow (Window w) = Raw.destroyWindow w

-- | Get or set if the window should have a border.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
windowBordered :: Window -> StateVar Bool
windowBordered (Window w) = makeStateVar getWindowBordered setWindowBordered
  where
  getWindowBordered = fmap ((== 0) . (.&. Raw.SDL_WINDOW_BORDERLESS)) (Raw.getWindowFlags w)
  setWindowBordered = Raw.setWindowBordered w

-- | Get or set the window's brightness, where 0.0 is completely dark and 1.0 is normal brightness.
--
-- Throws 'SDLException' if the hardware does not support gamma
-- correction, or if the system has run out of memory.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
windowBrightness :: Window -> StateVar Float
windowBrightness (Window w) = makeStateVar getWindowBrightness setWindowBrightness
  where
  setWindowBrightness brightness = do
    throwIfNot0_ "SDL.Video.setWindowBrightness" "SDL_SetWindowBrightness" $
      Raw.setWindowBrightness w $ realToFrac brightness

  getWindowBrightness =
      return . realToFrac =<< Raw.getWindowBrightness w

-- | Get or set whether the mouse shall be confined to the window.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
windowGrab :: Window -> StateVar Bool
windowGrab (Window w) = makeStateVar getWindowGrab setWindowGrab
  where
  setWindowGrab = Raw.setWindowGrab w
  getWindowGrab = Raw.getWindowGrab w

-- | Change between window modes.
--
-- Throws 'SDLException' on failure.
setWindowMode :: MonadIO m => Window -> WindowMode -> m ()
setWindowMode (Window w) mode =
  liftIO . throwIfNot0_ "SDL.Video.setWindowMode" "SDL_SetWindowFullscreen" $
    case mode of
      Fullscreen -> Raw.setWindowFullscreen w Raw.SDL_WINDOW_FULLSCREEN <* Raw.raiseWindow w
      FullscreenDesktop -> Raw.setWindowFullscreen w Raw.SDL_WINDOW_FULLSCREEN_DESKTOP <* Raw.raiseWindow w
      Maximized -> Raw.setWindowFullscreen w 0 <* Raw.maximizeWindow w
      Minimized -> Raw.minimizeWindow w >> return 0
      Windowed -> Raw.setWindowFullscreen w 0 <* Raw.restoreWindow w

-- | Set the position of the window.
setWindowPosition :: MonadIO m => Window -> WindowPosition -> m ()
setWindowPosition (Window w) pos = case pos of
  Centered -> let u = Raw.SDL_WINDOWPOS_CENTERED in Raw.setWindowPosition w u u
  Wherever -> let u = Raw.SDL_WINDOWPOS_UNDEFINED in Raw.setWindowPosition w u u
  Absolute (P (V2 x y)) -> Raw.setWindowPosition w x y

-- | Get the position of the window.
getWindowAbsolutePosition :: MonadIO m => Window -> m (V2 CInt)
getWindowAbsolutePosition (Window w) =
    liftIO $
    alloca $ \wPtr ->
    alloca $ \hPtr -> do
        Raw.getWindowPosition w wPtr hPtr
        V2 <$> peek wPtr <*> peek hPtr

-- | Get or set the size of a window's client area. Values beyond the maximum supported size are clamped.
--
-- If window was created with 'windowHighDPI' flag, this size may differ from the size in pixels.
-- Use 'glGetDrawableSize' or 'SDL.Video.Vulkan.vkGetDrawableSize' to get size in pixels.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetWindowSize SDL_SetWindowSize>@ and @<https://wiki.libsdl.org/SDL_GetWindowSize SDL_GetWindowSize>@ for C documentation.
windowSize :: Window -> StateVar (V2 CInt)
windowSize (Window win) = makeStateVar getWindowSize setWindowSize
  where
  setWindowSize (V2 w h) = Raw.setWindowSize win w h

  getWindowSize =
    liftIO $
    alloca $ \wptr ->
    alloca $ \hptr -> do
      Raw.getWindowSize win wptr hptr
      V2 <$> peek wptr <*> peek hptr

-- | Get or set the title of the window. If the window has no title, then an empty string is returned.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetWindowTitle SDL_SetWindowTitle>@ and @<https://wiki.libsdl.org/SDL_GetWindowTitle SDL_GetWindowTitle>@ for C documentation.
windowTitle :: Window -> StateVar Text
windowTitle (Window w) = makeStateVar getWindowTitle setWindowTitle
  where
  setWindowTitle title =
    liftIO . BS.useAsCString (Text.encodeUtf8 title) $
      Raw.setWindowTitle w

  getWindowTitle = liftIO $ do
      cstr <- Raw.getWindowTitle w
      Text.decodeUtf8 <$> BS.packCString cstr

-- | Get or set the pointer to arbitrary user data associated with the given
-- window and name.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
windowData :: Window -> CString -> StateVar (Ptr ())
windowData (Window w) key = makeStateVar getWindowData setWindowData
  where
  setWindowData = void . Raw.setWindowData w key
  getWindowData = Raw.getWindowData w key

-- | Retrieve the configuration of the given window.
--
-- Note that 'Nothing' will be returned instead of potential OpenGL parameters
-- used during the creation of the window.
getWindowConfig :: MonadIO m => Window -> m WindowConfig
getWindowConfig (Window w) = do
    wFlags <- Raw.getWindowFlags w

    wSize <- get (windowSize (Window w))
    wPos  <- getWindowAbsolutePosition (Window w)

    return WindowConfig {
        windowBorder       = wFlags .&. Raw.SDL_WINDOW_BORDERLESS == 0
      , windowHighDPI      = wFlags .&. Raw.SDL_WINDOW_ALLOW_HIGHDPI > 0
      , windowInputGrabbed = wFlags .&. Raw.SDL_WINDOW_INPUT_GRABBED > 0
      , windowMode         = fromNumber wFlags
        -- Should we store the openGL config that was used to create the window?
      , windowOpenGL       = Nothing
      , windowPosition     = Absolute (P wPos)
      , windowResizable    = wFlags .&. Raw.SDL_WINDOW_RESIZABLE > 0
      , windowInitialSize  = wSize
      , windowVisible      = wFlags .&. Raw.SDL_WINDOW_SHOWN > 0
      , windowVulkan       = wFlags .&. Raw.SDL_WINDOW_VULKAN > 0
    }

-- | Get the pixel format that is used for the given window.
getWindowPixelFormat :: MonadIO m => Window -> m PixelFormat
getWindowPixelFormat (Window w) = return . fromNumber =<< Raw.getWindowPixelFormat w

-- | Get the text from the clipboard.
--
-- Throws 'SDLException' on failure.
getClipboardText :: MonadIO m => m Text
getClipboardText = liftIO . mask_ $ do
  cstr <- throwIfNull "SDL.Video.getClipboardText" "SDL_GetClipboardText"
    Raw.getClipboardText
  finally (Text.decodeUtf8 <$> BS.packCString cstr) (free cstr)

-- | Checks if the clipboard exists, and has some text in it.
hasClipboardText :: MonadIO m => m Bool
hasClipboardText = Raw.hasClipboardText

-- | Replace the contents of the clipboard with the given text.
--
-- Throws 'SDLException' on failure.
setClipboardText :: MonadIO m => Text -> m ()
setClipboardText str = liftIO $ do
  throwIfNot0_ "SDL.Video.setClipboardText" "SDL_SetClipboardText" $
    BS.useAsCString (Text.encodeUtf8 str) Raw.setClipboardText

-- | Hide a window.
--
-- See @<https://wiki.libsdl.org/SDL_HideWindow SDL_HideWindow>@ for C documentation.
hideWindow :: MonadIO m => Window -> m ()
hideWindow (Window w) = Raw.hideWindow w

-- | Raise the window above other windows and set the input focus.
--
-- See @<https://wiki.libsdl.org/SDL_RaiseWindow SDL_RaiseWindow>@ for C documentation.
raiseWindow :: MonadIO m => Window -> m ()
raiseWindow (Window w) = Raw.raiseWindow w

-- | Get or set whether to allow the screen to be blanked by a screen saver.
--
-- Screen savers are re-enabled, if needed, when SDL quits.
screenSaverEnabled :: StateVar Bool
screenSaverEnabled = makeStateVar (isScreenSaverEnabled) (setScreenSaverEnabled)
  where
  isScreenSaverEnabled = Raw.isScreenSaverEnabled

  setScreenSaverEnabled True = Raw.enableScreenSaver
  setScreenSaverEnabled False = Raw.disableScreenSaver

-- | Show a window.
--
-- See @<https://wiki.libsdl.org/SDL_ShowWindow SDL_ShowWindow>@ for C documentation.
showWindow :: MonadIO m => Window -> m ()
showWindow (Window w) = Raw.showWindow w

-- | Gets or sets the gamma ramp for the display that owns a given window.
--
-- Note that the data for the gamma ramp - the 'V3' ('SV.Vector' 'Word16') - must contain 256 element arrays. This triple is a set of translation vectors for each of the 16-bit red, green and blue channels.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- Despite the name and signature, this method retrieves the gamma ramp of the entire display, not an individual window. A window is considered to be owned by the display that contains the window's center pixel.
windowGammaRamp :: Window -> StateVar (V3 (SV.Vector Word16))
windowGammaRamp (Window w) = makeStateVar getWindowGammaRamp setWindowGammaRamp
  where
  getWindowGammaRamp =
    allocaArray 256 $ \rPtr ->
    allocaArray 256 $ \gPtr ->
    allocaArray 256 $ \bPtr -> do
      throwIfNeg_ "SDL.Video.getWindowGammaRamp" "SDL_GetWindowGammaRamp"
        (Raw.getWindowGammaRamp w rPtr gPtr bPtr)
      liftA3 V3 (fmap SV.fromList (peekArray 256 rPtr))
                (fmap SV.fromList (peekArray 256 gPtr))
                (fmap SV.fromList (peekArray 256 bPtr))


  setWindowGammaRamp (V3 r g b) = liftIO $ do
    unless (all ((== 256) . SV.length) [r,g,b]) $
      error "setWindowGammaRamp requires 256 element in each colour channel"

    SV.unsafeWith r $ \rPtr ->
      SV.unsafeWith b $ \bPtr ->
        SV.unsafeWith g $ \gPtr ->
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
getDisplays :: MonadIO m => m [Display]
getDisplays = liftIO $ do
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
showSimpleMessageBox :: MonadIO m => Maybe Window -> MessageKind -> Text -> Text -> m ()
showSimpleMessageBox window kind title message =
  liftIO . throwIfNot0_ "SDL.Video.showSimpleMessageBox" "SDL_ShowSimpleMessageBox" $ do
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

-- | Get or set the maximum size of a window's client area.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetWindowMaximumSize SDL_SetWindowMaximumSize>@ and @<https://wiki.libsdl.org/SDL_GetWindowMaximumSize SDL_GetWindowMaximumSize>@ for C documentation.
windowMaximumSize :: Window -> StateVar (V2 CInt)
windowMaximumSize (Window win) = makeStateVar getWindowMaximumSize setWindowMaximumSize
  where
  setWindowMaximumSize (V2 w h) = Raw.setWindowMaximumSize win w h

  getWindowMaximumSize =
    liftIO $
    alloca $ \wptr ->
    alloca $ \hptr -> do
      Raw.getWindowMaximumSize win wptr hptr
      V2 <$> peek wptr <*> peek hptr

-- | Get or set the minimum size of a window's client area.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetWindowMinimumSize SDL_SetWindowMinimumSize>@ and @<https://wiki.libsdl.org/SDL_GetWindowMinimumSize SDL_GetWindowMinimumSize>@ for C documentation.
windowMinimumSize :: Window -> StateVar (V2 CInt)
windowMinimumSize (Window win) = makeStateVar getWindowMinimumSize setWindowMinimumSize
  where
  setWindowMinimumSize (V2 w h) = Raw.setWindowMinimumSize win w h

  getWindowMinimumSize =
    liftIO $
    alloca $ \wptr ->
    alloca $ \hptr -> do
      Raw.getWindowMinimumSize win wptr hptr
      V2 <$> peek wptr <*> peek hptr

createRenderer :: MonadIO m => Window -> CInt -> RendererConfig -> m Renderer
createRenderer (Window w) driver config =
  liftIO . fmap Renderer $
    throwIfNull "SDL.Video.createRenderer" "SDL_CreateRenderer" $
    Raw.createRenderer w driver (toNumber config)

-- | Create a 2D software rendering context for the given surface.
--
-- See @<https://wiki.libsdl.org/SDL_CreateSoftwareRenderer>@
createSoftwareRenderer :: MonadIO m => Surface -> m Renderer
createSoftwareRenderer (Surface ptr _) =
  liftIO . fmap Renderer $
    throwIfNull "SDL.Video.createSoftwareRenderer" "SDL_CreateSoftwareRenderer" $
    Raw.createSoftwareRenderer ptr

destroyRenderer :: MonadIO m => Renderer -> m ()
destroyRenderer (Renderer r) = Raw.destroyRenderer r
