{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , windowBordered
  , windowBrightness
  , windowGammaRamp
  , windowGrab
  , windowMaximumSize
  , windowMinimumSize
  , windowSize
  , windowTitle
  , setWindowMode
  , setWindowPosition

  -- * Renderer Management
  , createRenderer
  , destroyRenderer

  -- * Clipboard Handling
  , clipboardText
  , hasClipboardText

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

import Prelude hiding (all, foldl, foldr)

import Control.Applicative
import Control.Exception
import Control.Lens ((&), (%@~), traversed)
import Control.Monad (forM, unless)
import Data.Foldable
import Data.Maybe (catMaybes, fromMaybe)
import Data.StateVar hiding (GettableStateVar, get, makeGettableStateVar)
import Data.Text (Text)
import Foreign hiding (void, throwIfNull, throwIfNeg, throwIfNeg_)
import Foreign.C
import Linear
import Linear.Affine (Point(P))
import Linear.V
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types
import SDL.Video.OpenGL
import SDL.Video.Renderer

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified SDL.Raw as Raw

-- | Create a window with the given title and configuration.
--
-- Throws 'SDLException' on failure.
createWindow :: Text -> WindowConfig -> IO Window
createWindow title config =
  BS.useAsCString (Text.encodeUtf8 title) $ \title' -> do
    let create = Raw.createWindow title'
    let create' (V2 w h) = case windowCfgPosition config of
          Centered -> create Raw.windowPosCentered Raw.windowPosCentered w h
          Wherever -> create Raw.windowPosUndefined Raw.windowPosUndefined w h
          Absolute (P (V2 x y)) -> create x y w h
    create' (windowCfgSize config) flags >>= return . Window
  where
    flags = foldr (.|.) 0
      [ if windowCfgBorder config then 0 else Raw.windowFlagBorderless
      , if windowCfgHighDPI config then Raw.windowFlagAllowHighDPI else 0
      , if windowCfgInputGrabbed config then Raw.windowFlagInputGrabbed else 0
      , toNumber $ windowCfgMode config
      , if windowCfgOpenGL config then Raw.windowFlagOpenGL else 0
      , if windowCfgResizable config then Raw.windowFlagResizable else 0
      ]

-- | Default configuration for windows. Use the record update syntax to
-- override any of the defaults.
defaultWindow :: WindowConfig
defaultWindow = WindowConfig
  { windowCfgBorder       = True
  , windowCfgHighDPI      = False
  , windowCfgInputGrabbed = False
  , windowCfgMode         = Windowed
  , windowCfgOpenGL       = False
  , windowCfgPosition     = Wherever
  , windowCfgResizable    = False
  , windowCfgSize         = V2 800 600
  }

data WindowConfig = WindowConfig
  { windowCfgBorder       :: Bool           -- ^ Defaults to 'True'.
  , windowCfgHighDPI      :: Bool           -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowCfgInputGrabbed :: Bool           -- ^ Defaults to 'False'. Whether the mouse shall be confined to the window.
  , windowCfgMode         :: WindowMode     -- ^ Defaults to 'Windowed'.
  , windowCfgOpenGL       :: Bool           -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowCfgPosition     :: WindowPosition -- ^ Defaults to 'Wherever'.
  , windowCfgResizable    :: Bool           -- ^ Defaults to 'False'. Whether the window can be resized by the user. It is still possible to programatically change the size with 'setWindowSize'.
  , windowCfgSize         :: V2 CInt        -- ^ Defaults to @(800, 600)@.
  } deriving (Eq, Show)

data WindowMode
  = Fullscreen        -- ^ Real fullscreen with a video mode change
  | FullscreenDesktop -- ^ Fake fullscreen that takes the size of the desktop
  | Maximized
  | Minimized
  | Windowed
  deriving (Eq, Show)

instance ToNumber WindowMode Word32 where
  toNumber Fullscreen = Raw.windowFlagFullscreen
  toNumber FullscreenDesktop = Raw.windowFlagFullscreenDesktop
  toNumber Maximized = Raw.windowFlagMaximized
  toNumber Minimized = Raw.windowFlagMinimized
  toNumber Windowed = 0

data WindowPosition
  = Centered
  | Wherever -- ^ Let the window mananger decide where it's best to place the window.
  | Absolute (Point V2 CInt)
  deriving (Eq, Show)

-- | Destroy the given window. The 'Window' handler may not be used
-- afterwards.
destroyWindow :: Window -> IO ()
destroyWindow (Window w) = Raw.destroyWindow w

-- | Set whether the window should have a border or not.
windowBordered :: Window -> StateVar Bool
windowBordered (Window w) = makeStateVar get set
  where
  get = do
    flags <- Raw.getWindowFlags w
    return (flags .&. Raw.windowFlagBorderless /= 0)
  set = Raw.setWindowBordered w

-- | Set the window's brightness, where 0.0 is completely dark and 1.0 is
-- normal brightness.
--
-- Throws 'SDLException' if the hardware does not support gamma
-- correction, or if the system has run out of memory.
windowBrightness :: Window -> StateVar Float
windowBrightness (Window w) = makeStateVar get set
  where
  get = realToFrac <$> Raw.getWindowBrightness w
  set = do
    throwIfNot0_ "SDL.Video.setWindowBrightness" "SDL_SetWindowBrightness" .
      Raw.setWindowBrightness w . realToFrac

-- | Set whether the mouse shall be confined to the window.
windowGrab :: Window -> StateVar Bool
windowGrab (Window w) = makeStateVar (Raw.getWindowGrab w) (Raw.setWindowGrab w)

-- | Change between window modes.
--
-- Throws 'SDLException' on failure.
setWindowMode :: Window -> WindowMode -> IO ()
setWindowMode (Window w) mode =
  throwIfNot0_ "SDL.Video.setWindowMode" "SDL_SetWindowFullscreen" $
    case mode of
      Fullscreen -> Raw.setWindowFullscreen w Raw.windowFlagFullscreen
      FullscreenDesktop -> Raw.setWindowFullscreen w Raw.windowFlagFullscreenDesktop
      Maximized -> Raw.setWindowFullscreen w 0 <* Raw.maximizeWindow w
      Minimized -> Raw.minimizeWindow w >> return 0
      Windowed -> Raw.restoreWindow w >> return 0

-- | Set the position of the window.
setWindowPosition :: Window -> WindowPosition -> IO ()
setWindowPosition (Window w) pos = case pos of
  Centered -> let u = Raw.windowPosCentered in Raw.setWindowPosition w u u
  Wherever -> let u = Raw.windowPosUndefined in Raw.setWindowPosition w u u
  Absolute (P (V2 x y)) -> Raw.setWindowPosition w x y

-- | Set the size of the window. Values beyond the maximum supported size are
-- clamped.
windowSize :: Window -> StateVar (V2 CInt)
windowSize (Window win) = makeStateVar get set
  where
  get =
    alloca $ \w ->
      alloca $ \h -> do
        Raw.getWindowSize win w h
        V2 <$> peek w <*> peek h

  set (V2 w h) = Raw.setWindowSize win w h

-- | Set the title of the window.
windowTitle :: Window -> StateVar Text
windowTitle (Window w) = makeStateVar get set
  where
  get = do
    cstr <- Raw.getWindowTitle w
    Text.decodeUtf8 <$> BS.packCString cstr

  set title =
    BS.useAsCString (Text.encodeUtf8 title) $
      Raw.setWindowTitle w

-- | Get the text from the clipboard.
--
-- Throws 'SDLException' on failure.
clipboardText :: StateVar Text
clipboardText = makeStateVar get set
  where
  get = mask_ $ do
    cstr <- throwIfNull "SDL.Video.getClipboardText" "SDL_GetClipboardText"
      Raw.getClipboardText
    finally (Text.decodeUtf8 <$> BS.packCString cstr) (free cstr)

  set str = do
    throwIfNot0_ "SDL.Video.setClipboardText" "SDL_SetClipboardText" $
      BS.useAsCString (Text.encodeUtf8 str) Raw.setClipboardText

-- | Checks if the clipboard exists, and has some text in it.
hasClipboardText :: IO Bool
hasClipboardText = Raw.hasClipboardText

hideWindow :: Window -> IO ()
hideWindow (Window w) = Raw.hideWindow w

-- | Raise the window above other windows and set the input focus.
raiseWindow :: Window -> IO ()
raiseWindow (Window w) = Raw.raiseWindow w

screenSaverEnabled :: StateVar Bool
screenSaverEnabled = makeStateVar Raw.isScreenSaverEnabled set
  where
  set True = Raw.enableScreenSaver
  set False = Raw.disableScreenSaver

showWindow :: Window -> IO ()
showWindow (Window w) = Raw.showWindow w

windowGammaRamp :: Window -> StateVar (V3 (Maybe (V 256 Word16)))
windowGammaRamp (Window win) = makeStateVar get set
  where
  set :: V3 (Maybe (V 256 Word16)) -> IO ()
  set (V3 r g b) = do
    let withChan :: Maybe (V 256 Word16) -> (Ptr Word16 -> IO ()) -> IO ()
        withChan x f =
          case x of
            Just x' -> with x' (f . castPtr)
            Nothing -> f nullPtr

    withChan r $ \rPtr ->
      withChan b $ \bPtr ->
        withChan g $ \gPtr ->
          throwIfNeg_ "SDL.Video.windowGammaRamp" "SDL_SetWindowGammaRamp" $
            Raw.setWindowGammaRamp win rPtr gPtr bPtr

  get :: IO (V3 (Maybe (V 256 Word16)))
  get =
    allocaArray 256 $ \rPtr ->
    allocaArray 256 $ \bPtr ->
    allocaArray 256 $ \gPtr -> do
      throwIfNeg_ "SDL.Video.windowGammaRamp" "SDL_GetWindowGammaRamp" $
        Raw.getWindowGammaRamp win rPtr gPtr bPtr

      let peekChan :: Ptr Word16 -> IO (V 256 Word16)
          peekChan ptr =
            fmap (fromMaybe (error "SDL_GetWindowGammaRamp returned a non-256 element array") . fromVector . V.fromList) $
            peekArray 256 ptr
      V3 <$> (Just <$> peekChan rPtr)
         <*> (Just <$> peekChan gPtr)
         <*> (Just <$> peekChan bPtr)

data Display = Display {
               displayName           :: String
             , displayBoundsPosition :: Point V2 CInt
                 -- ^ Position of the desktop area represented by the display,
                 -- with the primary display located at @(0, 0)@.
             , displayBoundsSize     :: V2 CInt
                 -- ^ Size of the desktop area represented by the display.
             , displayModes          :: [DisplayMode]
             }
             deriving (Eq, Show)

data DisplayMode = DisplayMode {
                   displayModeFormat      :: PixelFormat
                 , displayModeSize        :: V2 CInt
                 , displayModeRefreshRate :: CInt -- ^ Display's refresh rate in hertz, or @0@ if unspecified.
                 }
                 deriving (Eq, Show)

data VideoDriver = VideoDriver {
                   videoDriverName :: String
                 }
                 deriving (Eq, Show)

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
  deriving (Eq, Show)

instance ToNumber MessageKind Word32 where
  toNumber Error = Raw.messageBoxFlagError
  toNumber Warning = Raw.messageBoxFlagWarning
  toNumber Information = Raw.messageBoxFlagInformation

windowMaximumSize :: Window -> StateVar (V2 CInt)
windowMaximumSize (Window win) = makeStateVar get set
  where
  get =
    alloca $ \wptr ->
    alloca $ \hptr -> do
      Raw.getWindowMaximumSize win wptr hptr
      V2 <$> peek wptr <*> peek hptr

  set (V2 w h) = Raw.setWindowMaximumSize win w h

windowMinimumSize :: Window -> StateVar (V2 CInt)
windowMinimumSize (Window win) = makeStateVar get set
  where
  get =
    alloca $ \wptr ->
    alloca $ \hptr -> do
      Raw.getWindowMinimumSize win wptr hptr
      V2 <$> peek wptr <*> peek hptr

  set (V2 w h) = Raw.setWindowMinimumSize win w h

createRenderer :: Window -> CInt -> RendererConfig -> IO Renderer
createRenderer (Window w) driver config =
  fmap Renderer $
    throwIfNull "SDL.Video.createRenderer" "SDL_CreateRenderer" $
    Raw.createRenderer w driver (toNumber config)

destroyRenderer :: Renderer -> IO ()
destroyRenderer (Renderer r) = Raw.destroyRenderer r
