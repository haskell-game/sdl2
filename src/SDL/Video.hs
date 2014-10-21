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
  , getWindowMinimumSize
  , getWindowMaximumSize
  , setWindowBordered
  , setWindowBrightness
  , setWindowGammaRamp
  , setWindowGrab
  , setWindowMode
  , setWindowMaximumSize
  , setWindowMinimumSize
  , setWindowPosition
  , setWindowSize
  , setWindowTitle

  -- * Renderer Management
  , RendererConfig(..)
  , createRenderer
  , defaultRenderer
  , destroyRenderer
  , RendererInfo(..)
  , getRendererInfo

  -- * Clipboard Handling
  , getClipboardText
  , hasClipboardText
  , setClipboardText

  -- * Display
  , getDisplays
  , Display(..)
  , DisplayMode(..)
  , VideoDriver(..)
  , PixelFormat(..)

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

import Prelude hiding (all, foldl, foldr)

import Control.Applicative
import Control.Exception
import Control.Monad (forM, unless)
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Foreign hiding (void, throwIfNull, throwIfNeg, throwIfNeg_)
import Foreign.C
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
createWindow title config =
  BS.useAsCString (Text.encodeUtf8 title) $ \title' -> do
    let create = Raw.createWindow title'
    let create' (V2 w h) = case windowPosition config of
          Centered -> create Raw.windowPosCentered Raw.windowPosCentered w h
          Wherever -> create Raw.windowPosUndefined Raw.windowPosUndefined w h
          Absolute (P (V2 x y)) -> create x y w h
    create' (windowSize config) flags >>= return . Window
  where
    flags = foldr (.|.) 0
      [ if windowBorder config then 0 else Raw.windowFlagBorderless
      , if windowHighDPI config then Raw.windowFlagAllowHighDPI else 0
      , if windowInputGrabbed config then Raw.windowFlagInputGrabbed else 0
      , toNumber $ windowMode config
      , if windowOpenGL config then Raw.windowFlagOpenGL else 0
      , if windowResizable config then Raw.windowFlagResizable else 0
      ]

-- | Default configuration for windows. Use the record update syntax to
-- override any of the defaults.
defaultWindow :: WindowConfig
defaultWindow = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = False
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = False
  , windowPosition     = Wherever
  , windowResizable    = False
  , windowSize         = V2 800 600
  }

data WindowConfig = WindowConfig
  { windowBorder       :: Bool           -- ^ Defaults to 'True'.
  , windowHighDPI      :: Bool           -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowInputGrabbed :: Bool           -- ^ Defaults to 'False'. Whether the mouse shall be confined to the window.
  , windowMode         :: WindowMode     -- ^ Defaults to 'Windowed'.
  , windowOpenGL       :: Bool           -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowPosition     :: WindowPosition -- ^ Defaults to 'Wherever'.
  , windowResizable    :: Bool           -- ^ Defaults to 'False'. Whether the window can be resized by the user. It is still possible to programatically change the size with 'setWindowSize'.
  , windowSize         :: V2 CInt        -- ^ Defaults to @(800, 600)@.
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
setWindowSize :: Window -> V2 CInt -> IO ()
setWindowSize (Window win) (V2 w h) = Raw.setWindowSize win w h

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

data PixelFormat = Unknown
                 | Index1LSB
                 | Index1MSB
                 | Index4LSB
                 | Index4MSB
                 | Index8
                 | RGB332
                 | RGB444
                 | RGB555
                 | BGR555
                 | ARGB4444
                 | RGBA4444
                 | ABGR4444
                 | BGRA4444
                 | ARGB1555
                 | RGBA5551
                 | ABGR1555
                 | BGRA5551
                 | RGB565
                 | BGR565
                 | RGB24
                 | BGR24
                 | RGB888
                 | RGBX8888
                 | BGR888
                 | BGRX8888
                 | ARGB8888
                 | RGBA8888
                 | ABGR8888
                 | BGRA8888
                 | ARGB2101010
                 | YV12
                 | IYUV
                 | YUY2
                 | UYVY
                 | YVYU
                 deriving (Eq, Show)

instance FromNumber PixelFormat Word32 where
  fromNumber n' = case n' of
    n | n == Raw.pixelFormatUnknown -> Unknown
    n | n == Raw.pixelFormatIndex1LSB -> Index1LSB
    n | n == Raw.pixelFormatIndex1MSB -> Index1MSB
    n | n == Raw.pixelFormatIndex4LSB -> Index4LSB
    n | n == Raw.pixelFormatIndex4MSB -> Index4MSB
    n | n == Raw.pixelFormatIndex8 -> Index8
    n | n == Raw.pixelFormatRGB332 -> RGB332
    n | n == Raw.pixelFormatRGB444 -> RGB444
    n | n == Raw.pixelFormatRGB555 -> RGB555
    n | n == Raw.pixelFormatBGR555 -> BGR555
    n | n == Raw.pixelFormatARGB4444 -> ARGB4444
    n | n == Raw.pixelFormatRGBA4444 -> RGBA4444
    n | n == Raw.pixelFormatABGR4444 -> ABGR4444
    n | n == Raw.pixelFormatBGRA4444 -> BGRA4444
    n | n == Raw.pixelFormatARGB1555 -> ARGB1555
    n | n == Raw.pixelFormatRGBA5551 -> RGBA5551
    n | n == Raw.pixelFormatABGR1555 -> ABGR1555
    n | n == Raw.pixelFormatBGRA5551 -> BGRA5551
    n | n == Raw.pixelFormatRGB565 -> RGB565
    n | n == Raw.pixelFormatBGR565 -> BGR565
    n | n == Raw.pixelFormatRGB24 -> RGB24
    n | n == Raw.pixelFormatBGR24 -> BGR24
    n | n == Raw.pixelFormatRGB888 -> RGB888
    n | n == Raw.pixelFormatRGBX8888 -> RGBX8888
    n | n == Raw.pixelFormatBGR888 -> BGR888
    n | n == Raw.pixelFormatBGRX8888 -> BGRX8888
    n | n == Raw.pixelFormatARGB8888 -> ARGB8888
    n | n == Raw.pixelFormatRGBA8888 -> RGBA8888
    n | n == Raw.pixelFormatABGR8888 -> ABGR8888
    n | n == Raw.pixelFormatBGRA8888 -> BGRA8888
    n | n == Raw.pixelFormatARGB2101010 -> ARGB2101010
    n | n == Raw.pixelFormatYV12 -> YV12
    n | n == Raw.pixelFormatIYUV -> IYUV
    n | n == Raw.pixelFormatYUY2 -> YUY2
    n | n == Raw.pixelFormatUYVY -> UYVY
    n | n == Raw.pixelFormatYVYU -> YVYU
    _ -> error "fromNumber: not numbered"

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

data RendererConfig = RendererConfig
  { rendererSoftware      :: Bool
  , rendererAccelerated   :: Bool
  , rendererPresentVSync  :: Bool
  , rendererTargetTexture :: Bool
  } deriving (Eq, Show)

instance FromNumber RendererConfig Word32 where
  fromNumber n = RendererConfig
    { rendererSoftware      = n .&. Raw.rendererFlagSoftware /= 0
    , rendererAccelerated   = n .&. Raw.rendererFlagAccelerated /= 0
    , rendererPresentVSync  = n .&. Raw.rendererFlagPresentVSync /= 0
    , rendererTargetTexture = n .&. Raw.rendererFlagTargetTexture /= 0
    }

instance ToNumber RendererConfig Word32 where
  toNumber config = foldr (.|.) 0
    [ if rendererSoftware config then Raw.rendererFlagSoftware else 0
    , if rendererAccelerated config then Raw.rendererFlagAccelerated else 0
    , if rendererPresentVSync config then Raw.rendererFlagPresentVSync else 0
    , if rendererTargetTexture config then Raw.rendererFlagTargetTexture else 0
    ]

defaultRenderer :: RendererConfig
defaultRenderer = RendererConfig
  { rendererSoftware      = False
  , rendererAccelerated   = True
  , rendererPresentVSync  = False
  , rendererTargetTexture = False
  }

createRenderer :: Window -> CInt -> RendererConfig -> IO Renderer
createRenderer (Window w) driver config = do
  fmap Renderer $
    throwIfNull "SDL.Video.createRenderer" "SDL_CreateRenderer" $
    Raw.createRenderer w driver (toNumber config)

destroyRenderer :: Renderer -> IO ()
destroyRenderer (Renderer r) = Raw.destroyRenderer r

data RendererInfo = RendererInfo
  { rendererInfoName              :: Text
  , rendererInfoFlags             :: RendererConfig
  , rendererInfoNumTextureFormats :: Word32
  , rendererInfoTextureFormats    :: [PixelFormat]
  , rendererInfoMaxTextureWidth   :: CInt
  , rendererInfoMaxTextureHeight  :: CInt
  } deriving (Eq, Show)

getRendererInfo :: Renderer -> IO RendererInfo
getRendererInfo (Renderer renderer) =
  alloca $ \rptr -> do
    throwIfNeg_ "getRendererInfo" "SDL_GetRendererInfo" $
      Raw.getRendererInfo renderer rptr
    (Raw.RendererInfo name flgs ntf tfs mtw mth) <- peek rptr
    name' <- Text.decodeUtf8 <$> BS.packCString name
    return $ RendererInfo name' (fromNumber flgs) ntf (fmap fromNumber tfs) mtw mth
