{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Video
  ( -- * Window Management
    Window
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
  , setWindowBordered
  , setWindowBrightness
  , setWindowGammaRamp
  , setWindowGrab
  , setWindowMode
  , setWindowPosition
  , setWindowSize
  , setWindowTitle

  -- * Clipboard Handling
  , getClipboardText
  , hasClipboardText
  , setClipboardText

  -- * Drawing Primitives
  , renderClear
  , renderDrawLine
  , renderDrawLines
  , renderDrawPoint
  , renderDrawPoints
  , renderDrawRect
  , renderDrawRects
  , renderFillRect
  , renderFillRects
  , renderSetClipRect
  , renderSetLogicalSize
  , renderSetScale
  , renderSetViewport
  , setRenderDrawBlendMode
  , setRenderDrawColor
  , BlendMode(..)
  , Rectangle(..)

  -- * Display
  , getDisplays
  , Display(..)
  , DisplayMode(..)
  , VideoDriver(..)
  , PixelFormat(..)

  -- * OpenGL
  , GLAttribute(..)
  , glSetAttribute
  , glSwapWindow
  , glGetCurrentContext
  , glCreateContext
  , Raw.glGetProcAddress
  , SwapInterval(..)
  , glSetSwapInterval
  , Raw.glResetAttributes

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

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as SV
import qualified SDL.Raw as Raw

newtype Window = Window (Raw.Window)
  deriving (Eq)

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
      , windowModeCtT $ windowMode config
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

windowModeCtT :: Num a => WindowMode -> a
windowModeCtT n' = case n' of
  Fullscreen -> Raw.windowFlagFullscreen
  FullscreenDesktop -> Raw.windowFlagFullscreenDesktop
  Maximized -> Raw.windowFlagMaximized
  Minimized -> Raw.windowFlagMinimized
  Windowed -> 0

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

newtype Renderer = Renderer Raw.Renderer

data GLAttribute
  = GLRedSize
  | GLGreenSize
  | GLBlueSize
  | GLAlphaSize
  | GLBufferSize
  | GLDoubleBuffer
  | GLDepthSize
  | GLStencilSize
  | GLAccumRedSize
  | GLAccumGreenSize
  | GLAccumBlueSize
  | GLAccumAlphaSize
  | GLStereo
  | GLMultiSampleBuffers
  | GLMultiSampleSamples
  | GLAcceleratedVisual
  | GLRetainedBacking
  | GLContextMajorVersion
  | GLContextMinorVersion
  | GLContextFlags
  | GLContextProfileMask
  | GLShareWithCurrentContext
  | GLFramebufferSRGBCapable
  | GLContextEGL

glAttributeToC :: GLAttribute -> Word32
glAttributeToC GLRedSize = Raw.glAttrRedSize
glAttributeToC GLGreenSize = Raw.glAttrGreenSize
glAttributeToC GLBlueSize = Raw.glAttrBlueSize
glAttributeToC GLAlphaSize = Raw.glAttrAlphaSize
glAttributeToC GLBufferSize = Raw.glAttrBufferSize
glAttributeToC GLDoubleBuffer = Raw.glAttrDoubleBuffer
glAttributeToC GLDepthSize = Raw.glAttrDepthSize
glAttributeToC GLStencilSize = Raw.glAttrStencilSize
glAttributeToC GLAccumRedSize = Raw.glAttrAccumRedSize
glAttributeToC GLAccumGreenSize = Raw.glAttrAccumGreenSize
glAttributeToC GLAccumBlueSize = Raw.glAttrAccumBlueSize
glAttributeToC GLAccumAlphaSize = Raw.glAttrAccumAlphaSize
glAttributeToC GLStereo = Raw.glAttrStereo
glAttributeToC GLMultiSampleBuffers = Raw.glAttrMultiSampleBuffers
glAttributeToC GLMultiSampleSamples = Raw.glAttrMultiSampleSamples
glAttributeToC GLAcceleratedVisual = Raw.glAttrAcceleratedVisual
glAttributeToC GLRetainedBacking = Raw.glAttrRetainedBacking
glAttributeToC GLContextMajorVersion = Raw.glAttrContextMajorVersion
glAttributeToC GLContextMinorVersion = Raw.glAttrContextMinorVersion
glAttributeToC GLContextFlags = Raw.glAttrContextFlags
glAttributeToC GLContextProfileMask = Raw.glAttrContextProfileMask
glAttributeToC GLShareWithCurrentContext = Raw.glAttrShareWithCurrentContext
glAttributeToC GLFramebufferSRGBCapable = Raw.glAttrFramebufferSRGBCapable
glAttributeToC GLContextEGL = Raw.glAttrContextEGL

glSetAttribute :: GLAttribute -> CInt -> IO ()
glSetAttribute attribute value =
  throwIfNeg_ "SDL.Video.glSetAttribute" "SDL_GL_SetAttribute" $
    Raw.glSetAttribute (glAttributeToC attribute) value

-- | Replace the contents of the front buffer with the back buffer's. The
-- contents of the back buffer are undefined, clear them with @glClear@ or
-- equivalent before drawing to them again.
glSwapWindow :: Window -> IO ()
glSwapWindow (Window w) = Raw.glSwapWindow w

newtype GLContext = GLContext (Raw.GLContext)

-- | Create a new OpenGL context and makes it the current context for the
-- window.
--
-- Throws 'SDLException' if the window wasn't configured with OpenGL
-- support, or if context creation fails.
glCreateContext :: Window -> IO GLContext
glCreateContext (Window w) = fmap GLContext $ throwIfNull "SDL.Video.glCreateContext" "SDL_GL_CreateContext" $ Raw.glCreateContext w

glGetCurrentContext :: IO GLContext
glGetCurrentContext = fmap GLContext $ throwIfNull "SDL.Video.glGetCurrentContext" "SDL_GL_GetCurrentContext" Raw.glGetCurrentContext

data SwapInterval = ImmediateUpdates | SynchronizedUpdates | LateSwapTearing

swapIntervalToC :: SwapInterval -> CInt
swapIntervalToC ImmediateUpdates = 0
swapIntervalToC SynchronizedUpdates = 1
swapIntervalToC LateSwapTearing = -1

glSetSwapInterval :: SwapInterval -> IO ()
glSetSwapInterval swapInterval =
  throwIfNeg_ "SDL.Video.glSetSwapInterval" "SDL_GL_SetSwapInterval" $
    Raw.glSetSwapInterval (swapIntervalToC swapInterval)

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

renderDrawLine :: Renderer -> Point V2 CInt -> Point V2 CInt -> IO ()
renderDrawLine (Renderer r) (P (V2 x y)) (P (V2 x' y')) =
  throwIfNeg_ "SDL.Video.renderDrawLine" "SDL_RenderDrawLine" $
  Raw.renderDrawLine r x y x' y'

renderDrawLines :: Renderer -> SV.Vector (Point V2 CInt) -> IO ()
renderDrawLines (Renderer r) points =
  throwIfNeg_ "SDL.Video.renderDrawLines" "SDL_RenderDrawLines" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawLines r
                        (castPtr cp)
                        (fromIntegral (SV.length points))

renderDrawPoint :: Renderer -> Point V2 CInt -> IO ()
renderDrawPoint (Renderer r) (P (V2 x y)) =
  throwIfNeg_ "SDL.Video.renderDrawPoint" "SDL_RenderDrawPoint" $
  Raw.renderDrawPoint r x y

renderDrawPoints :: Renderer -> SV.Vector (Point V2 CInt) -> IO ()
renderDrawPoints (Renderer r) points =
  throwIfNeg_ "SDL.Video.renderDrawPoints" "SDL_RenderDrawPoints" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawPoints r
                         (castPtr cp)
                         (fromIntegral (SV.length points))

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

pixelFormatCtT :: (Num a, Eq a) => a -> PixelFormat
pixelFormatCtT n' = case n' of
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
  _ -> error "pixelFormatCtT: unknown pixel format"

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
          displayModeFormat = pixelFormatCtT format
        , displayModeSize = V2 w' h'
        , displayModeRefreshRate = refreshRate
      }

    return $ Display {
        displayName = name'
      , displayBoundsPosition = P (V2 x y)
      , displayBoundsSize = V2 w h
      , displayModes = modes
    }

data Rectangle a = Rectangle (Point V2 a) (V2 a)

instance Storable a => Storable (Rectangle a) where
  sizeOf ~(Rectangle o s) = sizeOf o + sizeOf s
  alignment _ = 0
  peek ptr = do
    o <- peek (castPtr ptr)
    s <- peek (castPtr (ptr `plusPtr` sizeOf o))
    return (Rectangle o s)
  poke ptr (Rectangle o s) = do
    poke (castPtr ptr) o
    poke (castPtr (ptr `plusPtr` sizeOf o)) s

renderDrawRect :: Renderer -> Rectangle CInt -> IO ()
renderDrawRect (Renderer r) rect =
  throwIfNeg_ "SDL.Video.renderDrawRect" "SDL_RenderDrawRect" $
  with rect (Raw.renderDrawRect r . castPtr)

renderDrawRects :: Renderer -> SV.Vector (Rectangle CInt) -> IO ()
renderDrawRects (Renderer r) rects =
  throwIfNeg_ "SDL.Video.renderDrawRects" "SDL_RenderDrawRects" $
  SV.unsafeWith rects $ \rp ->
    Raw.renderDrawRects r
                        (castPtr rp)
                        (fromIntegral (SV.length rects))

setRenderDrawColor :: Renderer -> V4 Word8 -> IO ()
setRenderDrawColor (Renderer re) (V4 r g b a) =
  throwIfNeg_ "SDL.Video.setRenderDrawColor" "SDL_SetRenderDrawColor" $
  Raw.setRenderDrawColor re r g b a

renderFillRect :: Renderer -> Maybe (Rectangle CInt) -> IO ()
renderFillRect (Renderer r) rect = do
  throwIfNeg_ "SDL.Video.renderFillRect" "SDL_RenderFillRect" $
    maybeWith with rect $ \rPtr ->
      Raw.renderFillRect r
                         (castPtr rPtr)

renderFillRects :: Renderer -> SV.Vector (Rectangle CInt) -> IO ()
renderFillRects (Renderer r) rects = do
  throwIfNeg_ "SDL.Video.renderFillRects" "SDL_RenderFillRects" $
    SV.unsafeWith rects $ \rp ->
      Raw.renderFillRects r
                          (castPtr rp)
                          (fromIntegral (SV.length rects))

-- | Show a simple message box with the given title and a message. Consider
-- writing your messages to @stderr@ too.
--
-- Throws 'SDLException' if there are no available video targets.
showSimpleMessageBox :: Maybe Window -> MessageKind -> Text -> Text -> IO ()
showSimpleMessageBox window kind title message =
  throwIfNot0_ "SDL.Video.showSimpleMessageBox" "SDL_ShowSimpleMessageBox" $ do
    BS.useAsCString (Text.encodeUtf8 title) $ \title' ->
      BS.useAsCString (Text.encodeUtf8 message) $ \message' ->
        Raw.showSimpleMessageBox (messageKindToC kind) title' message' $
          windowId window
  where
    windowId (Just (Window w)) = w
    windowId Nothing = nullPtr

data MessageKind
  = Error
  | Warning
  | Information
  deriving (Eq, Show)

messageKindToC :: Num a => MessageKind -> a
messageKindToC kind = case kind of
  Error -> Raw.messageBoxFlagError
  Warning -> Raw.messageBoxFlagWarning
  Information -> Raw.messageBoxFlagInformation

renderClear :: Renderer -> IO ()
renderClear (Renderer r) =
  throwIfNeg_ "SDL.Video.renderClear" "SDL_RenderClear" $
  Raw.renderClear r

data BlendMode = BlendNone | BlendAlphaBlend | BlendAdditive | BlendMod
  deriving (Eq,Show)

blendModeToC :: BlendMode -> Word32
blendModeToC BlendNone = Raw.blendModeNone
blendModeToC BlendAlphaBlend = Raw.blendModeBlend
blendModeToC BlendAdditive = Raw.blendModeAdd
blendModeToC BlendMod = Raw.blendModeAdd

setRenderDrawBlendMode :: Renderer -> BlendMode -> IO ()
setRenderDrawBlendMode (Renderer r) bm =
  throwIfNeg_ "SDL.Video.setRenderDrawBlendMode" "SDL_RenderDrawBlendMode" $
  Raw.setRenderDrawBlendMode r (blendModeToC bm)

renderSetScale :: Renderer -> V2 CFloat -> IO ()
renderSetScale (Renderer r) (V2 x y) =
  throwIfNeg_ "SDL.Video.renderSetScale" "SDL_RenderSetScale" $
  Raw.renderSetScale r x y

renderSetLogicalSize :: Renderer -> V2 CInt -> IO ()
renderSetLogicalSize (Renderer r) (V2 x y) =
  throwIfNeg_ "SDL.Video.renderSetLogicalSize" "SDL_RenderSetLogicalSize" $
  Raw.renderSetLogicalSize r x y

renderSetClipRect :: Renderer -> Rectangle CInt -> IO ()
renderSetClipRect (Renderer r) rect =
  throwIfNeg_ "SDL.Video.renderSetClipRect" "SDL_RenderSetClipRect" $
  with rect $ Raw.renderSetClipRect r . castPtr

renderSetViewport :: Renderer -> Rectangle CInt -> IO ()
renderSetViewport (Renderer r) rect =
  throwIfNeg_ "SDL.Video.renderSetViewport" "SDL_RenderSetViewport" $
  with rect $ Raw.renderSetViewport r . castPtr
