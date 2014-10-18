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

  -- * Drawing Primitives
  , renderDrawLine
  , renderDrawLines
  , renderDrawPoint
  , renderDrawPoints
  , renderDrawRect
  , renderDrawRects
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
  ) where

import Prelude hiding (all, foldl, foldr)

import Control.Applicative
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
    let create' = case windowPosition config of
          Centered -> create Raw.windowPosCentered Raw.windowPosCentered
          Wherever -> create Raw.windowPosUndefined Raw.windowPosUndefined
          Absolute x y -> create x y
    uncurry create' (windowSize config) flags >>= return . Window
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
  , windowSize         = (800, 600)
  }

data WindowConfig = WindowConfig
  { windowBorder       :: Bool           -- ^ Defaults to 'True'.
  , windowHighDPI      :: Bool           -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowInputGrabbed :: Bool           -- ^ Defaults to 'False'. Whether the mouse shall be confined to the window.
  , windowMode         :: WindowMode     -- ^ Defaults to 'Windowed'.
  , windowOpenGL       :: Bool           -- ^ Defaults to 'False'. Can not be changed after window creation.
  , windowPosition     :: WindowPosition -- ^ Defaults to 'Wherever'.
  , windowResizable    :: Bool           -- ^ Defaults to 'False'. Whether the window can be resized by the user. It is still possible to programatically change the size with 'setWindowSize'.
  , windowSize         :: (CInt, CInt)   -- ^ Defaults to @(800, 600)@.
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
  | Absolute CInt CInt
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
  Absolute x y -> Raw.setWindowPosition w x y

-- | Set the size of the window. Values beyond the maximum supported size are
-- clamped.
setWindowSize :: Window -> CInt -> CInt -> IO ()
setWindowSize (Window w) = Raw.setWindowSize w

-- | Set the title of the window.
setWindowTitle :: Window -> Text -> IO ()
setWindowTitle (Window w) title =
  BS.useAsCString (Text.encodeUtf8 title) $
    Raw.setWindowTitle w

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

{-

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_CreateWindowFrom"
  sdlCreateWindowFrom :: Ptr a -> IO (Ptr WindowStruct)

createWindowFrom :: Ptr a -> IO Window
createWindowFrom = fatalSDLNull "SDL_CreateWindowFrom" . sdlCreateWindowFrom >=> mkFinalizedWindow

--------------------------------------------------------------------------------
withWindow :: String -> Position -> Size -> [WindowFlag] -> (Window -> IO r) -> IO r
withWindow title position size flags =
  bracket (createWindow title position size flags) destroyWindow

destroyWindow :: Window -> IO ()
destroyWindow = finalizeForeignPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_GetCurrentWindow"
  sdlGlGetCurrentWindow :: IO (Ptr WindowStruct)

glGetCurrentWindow :: IO Window
glGetCurrentWindow =
  fatalSDLNull "SDL_GL_GetCurrentWindow" sdlGlGetCurrentWindow >>= newForeignPtr_

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_GetDrawableSize"
  sdlGlGetDrawableSize :: Ptr WindowStruct -> Ptr #{type int} -> Ptr #{type int} -> IO ()

glGetDrawableSize :: Window -> IO Size
glGetDrawableSize window = withForeignPtr window $ \cWin ->
  alloca $ \wPtr -> alloca $ \hPtr -> do
    sdlGlGetDrawableSize cWin wPtr hPtr
    Size <$> (fromIntegral <$> peek wPtr) <*> (fromIntegral <$> peek hPtr)

--------------------------------------------------------------------------------
foreign import ccall unsafe "&SDL_GL_DeleteContext"
  sdlGlDeleteContext_finalizer :: FunPtr (Ptr GLContextStruct -> IO ())

glDeleteContext :: GLContext -> IO ()
glDeleteContext = finalizeForeignPtr

--------------------------------------------------------------------------------
withOpenGL :: Window -> IO a -> IO a
withOpenGL win = bracket (glCreateContext win) glDeleteContext . const

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_ExtensionSupported"
  sdlGlExtensionSupported :: CString -> IO #{type SDL_bool}

glExtensionSupported :: String -> IO Bool
glExtensionSupported ext = withCString ext $
  fmap sdlBoolToBool . sdlGlExtensionSupported

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_BindTexture"
  sdlGlBindTexture :: Ptr TextureStruct -> Ptr CFloat -> Ptr CFloat -> IO #{type int}

-- | Bind a texture to the active texture unit in the current OpenGL context.
glBindTexture :: Texture -> IO ()
glBindTexture tex = void $ withForeignPtr tex $ \texp ->
  fatalSDLBool "SDL_GL_BindTexture" $ sdlGlBindTexture texp nullPtr nullPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_UnbindTexture"
  sdlGlUnbindTexture :: Ptr TextureStruct -> IO CInt

-- | Unbind a texture from the current OpenGL context.
glUnbindTexture :: Texture -> IO ()
glUnbindTexture tex = Control.Monad.void $ withForeignPtr tex $ \texp ->
  sdlGlUnbindTexture texp

-- | Run an action with a texture bound to the active texture unit in the current OpenGL context, and unbind it afterwards.
withBoundTexture :: Texture -> IO a -> IO a
withBoundTexture tex = bracket_ (glBindTexture tex) (glUnbindTexture tex)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_GetAttribute"
  sdlGlGetAttribute :: #{type int} -> Ptr #{type int} -> IO #{type int}


glGetAttribute :: GLAttribute -> IO #{type int}
glGetAttribute attribute = alloca $ \payloadPtr ->  do
  fatalSDLBool "SDL_GL_GetAttribute" $
    sdlGlGetAttribute (sdlGLAttributeToC attribute) payloadPtr
  peek payloadPtr

--------------------------------------------------------------------------------
glSetAttribute :: GLAttribute -> #{type int} -> IO ()
glSetAttribute attribute value = fatalSDLBool "SDL_GL_SetAttribute" $
  sdlGlSetAttribute (sdlGLAttributeToC attribute) value

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_ResetAttributes"
  glResetAttributes :: IO ()

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_SetSwapInterval"
  sdlGlSetSwapInterval :: #{type int} -> IO #{type int}


--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_SetSwapInterval"
  sdlGlGetSwapInterval :: IO #{type int}

swapIntervalFromC :: #{type int} -> SwapInterval
swapIntervalFromC 0 = ImmediateUpdates
swapIntervalFromC 1 = SynchronizedUpdates
swapIntervalFromC (-1) = LateSwapTearing
swapIntervalFromC unknown = error $ "Graphics.UI.SDL.Video.swapIntervalFromC called with unknown argument: " ++ show unknown

glGetSwapInterval :: IO SwapInterval
glGetSwapInterval = swapIntervalFromC <$> sdlGlGetSwapInterval

--------------------------------------------------------------------------------

withoutScreenSaver :: IO a -> IO a
withoutScreenSaver = bracket_ disableScreenSaver enableScreenSaver

-- int SDL_SetWindowBrightness(SDL_Window* window, float brightness)
foreign import ccall unsafe "SDL_SetWindowBrightness"
  sdlSetWindowBrightness :: Ptr WindowStruct -> CFloat -> IO CInt

setWindowBrightness :: Window -> Double -> IO ()
setWindowBrightness win brightness =
  unwrapBool "setWindowBrightness" $
  withForeignPtr win $ \cw ->
    fmap (==0) (sdlSetWindowBrightness cw (realToFrac brightness))

-- float SDL_GetWindowBrightness(SDL_Window* window)
foreign import ccall unsafe "SDL_GetWindowBrightness"
  sdlGetWindowBrightness :: Ptr WindowStruct -> IO CFloat

-- FIXME: Error handling?
getWindowBrightness :: Window -> IO Double
getWindowBrightness win =
  withForeignPtr win $
  fmap realToFrac . sdlGetWindowBrightness

-- void* SDL_SetWindowData(SDL_Window* window, const char* name, void* userdata)
-- void* SDL_GetWindowData(SDL_Window* window, const char* name)
-- int SDL_SetWindowFullscreen(SDL_Window* window, Uint32 flags)
-- int SDL_SetWindowGammaRamp(SDL_Window*window,const Uint16* red,const Uint16* green,const Uint16* blue)
-- int SDL_GetWindowGammaRamp(SDL_Window* window,Uint16*red,Uint16*green,Uint16*blue)

-- void SDL_SetWindowGrab(SDL_Window* window, SDL_bool    grabbed)
foreign import ccall unsafe "SDL_SetWindowGrab"
  sdlSetWindowGrab :: Ptr WindowStruct -> SDL_bool -> IO ()

setWindowGrab :: Window -> Bool -> IO ()
setWindowGrab win flag =
  withForeignPtr win $ \cw ->
  sdlSetWindowGrab cw (if flag then 1 else 0)

-- SDL_bool SDL_GetWindowGrab(SDL_Window* window)
foreign import ccall unsafe "SDL_GetWindowGrab"
  sdlGetWindowGrab :: Ptr WindowStruct -> IO SDL_bool

getWindowGrab :: Window -> IO Bool
getWindowGrab win = withForeignPtr win $ fmap (/=0) . sdlGetWindowGrab

foreign import ccall unsafe "SDL_SetWindowIcon"
  sdlSetWindowIcon :: Ptr WindowStruct -> Ptr SurfaceStruct -> IO ()

setWindowIcon :: Window -> Surface -> IO ()
setWindowIcon win icon =
  withForeignPtr win $ \cw ->
    withForeignPtr icon $ \icon' -> sdlSetWindowIcon cw icon'

-- void SDL_SetWindowMaximumSize(SDL_Window* window,int max_w,int max_h)
foreign import ccall unsafe "SDL_SetWindowMaximumSize"
  sdlSetWindowMaximumSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowMaximumSize :: Window -> Size -> IO ()
setWindowMaximumSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowMaximumSize cw (fromIntegral height) (fromIntegral width)

-- void SDL_GetWindowMaximumSize(SDL_Window* window,int*w,int*h)
foreign import ccall unsafe "SDL_GetWindowMaximumSize"
  sdlGetWindowMaximumSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowMaximumSize :: Window -> IO Size
getWindowMaximumSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowMaximumSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowMinimumSize(SDL_Window* window,int min_w,int min_h)
foreign import ccall unsafe "SDL_SetWindowMinimumSize"
  sdlSetWindowMinimumSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowMinimumSize :: Window -> Size -> IO ()
setWindowMinimumSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowMinimumSize cw (fromIntegral width) (fromIntegral height)

-- void SDL_GetWindowMinimumSize(SDL_Window* window, int*w, int*h)
foreign import ccall unsafe "SDL_GetWindowMinimumSize"
  sdlGetWindowMinimumSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowMinimumSize :: Window -> IO Size
getWindowMinimumSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowMinimumSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowPosition(SDL_Window* window, int x, int y)
foreign import ccall unsafe "SDL_SetWindowPosition"
  sdlSetWindowPosition :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowPosition :: Window -> Position -> IO ()
setWindowPosition win (Position x y) =
  withForeignPtr win $ \cw ->
  sdlSetWindowPosition cw (fromIntegral x) (fromIntegral y)

-- void SDL_GetWindowPosition(SDL_Window* window, int*x, int*y)
foreign import ccall unsafe "SDL_GetWindowPosition"
  sdlGetWindowPosition :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowPosition :: Window -> IO Position
getWindowPosition win =
  withForeignPtr win $ \cw ->
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
    sdlGetWindowPosition cw xPtr yPtr
    mkPosition <$> peek xPtr <*> peek yPtr

-- void SDL_SetWindowSize(SDL_Window* window, int w, int h)
foreign import ccall unsafe "SDL_SetWindowSize"
  sdlSetWindowSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowSize :: Window -> Size -> IO ()
setWindowSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowSize cw (fromIntegral width) (fromIntegral height)

-- void SDL_GetWindowSize(SDL_Window* window, int*w, int*h)
foreign import ccall unsafe "SDL_GetWindowSize"
  sdlGetWindowSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowSize :: Window -> IO Size
getWindowSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowTitle(SDL_Window* window, const char* title)
foreign import ccall unsafe "SDL_SetWindowTitle"
  sdlSetWindowTitle :: Ptr WindowStruct -> CString -> IO ()

setWindowTitle :: Window -> String -> IO ()
setWindowTitle win title =
  withUtf8CString title $ \cstr ->
        withForeignPtr win $ \winptr -> sdlSetWindowTitle winptr cstr

-- const char* SDL_GetWindowTitle(SDL_Window* window)

foreign import ccall unsafe "SDL_GetWindowTitle"
  sdlGetWindowTitle :: Ptr WindowStruct -> IO CString

getWindowTitle :: Window -> IO String
getWindowTitle w = withForeignPtr w $ sdlGetWindowTitle >=> peekCString

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetWindowPixelFormat"
  sdlGetWindowPixelFormat :: Ptr WindowStruct -> IO Word32

foreign import ccall unsafe "SDL_AllocFormat"
  sdlAllocFormat :: Word32 -> IO (Ptr PixelFormatStruct)

getWindowPixelFormat :: Window -> IO Word32
getWindowPixelFormat w = withForeignPtr w sdlGetWindowPixelFormat

allocFormat :: Word32 -> IO PixelFormat
allocFormat pf = sdlAllocFormat pf >>= newForeignPtr sdlFreeFormat_finalizer

foreign import ccall unsafe "&SDL_FreeFormat"
  sdlFreeFormat_finalizer :: FunPtr (Ptr PixelFormatStruct -> IO ())

foreign import ccall unsafe "SDL_MapRGB"
  sdlMapRGB :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> IO Word32

mapRGB :: PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
mapRGB p r g b = withForeignPtr p $ \cp -> sdlMapRGB cp r g b

foreign import ccall unsafe "SDL_MapRGBA"
  sdlMapRGBA :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

mapRGBA :: PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
mapRGBA p r g b a = withForeignPtr p $ \cp -> sdlMapRGBA cp r g b a

surfaceFormat :: Surface -> IO PixelFormat
surfaceFormat s =
  withForeignPtr s $ \cs ->
  #{peek SDL_Surface, format} cs >>= newForeignPtr_

--------------------------------------------------------------------------------
data DisplayMode = DisplayMode { displayModeFormat :: PixelFormatEnum
                               , displayModeWidth  :: #{type int}
                               , displayModeHeight :: #{type int}
                               , displayModeRefreshRate :: #{type int}
                               } deriving (Eq, Show)

instance Storable DisplayMode where
  sizeOf = const #{size SDL_DisplayMode}

  alignment = const 4

  poke ptr DisplayMode{..} = do
    #{poke SDL_DisplayMode, format} ptr (pixelFormatEnumToC displayModeFormat)
    #{poke SDL_DisplayMode, w} ptr displayModeWidth
    #{poke SDL_DisplayMode, h} ptr displayModeHeight
    #{poke SDL_DisplayMode, refresh_rate} ptr displayModeRefreshRate
    #{poke SDL_DisplayMode, driverdata} ptr nullPtr

  peek ptr = DisplayMode
    <$> (pixelFormatEnumFromC <$> #{peek SDL_DisplayMode, format} ptr)
    <*> #{peek SDL_DisplayMode, w} ptr
    <*> #{peek SDL_DisplayMode, h} ptr
    <*> #{peek SDL_DisplayMode, refresh_rate} ptr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetDisplayMode"
  sdlGetDisplayMode :: #{type int} -> #{type int} -> Ptr DisplayMode -> IO #{type int}

getDisplayMode :: #{type int} -> #{type int} -> IO DisplayMode
getDisplayMode d m = alloca $ \displayModePtr -> do
  fatalSDLBool "SDL_GetDisplayMode" (sdlGetDisplayMode d m displayModePtr)
  peek displayModePtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetCurrentDisplayMode"
  sdlGetCurrentDisplayMode :: #{type int} -> Ptr DisplayMode -> IO #{type int}

getCurrentDisplayMode :: #{type int} -> IO DisplayMode
getCurrentDisplayMode d = alloca $ \displayModePtr -> do
  fatalSDLBool "SDL_GetCurrentDisplayMode" (sdlGetCurrentDisplayMode d displayModePtr)
  peek displayModePtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetDesktopDisplayMode"
  sdlGetDesktopDisplayMode :: #{type int} -> Ptr DisplayMode -> IO #{type int}

getDesktopDisplayMode :: #{type int} -> IO DisplayMode
getDesktopDisplayMode d = alloca $ \displayModePtr -> do
  fatalSDLBool "SDL_GetDesktopDisplayMode" (sdlGetDesktopDisplayMode d displayModePtr)
  peek displayModePtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetClosestDisplayMode"
  sdlGetClosestDisplayMode :: #{type int} -> Ptr DisplayMode -> Ptr DisplayMode -> IO (Ptr DisplayMode)

getClosestDisplayMode :: #{type int} -> DisplayMode -> IO (Maybe DisplayMode)
getClosestDisplayMode d mode =
  with mode $ \modePtr ->
  alloca $ \closestPtr -> do
    _ <- sdlGetClosestDisplayMode d modePtr closestPtr
    maybePeek peek closestPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetWindowDisplayMode"
  sdlGetWindowDisplayMode :: Ptr WindowStruct -> Ptr DisplayMode -> IO #{type int}

getWindowDisplayMode :: Window -> IO DisplayMode
getWindowDisplayMode win =
  alloca $ \modePtr ->
  withForeignPtr win $ \cw -> do
    fatalSDLBool "SDL_GetWindowDisplayMode" (sdlGetWindowDisplayMode cw modePtr)
    peek modePtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_SetWindowDisplayMode"
  sdlSetWindowDisplayMode :: Ptr WindowStruct -> Ptr DisplayMode -> IO #{type int}

setWindowDisplayMode :: Window -> Maybe DisplayMode -> IO ()
setWindowDisplayMode win mode =
  withForeignPtr win $ \cw ->
  maybeWith with mode $ \modePtr ->
  fatalSDLBool "SDL_SetWindowDisplayMode" (sdlSetWindowDisplayMode cw modePtr)

getWindowDisplayIndex :: Window -> IO Int
getWindowDisplayIndex win =
  withForeignPtr win $ \cw -> do
    ret <- sdlGetWindowDisplayIndex cw
    handleErrorI "getWindowDisplayIndex" ret (return . fromIntegral)

getWindowID :: Window -> IO WindowID
getWindowID win =
  withForeignPtr win $ \cw -> fromIntegral <$> sdlGetWindowID cw

getWindowFromID :: WindowID -> IO Window
getWindowFromID wid = do
  cw <- sdlGetWindowFromID (fromIntegral wid)
  handleError "getWindowFromID" cw mkFinalizedWindow


getDisplayName :: #{type int} -> IO String
getDisplayName i =
  fatalSDLNull "SDL_GetDisplayName" (sdlGetDisplayName i) >>= peekCString

getCurrentVideoDriver :: IO String
getCurrentVideoDriver = sdlGetCurrentVideoDriver >>= peekCString

getDisplayBounds :: Int -> IO Rect
getDisplayBounds index =
  alloca $ \rect -> do
    ret <- sdlGetDisplayBounds (fromIntegral index) rect
    handleErrorI "getDisplayBounds" ret $ return $ peek rect

getVideoDriver :: #{type int} -> IO String
getVideoDriver =
  fatalSDLNull "SDL_GetVideoDriver" . sdlGetVideoDriver >=> peekCString

getWindowFlags :: Window -> IO [WindowFlag]
getWindowFlags w = withForeignPtr w $
  fmap (fromBitmask windowFlagToC) . sdlGetWindowFlags

-}
