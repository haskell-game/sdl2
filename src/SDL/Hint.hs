{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SDL.Hint (
  -- * Getting and setting hints
  Hint(..),
  setHintWithPriority,
  HintPriority(..),
  clearHints,

  -- * Hint Information
  -- ** 'HintAccelerometerAsJoystick'
  AccelerometerJoystickOptions(..),

  -- ** 'HintFramebufferAcceleration'
  FramebufferAccelerationOptions(..),

  -- ** 'HintMacCTRLClick'
  MacCTRLClickOptions(..),

  -- ** 'HintMouseRelativeModeWarp'
  MouseModeWarpOptions(..),

  -- ** 'HintRenderDriver'
  RenderDrivers(..),

  -- ** 'HintRenderOpenGLShaders'
  RenderOpenGLShaderOptions(..),

  -- ** 'HintRenderScaleQuality'
  RenderScaleQuality(..),

  -- ** 'HintRenderVSync'
  RenderVSyncOptions(..),

  -- ** 'HintVideoWinD3DCompiler'
  VideoWinD3DCompilerOptions(..)
  ) where

import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.StateVar
import Data.Typeable
import Foreign.C
import GHC.Generics (Generic)
import SDL.Exception
import qualified SDL.Raw as Raw

-- | A hint that specifies whether the Android\/iOS built-in accelerometer should
-- be listed as a joystick device, rather than listing actual joysticks only.
-- By default SDL will list real joysticks along with the accelerometer as if it
-- were a 3 axis joystick.
data AccelerometerJoystickOptions
  = AccelerometerNotJoystick
    -- ^ List only real joysticks and accept input from them
  | AccelerometerIsJoystick
    -- ^ List real joysticks along with the accelerometer as if it were a 3 axis
    -- joystick (the default)
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies how 3D acceleration is used to accelerate the SDL
-- screen surface. By default SDL tries to make a best guess whether to use
-- acceleration or not on each platform.
data FramebufferAccelerationOptions
  = Disable3D -- ^ Disable 3D acceleration
  | Enable3DDefault -- ^ Enable 3D acceleration, using the default renderer
  | Enable3DDirect3D -- ^ Enable 3D acceleration using Direct3D
  | Enable3DOpenGL -- ^ Enable 3D acceleration using OpenGL
  | Enable3DOpenGLES -- ^ Enable 3D acceleration using OpenGLES
  | Enable3DOpenGLES2 -- ^ Enable 3D acceleration using OpenGLES2
  | Enable3DSoftware -- ^ Enable 3D acceleration using software rendering
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies whether ctrl+click should generate a right-click event
-- on Mac. By default holding ctrl while left clicking will not generate a right
-- click event when on Mac.
data MacCTRLClickOptions
  = NoRightClick -- ^ Disable emulating right click
  | EmulateRightClick -- ^ Enable emulating right click
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies whether relative mouse mode is implemented using mouse
-- warping. By default SDL will use raw input for relative mouse mode
data MouseModeWarpOptions
  = MouseRawInput -- ^ Relative mouse mode uses the raw input
  | MouseWarping -- ^ Relative mouse mode uses mouse warping
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies which render driver to use. By default the first one
-- in the list that is available on the current platform is chosen.
data RenderDrivers
  = Direct3D
  | OpenGL
  | OpenGLES
  | OpenGLES2
  | Software
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies whether the OpenGL render driver uses shaders.
-- By default shaders are used if OpenGL supports them.
data RenderOpenGLShaderOptions
  = DisableShaders -- ^ Disable shaders
  | EnableShaders -- ^ Enable shaders, if they are available
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies scaling quality. By default nearest pixel sampling is
-- used.
data RenderScaleQuality
  = ScaleNearest -- ^ Nearest pixel sampling
  | ScaleLinear -- ^ linear filtering (supported by OpenGL and Direct3D)
  | ScaleBest -- ^ Anisotropic filtering (supported by Direct3D)
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies whether sync to vertical refresh is enabled or
-- disabled to avoid tearing. By default SDL uses the flag passed into calls
-- to create renderers.
data RenderVSyncOptions
  = DisableVSync
  | EnableVSync
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A hint that specifies which shader compiler to preload when using the Chrome
-- ANGLE binaries. By default @d3dcompiler_46.dll@ will be used.
data VideoWinD3DCompilerOptions
  = D3DVistaOrLater -- ^ Use @d3dcompiler_46.dll@, best for Vista or later
  | D3DXPSupport -- ^ Use @d3dcompiler_43.dll@ for XP support
  | D3DNone -- ^ Do not load any library, useful if you compiled ANGLE from source and included the compiler in your binaries
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | The 'Hint' type exports a well-typed interface to SDL's concept of
-- <https://wiki.libsdl.org/CategoryHints hints>. This type has instances for
-- both 'HasGetter' and 'HasSetter', allowing you to get and set hints. Note that
-- the 'HasSetter' interface is fairly relaxed - if a hint cannot be set, the
-- failure will be silently discarded. For more feedback and control when setting
-- hints, see 'setHintWithPriority'.
data Hint :: * -> * where
  HintAccelerometerAsJoystick :: Hint AccelerometerJoystickOptions
  HintFramebufferAcceleration :: Hint FramebufferAccelerationOptions
  HintMacCTRLClick :: Hint MacCTRLClickOptions
  HintMouseRelativeModeWarp :: Hint MouseModeWarpOptions
  HintRenderDriver :: Hint RenderDrivers
  HintRenderOpenGLShaders :: Hint RenderOpenGLShaderOptions
  HintRenderScaleQuality :: Hint RenderScaleQuality
  HintRenderVSync :: Hint RenderVSyncOptions
  HintVideoWinD3DCompiler :: Hint VideoWinD3DCompilerOptions

instance HasSetter (Hint v) v where
  hint $= v =
    _setHint (\name value ->
                void (Raw.setHint name value))
             hint
             v

-- | How to deal with setting hints when an existing override or environment
-- variable is present.
data HintPriority
  = DefaultPriority -- ^ Low priority, used for default values
  | NormalPriority -- ^ Medium priority
  | OverridePriority -- ^ High priority
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | Set the value of a hint, applying priority rules for when there is a
-- conflict. Ordinarily, a hint will not be set if there is an existing override
-- hint or environment variable that takes precedence.
setHintWithPriority :: MonadIO m => HintPriority -> Hint v -> v -> m Bool
setHintWithPriority prio =
  _setHint (\name value ->
              Raw.setHintWithPriority
                name
                value
                (case prio of
                   DefaultPriority -> Raw.SDL_HINT_DEFAULT
                   NormalPriority -> Raw.SDL_HINT_NORMAL
                   OverridePriority -> Raw.SDL_HINT_OVERRIDE))

_setHint :: MonadIO m => (CString -> CString -> IO a) -> Hint v -> v -> m a
_setHint f h@HintAccelerometerAsJoystick v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         AccelerometerNotJoystick -> "0"
         AccelerometerIsJoystick -> "1")
      (f hint)

_setHint f h@HintFramebufferAcceleration v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         Disable3D -> "0"
         Enable3DDefault -> "1"
         Enable3DDirect3D -> "direct3d"
         Enable3DOpenGL -> "opengl"
         Enable3DOpenGLES -> "opengles"
         Enable3DOpenGLES2 -> "opengles2"
         Enable3DSoftware -> "software"
         )
      (f hint)

_setHint f h@HintMacCTRLClick v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         NoRightClick -> "0"
         EmulateRightClick -> "1")
      (f hint)

_setHint f h@HintMouseRelativeModeWarp v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         MouseRawInput -> "0"
         MouseWarping -> "1")
      (f hint)

_setHint f h@HintRenderDriver v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         Direct3D -> "direct3d"
         OpenGL -> "opengl"
         OpenGLES -> "opengles"
         OpenGLES2 -> "opengles2"
         Software -> "software")
      (f hint)

_setHint f h@HintRenderOpenGLShaders v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         DisableShaders -> "0"
         EnableShaders -> "1")
      (f hint)

_setHint f h@HintRenderScaleQuality v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         ScaleNearest -> "0"
         ScaleLinear -> "1"
         ScaleBest -> "2")
      (f hint)

_setHint f h@HintRenderVSync v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         DisableVSync -> "0"
         EnableVSync -> "1")
      (f hint)

_setHint f h@HintVideoWinD3DCompiler v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         D3DVistaOrLater -> "d3dcompiler_46.dll"
         D3DXPSupport -> "d3dcompiler_43.dll"
         D3DNone ->  "none")
      (f hint)

-- | Retrieve and map the current value associated with the given hint.
mapHint :: MonadIO m => Hint v -> (String -> Maybe v) -> m v
mapHint h f = liftIO $
  withCString (hintToString h) $ \hint -> do
    strResult <- peekCString =<< Raw.getHint hint
    return $! fromMaybe
        (throw (SDLUnknownHintValue (hintToString h) strResult))
        (f strResult)

instance HasGetter (Hint v) v where
  get h@HintAccelerometerAsJoystick =
    mapHint h (\case
        "0" -> Just AccelerometerNotJoystick
        "1" -> Just AccelerometerIsJoystick
        _ -> Nothing)

  get h@HintFramebufferAcceleration =
    mapHint h (\case
         "0" -> Just Disable3D
         "1" -> Just Enable3DDefault
         "direct3d" -> Just Enable3DDirect3D
         "opengl" -> Just Enable3DOpenGL
         "opengles" -> Just Enable3DOpenGLES
         "opengles2" -> Just Enable3DOpenGLES2
         "software" -> Just Enable3DSoftware
         _ -> Nothing)

  get h@HintMacCTRLClick =
    mapHint h (\case
         "0" -> Just NoRightClick
         "1" -> Just EmulateRightClick
         _ -> Nothing)

  get h@HintMouseRelativeModeWarp =
    mapHint h (\case
         "0" -> Just MouseRawInput
         "1" -> Just MouseWarping
         _ -> Nothing)

  get h@HintRenderDriver =
    mapHint h (\case
         "direct3d" -> Just Direct3D
         "opengl" -> Just OpenGL
         "opengles" -> Just OpenGLES
         "opengles2" -> Just OpenGLES2
         "software" -> Just Software
         _ -> Nothing)

  get h@HintRenderOpenGLShaders =
    mapHint h (\case
         "0" -> Just DisableShaders
         "1" -> Just EnableShaders
         _ -> Nothing)

  get h@HintRenderScaleQuality =
    mapHint h (\case
         "0" -> Just ScaleNearest
         "1" -> Just ScaleLinear
         "2" -> Just ScaleBest
         _ -> Nothing)

  get h@HintRenderVSync =
    mapHint h (\case
         "0" -> Just DisableVSync
         "1" -> Just EnableVSync
         _ -> Nothing)

  get h@HintVideoWinD3DCompiler =
    mapHint h (\case
         "d3dcompiler_46.dll" -> Just D3DVistaOrLater
         "d3dcompiler_43.dll" -> Just D3DXPSupport
         "none" -> Just D3DNone
         _ -> Nothing)

hintToString :: Hint v -> String
hintToString HintAccelerometerAsJoystick = "SDL_HINT_ACCELEROMETER_AS_JOYSTICK"
hintToString HintFramebufferAcceleration = "SDL_HINT_FRAMEBUFFER_ACCELERATION"
hintToString HintMacCTRLClick            = "SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK"
hintToString HintMouseRelativeModeWarp   = "SDL_HINT_MOUSE_RELATIVE_MODE_WARP"
hintToString HintRenderDriver            = "SDL_HINT_RENDER_DRIVER"
hintToString HintRenderOpenGLShaders     = "SDL_HINT_RENDER_OPENGL_SHADERS"
hintToString HintRenderScaleQuality      = "SDL_HINT_RENDER_SCALE_QUALITY"
hintToString HintRenderVSync             = "SDL_HINT_RENDER_VSYNC"
hintToString HintVideoWinD3DCompiler     = "SDL_HINT_VIDEO_WIN_D3DCOMPILER"

clearHints :: MonadIO m => m ()
clearHints = Raw.clearHints
