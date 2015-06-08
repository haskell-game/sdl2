{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module SDL.Hint (
    AccelerometerJoystickOptions(..),
    FramebufferAccelerationOptions(..),
    Hint(..),
    MacCTRLClickOptions(..),
    MouseModeWarpOptions(..),
    RenderDrivers(..),
    RenderOpenGLShaderOptions(..),
    RenderScaleQuality(..),
    RenderVSyncOptions(..),
    clearHints,
    HintPriority(..),
    setHintWithPriority,
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

import qualified SDL.Raw as Raw
import SDL.Exception

data AccelerometerJoystickOptions
  = AccelerometerNotJoystick
  | AccelerometerIsJoystick
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data FramebufferAccelerationOptions
  = Disable3D
  | Enable3DDefault
  | Enable3DDirect3D
  | Enable3DOpenGL
  | Enable3DOpenGLES
  | Enable3DOpenGLES2
  | Enable3DSoftware
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data MacCTRLClickOptions
  = NoRightClick
  | EmulateRightClick
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data MouseModeWarpOptions
  = MouseRawInput
  | MouseWarping
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data RenderDrivers
  = Direct3D
  | OpenGL
  | OpenGLES
  | OpenGLES2
  | Software
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data RenderOpenGLShaderOptions
  = DisableShaders
  | EnableShaders
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data RenderScaleQuality
  = ScaleNearest
  | ScaleLinear
  | ScaleBest
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data RenderVSyncOptions
  = DisableVSync
  | EnableVSync
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data VideoWinD3DCompilerOptions
  = D3DVistaOrLater
  | D3DXPSupport
  | D3DNone
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

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
  hint $= v = void (_setHint Raw.setHint hint v)

data HintPriority
  = DefaultPriority
  | NormalPriority
  | OverridePriority
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

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
