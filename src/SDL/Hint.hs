{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
    setHint,
    VideoWinD3DCompilerOptions(..)
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Typeable
import Foreign.C
import GHC.Generics (Generic)

import qualified SDL.Raw as Raw

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

setHint :: MonadIO m => Hint v -> v -> m Bool
setHint h@HintAccelerometerAsJoystick v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         AccelerometerNotJoystick -> "0"
         AccelerometerIsJoystick -> "1")
      (Raw.setHint hint)

setHint h@HintFramebufferAcceleration v = liftIO $
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
      (Raw.setHint hint)

setHint h@HintMacCTRLClick v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         NoRightClick -> "0"
         EmulateRightClick -> "1")
      (Raw.setHint hint)

setHint h@HintMouseRelativeModeWarp v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         MouseRawInput -> "0"
         MouseWarping -> "1")
      (Raw.setHint hint)

setHint h@HintRenderDriver v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         Direct3D -> "direct3d"
         OpenGL -> "opengl"
         OpenGLES -> "opengles"
         OpenGLES2 -> "opengles2"
         Software -> "software")
      (Raw.setHint hint)

setHint h@HintRenderOpenGLShaders v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         DisableShaders -> "0"
         EnableShaders -> "1")
      (Raw.setHint hint)

setHint h@HintRenderScaleQuality v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         ScaleNearest -> "0"
         ScaleLinear -> "1"
         ScaleBest -> "2")
      (Raw.setHint hint)

setHint h@HintRenderVSync v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         DisableVSync -> "0"
         EnableVSync -> "1")
      (Raw.setHint hint)

setHint h@HintVideoWinD3DCompiler v = liftIO $
  withCString (hintToString h) $ \hint ->
    withCString
      (case v of
         D3DVistaOrLater -> "d3dcompiler_46.dll"
         D3DXPSupport -> "d3dcompiler_43.dll"
         D3DNone ->  "none")
      (Raw.setHint hint)

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
