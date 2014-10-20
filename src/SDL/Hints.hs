{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module SDL.Hints (
    AccelerometerJoystickOptions(..),
    FramebufferAccelerationOptions(..),
    Hint(..), 
    MacCTRLClickOptions(..),
    MouseModeWarpOptions(..),
    RenderDrivers(..), 
    RenderOpenGLShaderOptions(..),
    RenderScaleQuality(..), 
    RenderVSyncOptions(..), 
    setHint,
    VideoWinD3DCompilerOptions(..)
) where

import Foreign
import Foreign.C
import qualified SDL.Raw as Raw

data AccelerometerJoystickOptions = AccelerometerNotJoystick | AccelerometerIsJoystick

data FramebufferAccelerationOptions = 
    Disable3D         | 
    Enable3DDefault   |
    Enable3DDirect3D  |
    Enable3DOpenGL    |
    Enable3DOpenGLES  |
    Enable3DOpenGLES2 |
    Enable3DSoftware

data MacCTRLClickOptions = NoRightClick | EmulateRightClick

data MouseModeWarpOptions = MouseRawInput | MouseWarping

data RenderDrivers = Direct3D | OpenGL | OpenGLES | OpenGLES2 | Software

data RenderOpenGLShaderOptions = DisableShaders | EnableShaders

data RenderScaleQuality = ScaleNearest | ScaleLinear | ScaleBest

data RenderVSyncOptions = DisableVSync | EnableVSync

data VideoWinD3DCompilerOptions = D3DVistaOrLater | D3DXPSupport | D3DNone

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

setHint :: Hint v -> v -> IO Bool
setHint (HintAccelerometerAsJoystick) v =
  withCString "SDL_HINT_ACCELEROMETER_AS_JOYSTICK" $ \hint ->
    withCString
      (case v of
         AccelerometerNotJoystick -> "0"
         AccelerometerIsJoystick -> "1")
      (Raw.setHint hint)

setHint (HintFramebufferAcceleration) v =
  withCString "SDL_HINT_FRAMEBUFFER_ACCELERATION" $ \hint ->
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

setHint (HintMacCTRLClick) v =
  withCString "SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK" $ \hint ->
    withCString
      (case v of
         NoRightClick -> "0"
         EmulateRightClick -> "1")
      (Raw.setHint hint)

setHint (HintMouseRelativeModeWarp) v =
  withCString "SDL_HINT_MOUSE_RELATIVE_MODE_WARP" $ \hint ->
    withCString
      (case v of
         MouseRawInput -> "0"
         MouseWarping -> "1")
      (Raw.setHint hint)

setHint (HintRenderDriver) v =
  withCString "SDL_HINT_RENDER_DRIVER" $ \hint ->
    withCString
      (case v of
         Direct3D -> "direct3d"
         OpenGL -> "opengl"
         OpenGLES -> "opengles"
         OpenGLES2 -> "opengles2"
         Software -> "software")
      (Raw.setHint hint)

setHint (HintRenderOpenGLShaders) v =
  withCString "SDL_HINT_RENDER_OPENGL_SHADERS" $ \hint ->
    withCString
      (case v of
         DisableShaders -> "0"
         EnableShaders -> "1")
      (Raw.setHint hint)

setHint (HintRenderScaleQuality) v =
  withCString "SDL_HINT_RENDER_SCALE_QUALITY" $ \hint ->
    withCString
      (case v of
         ScaleNearest -> "0"
         ScaleLinear -> "1"
         ScaleBest -> "2")
      (Raw.setHint hint)

setHint (HintRenderVSync) v =
  withCString "SDL_HINT_RENDER_VSYNC" $ \hint ->
    withCString
      (case v of
         DisableVSync -> "0"
         EnableVSync -> "1")
      (Raw.setHint hint)

setHint (HintVideoWinD3DCompiler) v =
  withCString "SDL_HINT_VIDEO_WIN_D3DCOMPILER" $ \hint ->
    withCString
      (case v of
         D3DVistaOrLater -> "d3dcompiler_46.dll"
         D3DXPSupport -> "d3dcompiler_43.dll"
         D3DNone ->  "none")
      (Raw.setHint hint)
