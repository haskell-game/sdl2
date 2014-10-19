{-# LANGUAGE OverloadedStrings #-}
module SDL.Video.OpenGL
  ( -- * OpenGL
    GLAttribute(..)
  , glSetAttribute
  , glSwapWindow
  , glGetCurrentContext
  , glCreateContext
  , Raw.glGetProcAddress
  , SwapInterval(..)
  , glSetSwapInterval
  , Raw.glResetAttributes
  ) where

import Data.Word
import Foreign.C.Types
import SDL.Exception
import SDL.Internal.Types
import qualified SDL.Raw as Raw

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
