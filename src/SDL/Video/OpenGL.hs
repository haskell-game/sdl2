{-# LANGUAGE MultiParamTypeClasses #-}
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
import SDL.Internal.Numbered
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

instance ToNumber GLAttribute Word32 where
  toNumber GLRedSize = Raw.glAttrRedSize
  toNumber GLGreenSize = Raw.glAttrGreenSize
  toNumber GLBlueSize = Raw.glAttrBlueSize
  toNumber GLAlphaSize = Raw.glAttrAlphaSize
  toNumber GLBufferSize = Raw.glAttrBufferSize
  toNumber GLDoubleBuffer = Raw.glAttrDoubleBuffer
  toNumber GLDepthSize = Raw.glAttrDepthSize
  toNumber GLStencilSize = Raw.glAttrStencilSize
  toNumber GLAccumRedSize = Raw.glAttrAccumRedSize
  toNumber GLAccumGreenSize = Raw.glAttrAccumGreenSize
  toNumber GLAccumBlueSize = Raw.glAttrAccumBlueSize
  toNumber GLAccumAlphaSize = Raw.glAttrAccumAlphaSize
  toNumber GLStereo = Raw.glAttrStereo
  toNumber GLMultiSampleBuffers = Raw.glAttrMultiSampleBuffers
  toNumber GLMultiSampleSamples = Raw.glAttrMultiSampleSamples
  toNumber GLAcceleratedVisual = Raw.glAttrAcceleratedVisual
  toNumber GLRetainedBacking = Raw.glAttrRetainedBacking
  toNumber GLContextMajorVersion = Raw.glAttrContextMajorVersion
  toNumber GLContextMinorVersion = Raw.glAttrContextMinorVersion
  toNumber GLContextFlags = Raw.glAttrContextFlags
  toNumber GLContextProfileMask = Raw.glAttrContextProfileMask
  toNumber GLShareWithCurrentContext = Raw.glAttrShareWithCurrentContext
  toNumber GLFramebufferSRGBCapable = Raw.glAttrFramebufferSRGBCapable
  toNumber GLContextEGL = Raw.glAttrContextEGL

glSetAttribute :: GLAttribute -> CInt -> IO ()
glSetAttribute attribute value =
  throwIfNeg_ "SDL.Video.glSetAttribute" "SDL_GL_SetAttribute" $
    Raw.glSetAttribute (toNumber attribute) value

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
