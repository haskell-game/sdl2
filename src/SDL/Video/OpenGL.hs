{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Video.OpenGL
  ( -- * OpenGL
    defaultOpenGL
  , OpenGLConfig(..)
  , GLContext
  , glCreateContext
  , Profile(..)
  , Mode(..)
  , glMakeCurrent
  , glDeleteContext

  , glSwapWindow
  , SwapInterval(..)
  , glSetSwapInterval

  -- ** Function Loading
  , Raw.glGetProcAddress
  ) where

import Control.Applicative
import Data.Typeable
import Foreign.C.Types
import Linear
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types
import qualified SDL.Raw as Raw

defaultOpenGL :: OpenGLConfig
defaultOpenGL = OpenGLConfig
  { glColorPrecision = V4 8 8 8 0
  , glDepthPrecision = 24
  , glStencilPrecision = 8
  , glProfile = Compatibility Normal 2 1
  }

data OpenGLConfig = OpenGLConfig
  { glColorPrecision   :: V4 CInt -- ^ Defaults to 'V4' @8 8 8 0@.
  , glDepthPrecision   :: CInt    -- ^ Defaults to @24@.
  , glStencilPrecision :: CInt    -- ^ Defaults to @8@.
  , glProfile          :: Profile -- ^ Defaults to 'Compatibility' 'Normal' @2 1@.
  } deriving (Eq, Show, Typeable)

data Profile
  = Core Mode CInt CInt
  | Compatibility Mode CInt CInt
  | ES Mode CInt CInt
  deriving (Eq, Show, Typeable)

data Mode
  = Normal
  | Debug
  deriving (Eq, Show, Typeable)

newtype GLContext = GLContext Raw.GLContext
  deriving (Eq, Typeable)

-- | Create a new OpenGL context and makes it the current context for the
-- window.
--
-- Throws 'SDLException' if the window wasn't configured with OpenGL
-- support, or if context creation fails.
glCreateContext :: Window -> IO GLContext
glCreateContext (Window w) =
  GLContext <$> throwIfNull "SDL.Video.glCreateContext" "SDL_GL_CreateContext"
    (Raw.glCreateContext w)

-- | Throws 'SDLException' on failure.
glMakeCurrent :: Window -> GLContext -> IO ()
glMakeCurrent (Window w) (GLContext ctx) =
  throwIfNeg_ "SDL.Video.OpenGL.glMakeCurrent" "SDL_GL_MakeCurrent" $
    Raw.glMakeCurrent w ctx

-- | Delete the given OpenGL context.
--
-- You /must/ make sure that there are no pending commands in the OpenGL
-- command queue, the driver may still be processing commands even if you have
-- stopped issuing them!
--
-- The @glFinish@ command will block until the command queue has been fully
-- processed. You should call that function before deleting a context.
glDeleteContext :: GLContext -> IO ()
glDeleteContext (GLContext ctx) = Raw.glDeleteContext ctx

-- | Replace the contents of the front buffer with the back buffer's. The
-- contents of the back buffer are undefined, clear them with @glClear@ or
-- equivalent before drawing to them again.
glSwapWindow :: Window -> IO ()
glSwapWindow (Window w) = Raw.glSwapWindow w

data SwapInterval
  = ImmediateUpdates
  | SynchronizedUpdates
  | LateSwapTearing
  deriving (Eq, Show, Typeable)

instance ToNumber SwapInterval CInt where
  toNumber ImmediateUpdates = 0
  toNumber SynchronizedUpdates = 1
  toNumber LateSwapTearing = -1

glSetSwapInterval :: SwapInterval -> IO ()
glSetSwapInterval swapInterval =
  throwIfNeg_ "SDL.Video.glSetSwapInterval" "SDL_GL_SetSwapInterval" $
    Raw.glSetSwapInterval (toNumber swapInterval)
