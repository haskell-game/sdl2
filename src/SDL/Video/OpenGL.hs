{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Video.OpenGL
  ( -- * OpenGL
    GLContext
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
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types
import qualified SDL.Raw as Raw

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

data Profile
  = Core Mode CInt CInt
  | Compatibility Mode CInt CInt
  | ES Mode CInt CInt
  deriving (Eq, Show, Typeable)

data Mode
  = Normal
  | Debug
  deriving (Eq, Show, Typeable)

-- | Throws 'SDLException' on failure.
glMakeCurrent :: Window -> GLContext -> IO ()
glMakeCurrent (Window w) (GLContext ctx) =
  throwIfNeg_ "SDL.Video.OpenGL.glMakeCurrent" "SDL_GL_MakeCurrent" $
    Raw.glMakeCurrent w ctx

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
