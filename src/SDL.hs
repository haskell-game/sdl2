{-|

SDL (Simple DirectMedia Layer) is a library for cross-platform development of
interactive applications. SDL provides routines for managing windows, rendering
graphics, processing sound, collecting input data, and much more. The Haskell
@sdl2@ library provides both a high- and low-level API to interface with
SDL. This module exports the high-level API, whereas "SDL.Raw" provides the
lower-level bindings.

-}
module SDL
  ( -- * Initialization
    module SDL.Init

    -- * Modules
  , module SDL.Audio
  , module SDL.Event
  , module SDL.Filesystem
  , module SDL.Haptic
  , module SDL.Hint
  , module SDL.Input
  , module SDL.Power
  , module SDL.Time
  , module SDL.Video

  -- * Error Handling
  , SDLException(..)

  -- * Working with State Variables
  , module Data.StateVar
  ) where

import Data.StateVar
import SDL.Audio
import SDL.Event
import SDL.Exception (SDLException(..))
import SDL.Filesystem
import SDL.Haptic
import SDL.Hint
import SDL.Init
import SDL.Input
import SDL.Power
import SDL.Time
import SDL.Video
