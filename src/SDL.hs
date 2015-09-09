{-|

SDL (Simple DirectMedia Layer) is a library for cross-platform development of
interactive applications. SDL provides routines for managing windows, rendering
graphics, processing sound, collecting input data, and much more. The Haskell
@sdl2@ library provides both a high- and low-level API to interface with
SDL. This module exports the high-level API, whereas "SDL.Raw" provides the
lower-level bindings.

-}
module SDL
  ( -- * Getting Started
    -- $gettingStarted

    -- * Initialization
    module SDL.Init

    -- * Modules
  , module SDL.Audio
  , module SDL.Event
  , module SDL.Filesystem
  , module SDL.Hint
  , module SDL.Input
  , module SDL.Power
  , module SDL.Time
  , module SDL.Video

  -- * Working with State Variables
  -- $stateVars
  , get, ($=), ($~)
  -- ** Strict modification
  , ($=!), ($~!)

  -- * Error Handling
  , SDLException(..)
  ) where

import Data.StateVar
import SDL.Audio
import SDL.Event
import SDL.Exception (SDLException(..))
import SDL.Filesystem
import SDL.Hint
import SDL.Init
import SDL.Input
import SDL.Power
import SDL.Time
import SDL.Video

{- $gettingStarted

The "SDL" module exports a high-level Haskell-like abstraction to use the <http://libsdl.org/ SDL> library. SDL is a cross-platform development library designed to provide low level access to audio, keyboard, mouse, joystick, and graphics hardware via OpenGL and Direct3D.

To get started, import "SDL" and begin by initializing the subsystems you need:

@
import "SDL"

main :: IO ()
main = do
  'initialize' ['InitEverything']
@

Next, you can create a 'Window' by using 'createWindow'

@
  window <- 'createWindow' "My SDL Application" 'defaultWindow'
@

If you wish to use SDL's 2D graphics API, you can also create a 'Renderer':

@
  renderer <- 'createRenderer' window (-1) 'defaultRenderer'
@

Finally, we enter our main application loop:

@
  appLoop renderer
@

For the body of your application, we enter a loop. Inside this loop you should begin by collecting all events that
have happened - these events will inform you about information such as key presses and mouse movement:

@
appLoop :: 'Renderer' -> IO ()
appLoop renderer = do
  events <- 'pollEvents'
@

Here @events@ is a list of 'Event' values. For our application we will check if the user pressed the q key, indicating they wish to quit the application

@
  let eventIsQPress event =
        case 'eventPayload' event of
          'KeyboardEvent' keyboardEvent ->
            'keyboardEventKeyMotion' keyboardEvent == 'Pressed' &&
            'keysymKeycode' ('keyboardEventKeysym' keyboardEvent) == 'KeycodeQ'
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
@

In our @appLoop@ we process events and then update the screen accordingly. Here we simply use the 'Renderer'
to clear the screen to blue:

@
  'rendererDrawColor' renderer '$=' V4 0 0 255 255
  'clear' renderer
  'present' renderer
@

If q was not pressed, we loop again. Otherwise, we exit the loop:

@
  unless qPressed (appLoop renderer)
@

To recap, here is our full application

@

\{\-\# LANGUAGE OverloadedStrings \#\-\}
module "Main" where

import "SDL"
import "Linear" (V4(..))
import "Control.Monad" (unless)

main :: IO ()
main = do
  'initialize' ['InitEverything']
  window <- 'createWindow' "My SDL Application" 'defaultWindow'
  renderer <- 'createRenderer' window (-1) 'defaultRenderer'
  appLoop renderer

appLoop :: 'Renderer' -> IO ()
appLoop renderer = do
  events <- 'pollEvents'
  let eventIsQPress event =
        case 'eventPayload' event of
          'KeyboardEvent' keyboardEvent ->
            'keyboardEventKeyMotion' keyboardEvent == 'Pressed' &&
            'keysymKeycode' ('keyboardEventKeysym' keyboardEvent) == 'KeycodeQ'
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
  'rendererDrawColor' renderer '$=' V4 0 0 255 255
  'clear' renderer
  'present' renderer
  unless qPressed (appLoop renderer)
@

-}

{- $stateVars

The SDL API is moderately stateful. For the places where there is state that can be both read and changed, we use an abstraction provided by "Data.StateVar". This module exposes the 'StateVar' type, which models a mutable variable. You can query the contents of a 'StateVar' with 'get', and you can replace the contents of 'StateVar' with the infix assignment operator '$='.

-}
