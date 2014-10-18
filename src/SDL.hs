-- | Main module, re-exports the bulk of the API.

module SDL
  (module SDL.Audio
  ,module SDL.Events
  ,module SDL.Init
  ,module SDL.Joystick
  ,module SDL.Timers
  ,module SDL.Video
  ,module SDL.Window
  ,module SDL.Haptic
  ,module SDL.Power
  ,module SDL.Exception)
  where

import SDL.Audio
import SDL.Events
import SDL.Exception
import SDL.Init
import SDL.Haptic
import SDL.Joystick
import SDL.Power
import SDL.Timers
import SDL.Video
import SDL.Window
