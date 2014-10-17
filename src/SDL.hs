-- | Main module, re-exports the bulk of the API.

module SDL
  (module SDL.Audio
  ,module SDL.Events
  ,module SDL.Joystick
  ,module SDL.Timers
  ,module SDL.Video
  ,module SDL.Window
  ,module SDL.Haptic
  ,module SDL.Power)
  where

import SDL.Audio
import SDL.Events
import SDL.Haptic
import SDL.Power
import SDL.Joystick
import SDL.Timers
import SDL.Video
import SDL.Window
