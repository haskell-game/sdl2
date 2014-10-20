{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson01 where

import Control.Concurrent (threadDelay)
import Linear
import qualified SDL

(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.init [SDL.InitVideo]

  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  white <- SDL.mapRGB screenSurfaceFormat 0xff 0xff 0xff
  SDL.fillRect screenSurface Nothing white
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.quit
