{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson01 (main) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Linear
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.withWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight } $ \window -> do
    SDL.showWindow window

    screenSurface <- SDL.getWindowSurface window
    screenSurfaceFormat <- SDL.surfaceFormat screenSurface
    white <- SDL.mapRGB screenSurfaceFormat (V3 maxBound maxBound maxBound)
    SDL.fillRect screenSurface Nothing white
    SDL.updateWindowSurface window

  threadDelay 2000000
  SDL.quit
