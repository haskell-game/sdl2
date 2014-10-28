{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson02 (main) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Linear
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.withWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight } $ \window -> do
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window

    helloWorld <- getDataFileName "examples/lazyfoo/hello_world.bmp" >>= SDL.loadBMP

    SDL.blitSurface helloWorld Nothing screenSurface Nothing
    SDL.updateWindowSurface window
    threadDelay 2000000
    SDL.freeSurface helloWorld

  SDL.quit
