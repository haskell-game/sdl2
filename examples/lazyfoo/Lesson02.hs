{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson02 (main) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  helloWorld <- getDataFileName "examples/lazyfoo/hello_world.bmp" >>= SDL.loadBMP

  SDL.surfaceBlit helloWorld Nothing screenSurface Nothing
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.freeSurface helloWorld
  SDL.quit
