{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lazyfoo.Lesson03 (main) where

import Control.Monad
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

  xOut <- getDataFileName "examples/lazyfoo/x.bmp" >>= SDL.loadBMP

  let
    loop = do
      events <- SDL.pollEvents
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

      SDL.surfaceBlit xOut Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit loop

  loop

  SDL.freeSurface xOut
  SDL.destroyWindow window
  SDL.quit
