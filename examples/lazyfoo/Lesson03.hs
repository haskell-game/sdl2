{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson03 (main) where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Linear
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  xOut <- getDataFileName "examples/lazyfoo/x.bmp" >>= SDL.loadBMP

  let
    loop = do
      let collectEvents = do
            e <- SDL.pollEvent
            case e of
              Nothing -> return []
              Just e' -> (e' :) <$> collectEvents

      events <- collectEvents
      let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

      SDL.blitSurface xOut Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit loop

  loop

  SDL.freeSurface xOut
  SDL.destroyWindow window
  SDL.quit
