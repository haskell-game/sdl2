{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson03 where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Linear
import qualified SDL

(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.init [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  xOut <- SDL.loadBMP "examples/lazyfoo/x.bmp"

  white <- SDL.mapRGB screenSurface 0xff 0xff 0xff
  SDL.fillRect screenSurface Nothing white
  SDL.updateWindowSurface window

  let
    loop = do
      let collectEvents = do
            e <- SDL.pollEvent
            case e of
              Nothing -> return []
              Just e' -> (e' :) <$> collectEvents

      events <- collectEvents
      let quit = any (\case SDL.QuitEvent -> True ; _ -> False) $
                 map SDL.eventPayload events

      SDL.blitSurface xOut Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit loop

  loop

  SDL.freeSurface xOut
  SDL.destroyWindow window
  SDL.quit
