{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson05 where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Linear
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

loadSurface :: FilePath -> SDL.Surface -> IO SDL.Surface
loadSurface path screenSurface = do
  loadedSurface <- SDL.loadBMP path
  desiredFormat <- SDL.surfaceFormat screenSurface
  SDL.convertSurface loadedSurface desiredFormat <* SDL.freeSurface loadedSurface

main :: IO ()
main = do
  SDL.init [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  stretchedSurface <- loadSurface "examples/lazyfoo/stretch.bmp" screenSurface

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

      SDL.blitScaled stretchedSurface Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit loop

  loop

  SDL.freeSurface stretchedSurface
  SDL.destroyWindow window
  SDL.quit
