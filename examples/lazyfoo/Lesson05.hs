{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson05 (main) where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Linear
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

loadSurface :: SDL.Surface -> FilePath -> IO SDL.Surface
loadSurface screenSurface path = do
  loadedSurface <- getDataFileName path >>= SDL.loadBMP
  desiredFormat <- SDL.surfaceFormat screenSurface
  SDL.convertSurface loadedSurface desiredFormat <* SDL.freeSurface loadedSurface

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.withWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight } $ \window -> do
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window

    stretchedSurface <- loadSurface screenSurface "examples/lazyfoo/stretch.bmp"

    let
      loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents

        events <- collectEvents
        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.blitScaled stretchedSurface Nothing screenSurface Nothing
        SDL.updateWindowSurface window

        unless quit loop

    loop

    SDL.freeSurface stretchedSurface

  SDL.quit
