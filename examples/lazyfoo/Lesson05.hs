{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson05 (main) where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

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
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  stretchedSurface <- loadSurface screenSurface "examples/lazyfoo/stretch.bmp"

  let
    loop = do
      events <- SDL.pollEvents
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

      SDL.surfaceBlitScaled stretchedSurface Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit loop

  loop

  SDL.freeSurface stretchedSurface
  SDL.destroyWindow window
  SDL.quit
