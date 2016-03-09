{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Lazyfoo.Lesson04 (main) where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Linear
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

loadBMP :: FilePath -> IO (SDL.Surface)
loadBMP path = getDataFileName path >>= SDL.loadBMP

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  surfaceDefault <- loadBMP "examples/lazyfoo/press.bmp"
  surfaceUp <- loadBMP "examples/lazyfoo/up.bmp"
  surfaceDown <- loadBMP "examples/lazyfoo/down.bmp"
  surfaceLeft <- loadBMP "examples/lazyfoo/left.bmp"
  surfaceRight <- loadBMP "examples/lazyfoo/right.bmp"

  let
    loop oldSurface = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = any (== SDL.QuitEvent) events

          currentSurface =
            fromMaybe oldSurface $ getLast $
            foldMap (\case SDL.KeyboardEvent e
                             | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                 case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                                   SDL.KeycodeUp    -> Last (Just surfaceUp)
                                   SDL.KeycodeDown  -> Last (Just surfaceDown)
                                   SDL.KeycodeRight -> Last (Just surfaceRight)
                                   SDL.KeycodeLeft  -> Last (Just surfaceLeft)
                                   _ -> mempty
                           _ -> mempty)
                    events

      SDL.surfaceBlit currentSurface Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit (loop currentSurface)

  loop surfaceDefault

  mapM_ SDL.freeSurface [ surfaceDefault, surfaceUp, surfaceDown, surfaceRight, surfaceLeft ]
  SDL.destroyWindow window
  SDL.quit
