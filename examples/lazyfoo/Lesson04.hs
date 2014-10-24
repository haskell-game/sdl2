{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyfoo.Lesson04 (main) where

import Prelude hiding (any, mapM_)
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Linear
import qualified SDL

import Paths_sdl2

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

loadBMP :: FilePath -> IO (SDL.Surface)
loadBMP path = getDataFileName path >>= SDL.loadBMP

main :: IO ()
main = do
  SDL.init [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  surfaceDefault <- loadBMP "examples/lazyfoo/press.bmp"
  surfaceUp <- loadBMP "examples/lazyfoo/up.bmp"
  surfaceDown <- loadBMP "examples/lazyfoo/down.bmp"
  surfaceLeft <- loadBMP "examples/lazyfoo/left.bmp"
  surfaceRight <- loadBMP "examples/lazyfoo/right.bmp"

  let
    loop oldSurface = do
      let collectEvents = do
            e <- SDL.pollEvent
            case e of
              Nothing -> return []
              Just e' -> (e' :) <$> collectEvents

      events <- map SDL.eventPayload <$> collectEvents
      let quit =
            any (\case SDL.QuitEvent -> True ; _ -> False) events

          currentSurface =
            fromMaybe oldSurface $ getLast $
            foldMap (\case SDL.KeyboardEvent{..}
                             | keyboardEventKeyMotion == SDL.KeyDown ->
                                 case SDL.keysymKeycode keyboardEventKeysym of
                                   SDL.KeycodeUp    -> Last (Just surfaceUp)
                                   SDL.KeycodeDown  -> Last (Just surfaceDown)
                                   SDL.KeycodeRight -> Last (Just surfaceRight)
                                   SDL.KeycodeLeft  -> Last (Just surfaceLeft)
                                   _ -> mempty
                           _ -> mempty)
                    events

      SDL.blitSurface currentSurface Nothing screenSurface Nothing
      SDL.updateWindowSurface window

      unless quit (loop currentSurface)

  loop surfaceDefault

  mapM_ SDL.freeSurface [ surfaceDefault, surfaceUp, surfaceDown, surfaceRight, surfaceLeft ]
  SDL.destroyWindow window
  SDL.quit
