{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson07 (main) where

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

  hintSet <- SDL.setHint SDL.HintRenderScaleQuality SDL.ScaleLinear
  unless hintSet $
    putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      (SDL.RendererConfig
         { SDL.rendererAccelerated = True
         , SDL.rendererSoftware = False
         , SDL.rendererTargetTexture = False
         , SDL.rendererPresentVSync = False
         })

  texture <- SDL.withRenderer renderer $ do
    SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
    xOutSurface <- SDL.liftRender $ getDataFileName "examples/lazyfoo/texture.bmp" >>= SDL.loadBMP
    tex <- SDL.createTextureFromSurface xOutSurface
    SDL.liftRender $ SDL.freeSurface xOutSurface
    return tex

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.withRenderer renderer $ do
          SDL.renderClear
          SDL.renderCopy texture Nothing Nothing
          SDL.renderPresent

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
