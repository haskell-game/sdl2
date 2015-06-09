{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson09 (main) where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
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

  SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  textureSurface <- getDataFileName "examples/lazyfoo/viewport.bmp" >>= SDL.loadBMP
  texture <- SDL.createTextureFromSurface renderer textureSurface
  SDL.freeSurface textureSurface

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.renderClear renderer

        SDL.renderViewport renderer $= Just (SDL.Rectangle (P (V2 0 0)) (V2 (screenWidth `div` 2) (screenHeight `div` 2)))
        SDL.renderCopy renderer texture Nothing Nothing

        SDL.renderViewport renderer $= Just (SDL.Rectangle (P (V2 (screenWidth `div` 2) 0)) (V2 (screenWidth `div` 2) (screenHeight `div` 2)))
        SDL.renderCopy renderer texture Nothing Nothing

        SDL.renderViewport renderer $= Just (SDL.Rectangle (P (V2 0 (screenHeight `div` 2))) (V2 screenWidth (screenHeight `div` 2)))
        SDL.renderCopy renderer texture Nothing Nothing

        SDL.renderPresent renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
