{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson09 (main) where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Linear
import Linear.Affine
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

  SDL.withWindow "SDL Tutorial" SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight} $ \window -> do
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

    SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

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

          SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)
          SDL.renderClear renderer

          SDL.renderSetViewport renderer (Just $ SDL.Rectangle (P (V2 0 0)) (V2 (screenWidth `div` 2) (screenHeight `div` 2)))
          SDL.renderCopy renderer texture Nothing Nothing

          SDL.renderSetViewport renderer (Just $ SDL.Rectangle (P (V2 (screenWidth `div` 2) 0)) (V2 (screenWidth `div` 2) (screenHeight `div` 2)))
          SDL.renderCopy renderer texture Nothing Nothing

          SDL.renderSetViewport renderer (Just $ SDL.Rectangle (P (V2 0 (screenHeight `div` 2))) (V2 screenWidth (screenHeight `div` 2)))
          SDL.renderCopy renderer texture Nothing Nothing

          SDL.renderPresent renderer

          unless quit loop

    loop

    SDL.quit
