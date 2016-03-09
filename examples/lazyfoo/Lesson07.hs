{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson07 (main) where

import Control.Monad
import Foreign.C.Types
import Linear
import SDL (($=))
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

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
         { SDL.rendererType = SDL.AcceleratedRenderer
         , SDL.rendererTargetTexture = False
         })

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  xOutSurface <- getDataFileName "examples/lazyfoo/texture.bmp" >>= SDL.loadBMP
  texture <- SDL.createTextureFromSurface renderer xOutSurface
  SDL.freeSurface xOutSurface

  let loop = do
        events <- SDL.pollEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.clear renderer
        SDL.copy renderer texture Nothing Nothing
        SDL.present renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
