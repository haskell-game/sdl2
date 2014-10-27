{-# LANGUAGE OverloadedStrings #-}
module TwinkleBear.Lesson01 (main) where


import Prelude hiding (init)
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL

import Paths_sdl2 (getDataFileName)

main :: IO ()
main = do
  SDL.initialize [ SDL.InitEverything ]

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowSize     = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererSoftware      = False
                                     , SDL.rendererAccelerated   = True
                                     , SDL.rendererPresentVSync  = True
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello World!" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  tex <- SDL.withRenderer renderer $ do
    bmp <- SDL.liftRender $ getDataFileName "examples/twinklebear/hello.bmp" >>= SDL.loadBMP
    texture <- SDL.createTextureFromSurface bmp
    SDL.liftRender $ SDL.freeSurface bmp

    SDL.renderClear
    SDL.renderCopy texture Nothing Nothing
    SDL.renderPresent
    return texture

  SDL.delay 2000

  SDL.destroyTexture tex
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
