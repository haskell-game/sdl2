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
                                    , SDL.windowInitialSize     = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererSoftware      = False
                                     , SDL.rendererAccelerated   = True
                                     , SDL.rendererPresentVSync  = True
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello World!" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  bmp <- getDataFileName "examples/twinkleBear/hello.bmp" >>= SDL.loadBMP
  tex <- SDL.createTextureFromSurface renderer bmp
  SDL.freeSurface bmp

  SDL.renderClear renderer
  SDL.renderCopy renderer tex Nothing Nothing
  SDL.renderPresent renderer

  SDL.delay 2000

  SDL.destroyTexture tex
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
