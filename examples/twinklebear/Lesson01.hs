{-# LANGUAGE OverloadedStrings #-}
module TwinkleBear.Lesson01 (main) where


import Prelude hiding (init)
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL


main :: IO ()
main = do
  SDL.init [ SDL.InitEverything ]

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowSize     = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererSoftware      = False
                                        , SDL.rendererAccelerated   = True
                                        , SDL.rendererPresentVSync  = True
                                        , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello World!" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  bmp <- SDL.loadBMP "examples/twinklebear/hello.bmp"
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
