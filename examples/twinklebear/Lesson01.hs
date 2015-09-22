{-# LANGUAGE OverloadedStrings #-}
module TwinkleBear.Lesson01 (main) where


import Prelude hiding (init)
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL

import Paths_sdl2 (getDataFileName)

main :: IO ()
main = do
  SDL.initializeAll

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowInitialSize = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello World!" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  bmp <- getDataFileName "examples/twinklebear/hello.bmp" >>= SDL.loadBMP
  tex <- SDL.createTextureFromSurface renderer bmp
  SDL.freeSurface bmp

  SDL.clear renderer
  SDL.copy renderer tex Nothing Nothing
  SDL.present renderer

  SDL.delay 2000

  SDL.destroyTexture tex
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
