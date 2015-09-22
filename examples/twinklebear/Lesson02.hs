{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module TwinkleBear.Lesson02 (main) where

import Prelude hiding (init)
import Control.Monad
import Foreign.C.Types
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


data RenderPos = Centered | At (Point V2 CInt)


loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp


renderTexture :: SDL.Renderer -> SDL.Texture -> RenderPos -> IO ()
renderTexture renderer tex pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pos'   = case pos of
        At p     -> p
        Centered -> let cntr a b = (a - b) `div` 2
                    in P $ V2 (cntr screenWidth w) (cntr screenHeight h)
      extent = (V2 w h)
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle pos' extent)


renderTiledBackground :: SDL.Renderer -> SDL.Texture -> IO ()
renderTiledBackground renderer tex = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      grid   = [ At . P $ V2 (x*w) (y*h) | x <- [ 0..screenWidth  `div` w ],
                                           y <- [ 0..screenHeight `div` h ]]
  forM_ grid (renderTexture renderer tex)


main :: IO ()
main = do
  SDL.initializeAll

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
      rdrConfig = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }

  window <- SDL.createWindow "Lesson 2" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  background <- getDataFileName "examples/twinklebear/background.bmp" >>= loadTexture renderer
  image <- getDataFileName "examples/twinklebear/smiley.bmp" >>= loadTexture renderer

  renderTiledBackground renderer background
  renderTexture renderer image Centered
  SDL.present renderer

  SDL.delay 2000

  SDL.destroyTexture image
  SDL.destroyTexture background
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
