{-# LANGUAGE OverloadedStrings #-}
module TwinkleBear.Lesson02 (main) where


import Prelude hiding (init)
import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


data RenderPos = Centered | At (Point V2 CInt)


loadTexture :: FilePath -> SDL.RenderM SDL.Texture
loadTexture path = do
  bmp <- SDL.liftRender $ getDataFileName path >>= SDL.loadBMP
  SDL.createTextureFromSurface bmp <* (SDL.liftRender $ SDL.freeSurface bmp)

renderTexture :: SDL.Texture -> RenderPos -> SDL.RenderM ()
renderTexture tex pos = do
  ti <- SDL.liftRender $ SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pos'   = case pos of
        At p     -> p
        Centered -> let cntr a b = (a - b) `div` 2
                    in P $ V2 (cntr screenWidth w) (cntr screenHeight h)
      extent = (V2 w h)
  SDL.renderCopy tex Nothing (Just $ SDL.Rectangle pos' extent)


renderTiledBackground :: SDL.Texture -> SDL.RenderM ()
renderTiledBackground tex = do
  ti <- SDL.liftRender $ SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      grid   = [ At . P $ V2 (x*w) (y*h) | x <- [ 0..screenWidth  `div` w ],
                                           y <- [ 0..screenHeight `div` h ]]
  forM_ grid (renderTexture tex)


main :: IO ()
main = do
  SDL.initialize [ SDL.InitEverything ]

  let winConfig = SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }
      rdrConfig = SDL.defaultRenderer { SDL.rendererAccelerated = True }

  window <- SDL.createWindow "Lesson 2" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  SDL.withRenderer renderer $ do
    background <- loadTexture "examples/twinklebear/background.bmp"
    image <- loadTexture "examples/twinklebear/smiley.bmp"

    renderTiledBackground background
    renderTexture image Centered
    SDL.renderPresent

    SDL.liftRender $ do
      SDL.delay 2000

      SDL.destroyTexture image
      SDL.destroyTexture background

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
