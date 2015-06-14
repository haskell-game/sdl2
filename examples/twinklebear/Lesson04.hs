{-# LANGUAGE OverloadedStrings #-}
module TwinkleBear.Lesson04 (main) where


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
  SDL.renderCopy renderer tex Nothing (Just $ SDL.Rectangle pos' extent)


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "Lesson 4" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  image <- getDataFileName "examples/twinklebear/event-driven.bmp" >>= loadTexture renderer

  let loop = do
        renderTexture renderer image Centered
        SDL.renderPresent renderer

        quit <- fmap (\ev -> case SDL.eventPayload ev of
            SDL.QuitEvent -> True
            SDL.KeyboardEvent e -> SDL.keyboardEventKeyMotion e ==  SDL.KeyDown
            SDL.MouseButtonEvent e -> SDL.mouseButtonEventMotion e == SDL.MouseButtonDown
            _ -> False) SDL.waitEvent

        unless quit loop

  loop

  SDL.destroyTexture image
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
