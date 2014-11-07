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


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "Lesson 4" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  SDL.withRenderer renderer $ do
    image <- loadTexture "examples/twinklebear/event-driven.bmp"

    let loop = do
          renderTexture image Centered
          SDL.renderPresent

          quit <- fmap (\ev -> case SDL.eventPayload ev of
              SDL.QuitEvent -> True
              (SDL.KeyboardEvent _ SDL.KeyDown _ _ _) -> True
              (SDL.MouseButtonEvent _ SDL.MouseButtonDown _ _ _ _ _) -> True
              _ -> False) (SDL.liftRender SDL.waitEvent)

          unless quit loop

    loop

    SDL.liftRender $ SDL.destroyTexture image

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
