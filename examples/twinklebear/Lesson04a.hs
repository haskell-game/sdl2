{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module TwinkleBear.Lesson04a (main) where


import Prelude hiding (init)
import Control.Monad
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine ( Point(P) )
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable
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


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "Lesson 4a" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  image <- getDataFileName "examples/twinklebear/ladybeetle.bmp" >>= loadTexture renderer

  let loop imgPos = do
        events <- SDL.pollEvents

        let (Any quit, Sum posDelta) =
              foldMap (\case
                SDL.QuitEvent -> (Any True, mempty)
                SDL.KeyboardEvent e ->
                  if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                         case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                           SDL.ScancodeUp    -> (Any False, Sum (V2    0  (-10)))
                           SDL.ScancodeDown  -> (Any False, Sum (V2    0    10 ))
                           SDL.ScancodeLeft  -> (Any False, Sum (V2 (-10)    0 ))
                           SDL.ScancodeRight -> (Any False, Sum (V2   10     0 ))
                           SDL.ScancodeQ     -> (Any True,  mempty)
                           _ -> mempty
                     | otherwise -> mempty
                _ -> mempty) $
              map SDL.eventPayload events

            imgPos' = imgPos + posDelta

        SDL.clear renderer
        renderTexture renderer image $ At (P imgPos')
        SDL.present renderer

        unless quit $ loop imgPos'

  loop $ (V2 100 100)

  SDL.destroyTexture image
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
