{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module TwinkleBear.Lesson04a (main) where


import Prelude hiding (init)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
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

  window <- SDL.createWindow "Lesson 4a" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  image <- SDL.withRenderer renderer $ loadTexture "examples/twinklebear/ladybeetle.bmp"

  let loop imgPos = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let (Any quit, Sum posDelta) =
              foldMap (\case
                SDL.QuitEvent -> (Any True, mempty)
                SDL.KeyboardEvent{..} ->
                  if | keyboardEventKeyMotion == SDL.KeyDown ->
                         let scancode = SDL.keysymScancode keyboardEventKeysym
                         in if | scancode == SDL.ScancodeUp    -> (Any False, Sum (V2    0  (-10)))
                               | scancode == SDL.ScancodeDown  -> (Any False, Sum (V2    0    10 ))
                               | scancode == SDL.ScancodeLeft  -> (Any False, Sum (V2 (-10)    0 ))
                               | scancode == SDL.ScancodeRight -> (Any False, Sum (V2   10     0 ))
                               | scancode == SDL.ScancodeQ     -> (Any True,  mempty)
                               | otherwise -> mempty
                     | otherwise -> mempty
                _ -> mempty) $
              map SDL.eventPayload events

            imgPos' = imgPos + posDelta

        SDL.withRenderer renderer $ do
          SDL.renderClear
          renderTexture image $ At (P imgPos')
          SDL.renderPresent

        unless quit $ loop imgPos'

  loop $ (V2 100 100)

  SDL.destroyTexture image
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
