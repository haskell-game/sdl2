{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module TwinkleBear.Lesson05 (main) where


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

spriteWidth, spriteHeight :: CInt
(spriteWidth, spriteHeight) = (100, 100)


type ClipRect = Maybe (SDL.Rectangle CInt)

data RenderPos = Centered | At (Point V2 CInt)


loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp


renderTexture :: SDL.Renderer -> SDL.Texture -> ClipRect -> RenderPos -> IO ()
renderTexture renderer tex clipRect pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pos'   = case pos of
        At p     -> p
        Centered -> let cntr a b = (a - b) `div` 2
                    in P $ V2 (cntr screenWidth w) (cntr screenHeight h)
      extent = (V2 w h)
  SDL.renderCopy renderer tex clipRect (Just $ SDL.Rectangle pos' extent)


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }

  SDL.withWindow "Lesson 5" winConfig $ \window -> do
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    spriteSheet <- getDataFileName "examples/twinklebear/spritesheet.bmp" >>= loadTexture renderer
    let [spriteOne, spriteTwo, spriteThree, spriteFour] =
          [ SDL.Rectangle (P (V2 (x * spriteWidth) (y * spriteHeight))) (V2 spriteWidth spriteHeight)
            | x <- [0..1], y <- [0..1] ]

    let loop spriteRect = do
          let collectEvents = do
                e <- SDL.pollEvent
                case e of
                  Nothing -> return []
                  Just e' -> (e' :) <$> collectEvents
          events <- collectEvents

          let (Any quit, Last newSpriteRect) =
                foldMap (\case
                  SDL.QuitEvent -> (Any True, mempty)
                  SDL.KeyboardEvent{..} ->
                    if | keyboardEventKeyMotion == SDL.KeyDown ->
                           let scancode = SDL.keysymScancode keyboardEventKeysym
                           in if | scancode == SDL.Scancode1 -> (Any False, Last (Just spriteOne))
                                 | scancode == SDL.Scancode2 -> (Any False, Last (Just spriteTwo))
                                 | scancode == SDL.Scancode3 -> (Any False, Last (Just spriteThree))
                                 | scancode == SDL.Scancode4 -> (Any False, Last (Just spriteFour))
                                 | scancode == SDL.ScancodeQ -> (Any True,  mempty)
                                 | otherwise -> mempty
                       | otherwise -> mempty
                  _ -> mempty) $
                map SDL.eventPayload events

              spriteRect' = newSpriteRect <|> spriteRect

          SDL.renderClear renderer
          renderTexture renderer spriteSheet spriteRect' Centered
          SDL.renderPresent renderer

          unless quit $ loop spriteRect'

    loop $ Just spriteOne

    SDL.quit
