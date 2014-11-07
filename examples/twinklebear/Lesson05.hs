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


loadTexture :: FilePath -> SDL.RenderM SDL.Texture
loadTexture path = do
  bmp <- SDL.liftRender $ getDataFileName path >>= SDL.loadBMP
  SDL.createTextureFromSurface bmp <* (SDL.liftRender $ SDL.freeSurface bmp)


renderTexture :: SDL.Texture -> ClipRect -> RenderPos -> SDL.RenderM ()
renderTexture tex clipRect pos = do
  ti <- SDL.liftRender $ SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pos'   = case pos of
        At p     -> p
        Centered -> let cntr a b = (a - b) `div` 2
                    in P $ V2 (cntr screenWidth w) (cntr screenHeight h)
      extent = (V2 w h)
  SDL.renderCopy tex clipRect (Just $ SDL.Rectangle pos' extent)


main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow { SDL.windowSize = V2 screenWidth screenHeight }

  window <- SDL.createWindow "Lesson 5" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  spriteSheet <- SDL.withRenderer renderer $ loadTexture "examples/twinklebear/spritesheet.bmp"
  let [spriteOne, spriteTwo, spriteThree, spriteFour] =
        [ SDL.Rectangle (P (V2 (x * spriteWidth) (y * spriteHeight))) (V2 spriteWidth spriteHeight)
          | x <- [0..1], y <- [0..1] ]

      loop spriteRect = do
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

        SDL.withRenderer renderer $ do
          SDL.renderClear
          renderTexture spriteSheet spriteRect' Centered
          SDL.renderPresent

        unless quit $ loop spriteRect'

  loop $ Just spriteOne

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
