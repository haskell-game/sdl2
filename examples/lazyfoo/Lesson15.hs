{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson15 (main) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  format <- SDL.surfaceFormat surface
  key <- SDL.mapRGB format (V3 0 maxBound maxBound)
  SDL.setColorKey surface (Just key)
  t <- SDL.createTextureFromSurface r surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> IO ()
renderTexture r (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.renderCopyEx r
                      t
                      clip
                      (Just (SDL.Rectangle xy dstSize))
                      (fromMaybe 0 theta)
                      center
                      (fromMaybe (pure False) flips)

textureSize :: Texture -> V2 CInt
textureSize (Texture _ sz) = sz

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  hintSet <- SDL.setHint SDL.HintRenderScaleQuality SDL.ScaleLinear
  unless hintSet $
    putStrLn "Warning: Linear texture filtering not enabled!"

  SDL.withWindow "SDL Tutorial" SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight} $ \window -> do
    SDL.showWindow window

    renderer <-
      SDL.createRenderer
        window
        (-1)
        (SDL.RendererConfig
           { SDL.rendererAccelerated = True
           , SDL.rendererSoftware = False
           , SDL.rendererTargetTexture = False
           , SDL.rendererPresentVSync = True
           })

    SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

    arrowTexture <- loadTexture renderer "examples/lazyfoo/arrow.bmp"

    let loop theta flips = do
          let collectEvents = do
                e <- SDL.pollEvent
                case e of
                  Nothing -> return []
                  Just e' -> (e' :) <$> collectEvents
          events <- collectEvents

          let (Any quit, Sum phi, Last newFlips) =
                foldMap (\case
                           SDL.QuitEvent -> (Any True, mempty, mempty)
                           SDL.KeyboardEvent{..} ->
                             (\(x,y) -> (mempty, x,y)) $
                             if | keyboardEventKeyMotion == SDL.KeyDown ->
                                    let scancode = SDL.keysymScancode keyboardEventKeysym
                                    in if | scancode == SDL.ScancodeQ -> (mempty, Last (Just (V2 True False)))
                                          | scancode == SDL.ScancodeW -> (mempty, Last (Just (V2 False False)))
                                          | scancode == SDL.ScancodeE -> (mempty, Last (Just (V2 False True)))
                                          | scancode == SDL.ScancodeA -> (Sum (-60), mempty)
                                          | scancode == SDL.ScancodeD -> (Sum 60, mempty)
                                          | otherwise -> mempty
                                | otherwise -> mempty
                           _ -> mempty) $
                map SDL.eventPayload events

          SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)
          SDL.renderClear renderer

          let theta' = theta + phi
              flips' = fromMaybe flips newFlips
          renderTexture renderer arrowTexture (P (fmap (`div` 2) (V2 screenWidth screenHeight) - fmap (`div` 2) (textureSize arrowTexture))) Nothing (Just theta') Nothing (Just flips')

          SDL.renderPresent renderer

          unless quit (loop theta' flips')

    loop 0 (pure False)

    SDL.quit
