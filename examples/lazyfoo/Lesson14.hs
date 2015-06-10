{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson14 (main) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
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
  SDL.colorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') -> size') clip
  in SDL.renderCopy r t clip (Just (SDL.Rectangle xy dstSize))

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      (SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedVSyncRenderer
         , SDL.rendererTargetTexture = False
         })

  SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  spriteSheetTexture <- loadTexture renderer "examples/lazyfoo/animation.bmp"
  let spriteSize = V2 64 205
      clip1 = SDL.Rectangle (P (V2 0 0)) spriteSize
      clip2 = SDL.Rectangle (P (V2 64 0)) spriteSize
      clip3 = SDL.Rectangle (P (V2 128 0)) spriteSize
      clip4 = SDL.Rectangle (P (V2 196 0)) spriteSize

  let loop (frame:frames) = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let (Any quit) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True)
                         _ -> mempty) $
              map SDL.eventPayload events

        SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.renderClear renderer

        renderTexture renderer spriteSheetTexture (P (fmap (`div` 2) (V2 screenWidth screenHeight) - fmap (`div` 2) spriteSize)) (Just frame)

        SDL.renderPresent renderer

        unless quit (loop frames)

  loop (cycle ([clip1, clip2, clip3, clip4] >>= replicate 4))

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
