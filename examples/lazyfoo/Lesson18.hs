{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyfoo.Lesson18 (main) where

import Prelude hiding (any, mapM_)
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
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

  pressTexture <- loadTexture renderer "examples/lazyfoo/press.bmp"
  upTexture <- loadTexture renderer "examples/lazyfoo/up.bmp"
  downTexture <- loadTexture renderer "examples/lazyfoo/down.bmp"
  leftTexture <- loadTexture renderer "examples/lazyfoo/left.bmp"
  rightTexture <- loadTexture renderer "examples/lazyfoo/right.bmp"

  let
    loop = do
      let collectEvents = do
            e <- SDL.pollEvent
            case e of
              Nothing -> return []
              Just e' -> (e' :) <$> collectEvents

      events <- map SDL.eventPayload <$> collectEvents
      let quit = any (== SDL.QuitEvent) events

      keyMap <- SDL.getKeyboardState
      let texture =
            if | keyMap SDL.ScancodeUp -> upTexture
               | keyMap SDL.ScancodeDown -> downTexture
               | keyMap SDL.ScancodeLeft -> leftTexture
               | keyMap SDL.ScancodeRight -> rightTexture
               | otherwise -> pressTexture

      SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
      SDL.renderClear renderer

      renderTexture renderer texture 0 Nothing Nothing Nothing Nothing

      SDL.renderPresent renderer

      unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit
