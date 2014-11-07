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
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: FilePath -> SDL.RenderM Texture
loadTexture filePath = do
  (surface, size) <- SDL.liftRender loadSurface
  t <- SDL.createTextureFromSurface surface
  SDL.liftRender $ SDL.freeSurface surface
  return (Texture t size)
    where
      loadSurface = do
        surface <- getDataFileName filePath >>= SDL.loadBMP
        size <- SDL.surfaceDimensions surface
        format <- SDL.surfaceFormat surface
        key <- SDL.mapRGB format (V3 0 maxBound maxBound)
        SDL.setColorKey surface (Just key)
        return (surface, size)

renderTexture :: Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> SDL.RenderM ()
renderTexture (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.renderCopyEx t
                      clip
                      (Just (SDL.Rectangle xy dstSize))
                      (fromMaybe 0 theta)
                      center
                      (fromMaybe (pure False) flips)
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  hintSet <- SDL.setHint SDL.HintRenderScaleQuality SDL.ScaleLinear
  unless hintSet $
    putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight}
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

  SDL.withRenderer renderer $ do
    SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)

    pressTexture <- loadTexture "examples/lazyfoo/press.bmp"
    upTexture <- loadTexture "examples/lazyfoo/up.bmp"
    downTexture <- loadTexture "examples/lazyfoo/down.bmp"
    leftTexture <- loadTexture "examples/lazyfoo/left.bmp"
    rightTexture <- loadTexture "examples/lazyfoo/right.bmp"

    let
      loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents

        (keyMap, quit) <- SDL.liftRender $ do
          events <- map SDL.eventPayload <$> collectEvents
          let quit = any (== SDL.QuitEvent) events

          keyMap <- SDL.getKeyboardState
          return (keyMap, quit)

        let texture =
              if | keyMap SDL.ScancodeUp -> upTexture
                 | keyMap SDL.ScancodeDown -> downTexture
                 | keyMap SDL.ScancodeLeft -> leftTexture
                 | keyMap SDL.ScancodeRight -> rightTexture
                 | otherwise -> pressTexture

        SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
        SDL.renderClear

        renderTexture texture 0 Nothing Nothing Nothing Nothing

        SDL.renderPresent

        unless quit loop

    loop

  SDL.destroyWindow window
  SDL.quit
