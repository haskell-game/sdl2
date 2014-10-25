{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson11 (main) where

import Control.Applicative
import Control.Lens.Operators
import Control.Monad
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
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.renderCopy r t clip (Just (SDL.Rectangle xy dstSize))

main :: IO ()
main = do
  SDL.init [SDL.InitVideo]

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
         { SDL.rendererAccelerated = False
         , SDL.rendererSoftware = True
         , SDL.rendererTargetTexture = False
         , SDL.rendererPresentVSync = False
         })

  SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

  spriteSheetTexture <- loadTexture renderer "examples/lazyfoo/dots.bmp"
  let spriteSize = V2 100 100
      clip1 = SDL.Rectangle (P (V2 0 0)) spriteSize
      clip2 = SDL.Rectangle (P (V2 100 0)) spriteSize
      clip3 = SDL.Rectangle (P (V2 0 100)) spriteSize
      clip4 = SDL.Rectangle (P (V2 100 100)) spriteSize

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit =
              any (\case SDL.QuitEvent -> True
                         _ -> False) $
              map SDL.eventPayload events

        SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)
        SDL.renderClear renderer

        renderTexture renderer spriteSheetTexture (P (V2 0 0)) (Just clip1)
        renderTexture renderer spriteSheetTexture (P (V2 (screenWidth - spriteSize ^. _x) 0)) (Just clip2)
        renderTexture renderer spriteSheetTexture (P (V2 0 (screenHeight - spriteSize ^. _y))) (Just clip3)
        renderTexture renderer spriteSheetTexture (P (V2 (screenWidth - spriteSize ^. _x) (screenHeight - spriteSize ^. _y))) (Just clip4)

        SDL.renderPresent renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
