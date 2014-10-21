{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson10 where

import Control.Applicative
import Control.Monad
import Data.Foldable (for_)
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture
  { textureSDL :: SDL.Texture
  , textureSize :: V2 CInt
  }

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  format <- SDL.surfaceFormat surface
  key <- SDL.mapRGB format (V3 0 maxBound maxBound)
  SDL.setColorKey surface (Just key)
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> IO ()
renderTexture r (Texture t size) xy =
  SDL.renderCopy r t Nothing (Just $ SDL.Rectangle xy size)

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
         { SDL.rendererAccelerated = True
         , SDL.rendererSoftware = False
         , SDL.rendererTargetTexture = False
         , SDL.rendererPresentVSync = False
         })

  SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

  fooTexture <- loadTexture renderer "examples/lazyfoo/foo.bmp"
  backgroundTexture <- loadTexture renderer "examples/lazyfoo/background.bmp"

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

        renderTexture renderer backgroundTexture 0
        renderTexture renderer fooTexture (P (V2 240 190))

        SDL.renderPresent renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
