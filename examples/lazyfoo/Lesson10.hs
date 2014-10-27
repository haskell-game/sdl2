{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson10 (main) where

import Control.Applicative
import Control.Monad
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

renderTexture :: Texture -> Point V2 CInt -> SDL.RenderM ()
renderTexture (Texture t size) xy =
  SDL.renderCopy t Nothing (Just $ SDL.Rectangle xy size)

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
         , SDL.rendererPresentVSync = False
         })

  (fooTexture, backgroundTexture) <- SDL.withRenderer renderer $ do
    SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)

    foo <- loadTexture "examples/lazyfoo/foo.bmp"
    bg <- loadTexture "examples/lazyfoo/background.bmp"
    return (foo, bg)

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.withRenderer renderer $ do
          SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
          SDL.renderClear

          renderTexture backgroundTexture 0
          renderTexture fooTexture (P (V2 240 190))

          SDL.renderPresent

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
