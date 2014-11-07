{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson13 (main) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Word
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

renderTexture :: Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> SDL.RenderM ()
renderTexture (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.renderCopy t clip (Just (SDL.Rectangle xy dstSize))

setTextureAlpha :: Texture -> Word8 -> IO ()
setTextureAlpha (Texture t _) = SDL.setTextureAlphaMod t

setTextureBlendMode :: Texture -> SDL.BlendMode -> IO ()
setTextureBlendMode (Texture t _) = SDL.setTextureBlendMode t

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
         { SDL.rendererAccelerated = False
         , SDL.rendererSoftware = True
         , SDL.rendererTargetTexture = False
         , SDL.rendererPresentVSync = False
         })

  SDL.withRenderer renderer $ do
    SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)

    modulatedTexture <- loadTexture "examples/lazyfoo/fadeout.bmp"
    SDL.liftRender $ setTextureBlendMode modulatedTexture SDL.BlendAlphaBlend

    backgroundTexture <- loadTexture "examples/lazyfoo/fadein.bmp"

    let loop alpha = do
          let collectEvents = do
                e <- SDL.pollEvent
                case e of
                  Nothing -> return []
                  Just e' -> (e' :) <$> collectEvents
          events <- SDL.liftRender collectEvents

          let (Any quit, Sum alphaAdjustment) =
                foldMap (\case
                           SDL.QuitEvent -> (Any True, mempty)
                           SDL.KeyboardEvent{..} ->
                             (\x -> (mempty, x)) $
                             if | keyboardEventKeyMotion == SDL.KeyDown ->
                                    let scancode = SDL.keysymScancode keyboardEventKeysym
                                    in if | scancode == SDL.ScancodeW -> Sum 32
                                          | scancode == SDL.ScancodeS -> Sum (-32)
                                          | otherwise -> mempty
                                | otherwise -> mempty
                           _ -> mempty) $
                map SDL.eventPayload events

          SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
          SDL.renderClear

          renderTexture backgroundTexture 0 Nothing

          let alpha' = max 0 (min 255 (alpha + alphaAdjustment))
          SDL.liftRender $ setTextureAlpha modulatedTexture (fromIntegral alpha')
          renderTexture modulatedTexture 0 Nothing

          SDL.renderPresent

          unless quit (loop alpha')

    loop (255 :: Int) -- We use 'Int' to avoid integer overflow

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
