{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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

setTextureAlpha :: Texture -> Word8 -> IO ()
setTextureAlpha (Texture t _) rgb = SDL.textureAlphaMod t $= rgb

setTextureBlendMode :: Texture -> SDL.BlendMode -> IO ()
setTextureBlendMode (Texture t _) bm = SDL.textureBlendMode t $= bm

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
        { SDL.rendererType = SDL.SoftwareRenderer
        , SDL.rendererTargetTexture = False
        })

  SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  modulatedTexture <- loadTexture renderer "examples/lazyfoo/fadeout.bmp"
  setTextureBlendMode modulatedTexture SDL.BlendAlphaBlend

  backgroundTexture <- loadTexture renderer "examples/lazyfoo/fadein.bmp"

  let loop alpha = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let (Any quit, Sum alphaAdjustment) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         SDL.KeyboardEvent e ->
                           (\x -> (mempty, x)) $
                           if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                  let scancode = SDL.keysymScancode (SDL.keyboardEventKeysym e)
                                  in if | scancode == SDL.ScancodeW -> Sum 32
                                        | scancode == SDL.ScancodeS -> Sum (-32)
                                        | otherwise -> mempty
                              | otherwise -> mempty
                         _ -> mempty) $
              map SDL.eventPayload events

        SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.renderClear renderer

        renderTexture renderer backgroundTexture 0 Nothing

        let alpha' = max 0 (min 255 (alpha + alphaAdjustment))
        setTextureAlpha modulatedTexture (fromIntegral alpha')
        renderTexture renderer modulatedTexture 0 Nothing

        SDL.renderPresent renderer

        unless quit (loop alpha')

  loop (255 :: Int) -- We use 'Int' to avoid integer overflow

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
