{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson12 (main) where

import Control.Monad
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))

setTextureColor :: Texture -> V3 Word8 -> IO ()
setTextureColor (Texture t _) rgb = SDL.textureColorMod t $= rgb

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

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  modulatedTexture <- loadTexture renderer "examples/lazyfoo/colors.bmp"

  let loop color = do
        events <- SDL.pollEvents

        let (Any quit, Sum colorAdjustment) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         SDL.KeyboardEvent e ->
                           (\x -> (mempty, x)) $
                           if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                  case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                                    SDL.ScancodeQ -> Sum (V3 32 0 0)
                                    SDL.ScancodeW -> Sum (V3 0 32 0)
                                    SDL.ScancodeE -> Sum (V3 0 0 32)
                                    SDL.ScancodeA -> Sum (V3 (-32) 0 0)
                                    SDL.ScancodeS -> Sum (V3 0 (-32) 0)
                                    SDL.ScancodeD -> Sum (V3 0 0 (-32))
                                    _ -> mempty
                              | otherwise -> mempty
                         _ -> mempty) $
              map SDL.eventPayload events

        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer

        let color' = color + colorAdjustment
        setTextureColor modulatedTexture color'
        renderTexture renderer modulatedTexture 0 Nothing

        SDL.present renderer

        unless quit (loop color')

  loop (V3 maxBound maxBound maxBound)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
