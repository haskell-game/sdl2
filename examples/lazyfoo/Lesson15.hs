{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson15 (main) where

import Control.Monad
import Data.Monoid
import Data.Maybe
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

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> IO ()
renderTexture r (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') -> size') clip
  in SDL.copyEx r
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

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  arrowTexture <- loadTexture renderer "examples/lazyfoo/arrow.bmp"

  let loop theta flips = do
        events <- SDL.pollEvents

        let (Any quit, Sum phi, Last newFlips) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty, mempty)
                         SDL.KeyboardEvent e ->
                           (\(x,y) -> (mempty, x,y)) $
                           if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                  case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                                    SDL.ScancodeQ -> (mempty, Last (Just (V2 True False)))
                                    SDL.ScancodeW -> (mempty, Last (Just (V2 False False)))
                                    SDL.ScancodeE -> (mempty, Last (Just (V2 False True)))
                                    SDL.ScancodeA -> (Sum (-60), mempty)
                                    SDL.ScancodeD -> (Sum 60, mempty)
                                    _ -> mempty
                              | otherwise -> mempty
                         _ -> mempty) $
              map SDL.eventPayload events

        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer

        let theta' = theta + phi
            flips' = fromMaybe flips newFlips
        renderTexture renderer arrowTexture (P (fmap (`div` 2) (V2 screenWidth screenHeight) - fmap (`div` 2) (textureSize arrowTexture))) Nothing (Just theta') Nothing (Just flips')

        SDL.present renderer

        unless quit (loop theta' flips')

  loop 0 (pure False)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
