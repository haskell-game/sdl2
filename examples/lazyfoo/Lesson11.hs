{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson11 (main) where

import Control.Lens.Operators
import Control.Monad
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
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

  spriteSheetTexture <- loadTexture renderer "examples/lazyfoo/dots.bmp"
  let spriteSize = V2 100 100
      clip1 = SDL.Rectangle (P (V2 0 0)) spriteSize
      clip2 = SDL.Rectangle (P (V2 100 0)) spriteSize
      clip3 = SDL.Rectangle (P (V2 0 100)) spriteSize
      clip4 = SDL.Rectangle (P (V2 100 100)) spriteSize

  let loop = do
        events <- SDL.pollEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer

        renderTexture renderer spriteSheetTexture (P (V2 0 0)) (Just clip1)
        renderTexture renderer spriteSheetTexture (P (V2 (screenWidth - spriteSize ^. _x) 0)) (Just clip2)
        renderTexture renderer spriteSheetTexture (P (V2 0 (screenHeight - spriteSize ^. _y))) (Just clip3)
        renderTexture renderer spriteSheetTexture (P (V2 (screenWidth - spriteSize ^. _x) (screenHeight - spriteSize ^. _y))) (Just clip4)

        SDL.present renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
