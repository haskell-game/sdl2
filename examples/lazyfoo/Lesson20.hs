{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyfoo.Lesson20 (main) where

import Prelude hiding (any, mapM_)
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL
import qualified Data.Vector as V

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

getJoystick :: IO (SDL.Joystick)
getJoystick = do
  joysticks <- SDL.availableJoysticks
  joystick <- if V.length joysticks == 0
              then error "No joysticks connected!"
              else return (joysticks V.! 0)

  SDL.openJoystick joystick

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitJoystick, SDL.InitHaptic]

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
         { SDL.rendererAccelerated = True
         , SDL.rendererSoftware = False
         , SDL.rendererTargetTexture = False
         , SDL.rendererPresentVSync = True
         })

  SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  rumbleTexture <- loadTexture renderer "examples/lazyfoo/rumble.bmp"

  joystick <- getJoystick
  hapticDevice <- SDL.openHaptic (SDL.OpenHapticJoystick joystick)
  SDL.hapticRumbleInit hapticDevice

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let (Any quit, Any buttonDown) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         SDL.KeyboardEvent{..} ->
                           if | keyboardEventKeyMotion == SDL.KeyDown ->
                                  let scancode = SDL.keysymScancode keyboardEventKeysym
                                  in if | scancode == SDL.ScancodeEscape -> (Any True, mempty)
                                        | otherwise -> mempty
                              | otherwise -> mempty
                         SDL.JoyButtonEvent{..} ->
                           if | joyButtonEventState /= 0 -> (mempty, Any True)
                              | otherwise -> mempty
                         _ -> mempty) $
              map SDL.eventPayload events

        if buttonDown
          then SDL.hapticRumblePlay hapticDevice 0.75 500
          else return ()

        SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.renderClear renderer

        renderTexture renderer rumbleTexture (P $ V2 0 0) Nothing Nothing Nothing Nothing

        SDL.renderPresent renderer

        unless quit $ loop

  loop

  SDL.closeHaptic hapticDevice
  SDL.closeJoystick joystick

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
