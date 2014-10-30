{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyfoo.Lesson19 (main) where

import Prelude hiding (any, mapM_)
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified SDL
import qualified Data.Vector as V

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

joystickDeadZone :: Int16
joystickDeadZone = 8000

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

textureSize :: Texture -> V2 CInt
textureSize (Texture _ sz) = sz

getJoystick :: IO (SDL.Joystick)
getJoystick = do
  joysticks <- SDL.availableJoysticks
  joystick <- if V.length joysticks == 0
              then error "No joysticks connected!"
              else return (joysticks V.! 0)

  SDL.openJoystick joystick


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitJoystick]

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

  SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

  arrowTexture <- loadTexture renderer "examples/lazyfoo/arrow.bmp"

  joystick <- getJoystick
  joystickID <- SDL.getJoystickID joystick

  let loop (xDir', yDir') = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let (Any quit, Last newDir) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         SDL.KeyboardEvent{..} ->
                           if | keyboardEventKeyMotion == SDL.KeyDown ->
                                  let scancode = SDL.keysymScancode keyboardEventKeysym
                                  in if | scancode == SDL.ScancodeEscape -> (Any True, mempty)
                                        | otherwise -> mempty
                              | otherwise -> mempty
                         SDL.JoyAxisEvent{..} ->
                           if | joyAxisEventWhich == joystickID ->
                                  (\x -> (mempty, Last $ Just x)) $
                                  case joyAxisEventAxis of
                                    0 -> if | joyAxisEventValue < -joystickDeadZone -> (-1, yDir')
                                            | joyAxisEventValue > joystickDeadZone -> (1, yDir')
                                            | otherwise -> (0, yDir')
                                    1 -> if | joyAxisEventValue < -joystickDeadZone -> (xDir', -1)
                                            | joyAxisEventValue > joystickDeadZone -> (xDir', 1)
                                            | otherwise -> (xDir', 0)
                                    _ -> (xDir', yDir')
                              | otherwise -> mempty
                         _ -> mempty) $
              map SDL.eventPayload events

        SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)
        SDL.renderClear renderer

        let dir@(xDir, yDir) = fromMaybe (xDir', yDir') newDir
            phi = if xDir == 0 && yDir == 0
                  then 0
                  else (atan2 yDir xDir) * (180.0 / pi)

        renderTexture renderer arrowTexture (P (fmap (`div` 2) (V2 screenWidth screenHeight) - fmap (`div` 2) (textureSize arrowTexture))) Nothing (Just phi) Nothing Nothing

        SDL.renderPresent renderer

        unless quit $ loop dir

  loop (0, 0)

  SDL.closeJoystick joystick

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
