{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyfoo.Lesson19 (main) where

import Prelude hiding (any, mapM_)
import Control.Monad hiding (mapM_)
import Data.Int
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL
import qualified Data.Vector as V

import Paths_sdl2 (getDataFileName)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

joystickDeadZone :: Int16
joystickDeadZone = 8000

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

  joystick <- getJoystick
  joystickID <- SDL.getJoystickID joystick

  let loop (xDir', yDir') = do
        events <- SDL.pollEvents

        let (Any quit, Last newDir) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         SDL.KeyboardEvent e ->
                           if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                  case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                                    SDL.ScancodeEscape -> (Any True, mempty)
                                    _ -> mempty
                              | otherwise -> mempty
                         SDL.JoyAxisEvent e ->
                           if | SDL.joyAxisEventWhich e == joystickID ->
                                  (\x -> (mempty, Last $ Just x)) $
                                  case SDL.joyAxisEventAxis e of
                                    0 -> if | SDL.joyAxisEventValue e < -joystickDeadZone -> (-1, yDir')
                                            | SDL.joyAxisEventValue e > joystickDeadZone -> (1, yDir')
                                            | otherwise -> (0, yDir')
                                    1 -> if | SDL.joyAxisEventValue e < -joystickDeadZone -> (xDir', -1)
                                            | SDL.joyAxisEventValue e > joystickDeadZone -> (xDir', 1)
                                            | otherwise -> (xDir', 0)
                                    _ -> (xDir', yDir')
                              | otherwise -> mempty
                         _ -> mempty) $
              map SDL.eventPayload events

        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer

        let dir@(xDir, yDir) = fromMaybe (xDir', yDir') newDir
            phi = if xDir == 0 && yDir == 0
                  then 0
                  else (atan2 yDir xDir) * (180.0 / pi)

        renderTexture renderer arrowTexture (P (fmap (`div` 2) (V2 screenWidth screenHeight) - fmap (`div` 2) (textureSize arrowTexture))) Nothing (Just phi) Nothing Nothing

        SDL.present renderer

        unless quit $ loop dir

  loop (0, 0)

  SDL.closeJoystick joystick

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
