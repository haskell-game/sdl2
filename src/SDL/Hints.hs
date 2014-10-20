{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module SDL.Hints (
    AccelerometerJoystickOptions(..),
    Hint(..), 
    RenderScaleQuality(..), 
    setHint
) where

import Foreign
import Foreign.C
import qualified SDL.Raw as Raw

data AccelerometerJoystickOptions = AccelerometerNotJoystick | AccelerometerIsJoystick
data RenderScaleQuality = ScaleNearest | ScaleLinear | ScaleBest

data Hint :: * -> * where
  HintAccelerometerAsJoystick :: Hint AccelerometerJoystickOptions
  HintRenderScaleQuality :: Hint RenderScaleQuality

setHint :: Hint v -> v -> IO Bool
setHint (HintAccelerometerAsJoystick) v =
  withCString "SDL_ACCELEROMETER_AS_JOYSTICK" $ \hint ->
    withCString
      (case v of
         AccelerometerNotJoystick -> "0"
         AccelerometerIsJoystick -> "1")
      (Raw.setHint hint)
setHint (HintRenderScaleQuality) v =
  withCString "SDL_RENDER_SCALE_QUALITY" $ \hint ->
    withCString
      (case v of
         ScaleNearest -> "0"
         ScaleLinear -> "1"
         ScaleBest -> "2")
      (Raw.setHint hint)
