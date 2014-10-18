{-# LANGUAGE OverloadedStrings #-}
module SDL.Haptic
  ( AvailableHapticDevice
  , availableHapticDeviceName
  , availableHapticDeviceIds
  , OpenHapticDevice(..)
  , openHaptic
  , HapticDevice
  , hapticDeviceName
  , hapticDeviceNumAxes
  ) where

import Control.Applicative
import Data.Text (Text)
import Foreign.C
import Data.Traversable (for)
import SDL.Internal.Types (Joystick(..))

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

data AvailableHapticDevice = AvailableHapticDevice
  { availableHapticDeviceName :: Text
  , availableHapticDeviceIndex :: CInt}

availableHapticDeviceIds :: IO (V.Vector AvailableHapticDevice)
availableHapticDeviceIds = do
  n <- SDLEx.throwIfNeg "SDL.Haptic.availableHapticDevices" "SDL_NumHaptics" Raw.numHaptics
  fmap V.fromList $
    for [0 .. (n - 1)] $ \i -> do
      cstr <- SDLEx.throwIfNull "SDL.Haptic.availableHapticDevices" "SDL_HapticName" $
        Raw.hapticName i
      name <- Text.decodeUtf8 <$> BS.packCString cstr
      return (AvailableHapticDevice name i)

data OpenHapticDevice = OpenHapticMouse | OpenHapticJoystick Joystick | OpenHapticDevice AvailableHapticDevice

data HapticDevice = HapticDevice
  { hapticDevicePtr :: Raw.Haptic
  , hapticDeviceName :: Text
  , hapticDeviceNumAxes :: CInt
  }

openHaptic :: OpenHapticDevice -> IO HapticDevice
openHaptic o = do
  ptr <-
    case o of
      OpenHapticMouse ->
        SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_OpenHapticFromMouse" $
        Raw.hapticOpenFromMouse

      OpenHapticJoystick j ->
        SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_OpenHapticFromJoystick" $
        Raw.hapticOpenFromJoystick (joystickPtr j)

      OpenHapticDevice d ->
        SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_OpenHaptic" $
        Raw.hapticOpen (availableHapticDeviceIndex d)

  i <- SDLEx.throwIfNeg "SDL.Haptic.openHaptic" "SDL_HapticIndex" $
        Raw.hapticIndex ptr

  n <- do
    cstr <- SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_HapticName" $
            Raw.hapticName i
    Text.decodeUtf8 <$> BS.packCString cstr

  axes <- SDLEx.throwIfNeg "SDL.Haptic.openHaptic" "SDL_HapticNumAxes" $
          Raw.hapticNumAxes ptr

  return (HapticDevice ptr n axes)
