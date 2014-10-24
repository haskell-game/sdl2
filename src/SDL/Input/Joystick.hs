{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Input.Joystick
  ( availableJoysticks
  , JoystickDevice
  , joystickDeviceName

  , openJoystick
  , Joystick
  , buttonPressed
  ) where

import Control.Applicative
import Data.Int
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Linear
import SDL.Exception

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified SDL.Raw as Raw

data JoystickDevice = JoystickDevice
  { joystickDeviceName :: Text
  , joystickDeviceId :: CInt
  } deriving (Eq, Show, Typeable)

availableJoysticks :: IO (V.Vector JoystickDevice)
availableJoysticks = do
  n <- throwIfNeg "SDL.Input.Joystick.availableJoysticks" "SDL_NumJoysticks" Raw.numJoysticks
  fmap (V.fromList) $
    for [0 .. (n - 1)] $ \i -> do
      cstr <-
        throwIfNull "SDL.Input.Joystick.availableJoysticks" "SDL_JoystickNameForIndex" $
          Raw.joystickNameForIndex i
      name <- Text.decodeUtf8 <$> BS.packCString cstr
      return (JoystickDevice name i)

newtype Joystick = Joystick Raw.Joystick
  deriving (Eq, Typeable)

openJoystick :: JoystickDevice -> IO Joystick
openJoystick (JoystickDevice _ x) =
  fmap Joystick $
  throwIfNull "SDL.Input.Joystick.openJoystick" "SDL_OpenJoystick" $
    Raw.joystickOpen x

buttonPressed :: Joystick -> CInt -> IO Bool
buttonPressed (Joystick j) buttonIndex = (== 1) <$> Raw.joystickGetButton j buttonIndex

ballDelta :: Joystick -> CInt -> IO (V2 CInt)
ballDelta (Joystick j) ballIndex =
  alloca $ \xptr ->
  alloca $ \yptr -> do
    throwIfNeg_ "SDL.Input.Joystick.ballDelta" "SDL_JoystickGetBall" $
      Raw.joystickGetBall j ballIndex xptr yptr

    V2 <$> peek xptr <*> peek yptr

axisPosition :: Joystick -> CInt -> IO Int16
axisPosition (Joystick j) axisIndex = Raw.joystickGetAxis j axisIndex
