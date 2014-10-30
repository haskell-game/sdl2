{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Input.Joystick
  ( numJoysticks
  , availableJoysticks
  , JoystickDevice(..)

  , openJoystick
  , closeJoystick

  , getJoystickID
  , Joystick
  , buttonPressed
  , ballDelta
  , axisPosition
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
import SDL.Internal.Types

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified SDL.Raw as Raw

data JoystickDevice = JoystickDevice
  { joystickDeviceName :: Text
  , joystickDeviceId :: CInt
  } deriving (Eq, Show, Typeable)

numJoysticks :: IO (CInt)
numJoysticks = throwIfNeg "SDL.Input.Joystick.availableJoysticks" "SDL_NumJoysticks" Raw.numJoysticks

availableJoysticks :: IO (V.Vector JoystickDevice)
availableJoysticks = do
  n <- numJoysticks
  fmap (V.fromList) $
    for [0 .. (n - 1)] $ \i -> do
      cstr <-
        throwIfNull "SDL.Input.Joystick.availableJoysticks" "SDL_JoystickNameForIndex" $
          Raw.joystickNameForIndex i
      name <- Text.decodeUtf8 <$> BS.packCString cstr
      return (JoystickDevice name i)

openJoystick :: JoystickDevice -> IO Joystick
openJoystick (JoystickDevice _ x) =
  fmap Joystick $
  throwIfNull "SDL.Input.Joystick.openJoystick" "SDL_OpenJoystick" $
  Raw.joystickOpen x

closeJoystick :: Joystick -> IO ()
closeJoystick (Joystick j) = Raw.joystickClose j

getJoystickID :: Joystick -> IO (Int32)
getJoystickID (Joystick j) =
  throwIfNeg "SDL.Input.Joystick.getJoystickID" "SDL_JoystickInstanceID" $
  Raw.joystickInstanceID j

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
