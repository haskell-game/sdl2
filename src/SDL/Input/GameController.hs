{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Input.GameController
  ( ControllerButton(..)
  , ControllerButtonState(..)
  , ControllerDeviceConnection(..)
  ) where

import Data.Data (Data)
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import GHC.Int (Int32)
import SDL.Internal.Numbered
import qualified SDL.Raw as Raw

-- | Identifies a gamepad button.
data ControllerButton
  = ControllerButtonInvalid
  | ControllerButtonA
  | ControllerButtonB
  | ControllerButtonX
  | ControllerButtonY
  | ControllerButtonBack
  | ControllerButtonGuide
  | ControllerButtonStart
  | ControllerButtonLeftStick
  | ControllerButtonRightStick
  | ControllerButtonLeftShoulder
  | ControllerButtonRightShoulder
  | ControllerButtonDpadUp
  | ControllerButtonDpadDown
  | ControllerButtonDpadLeft
  | ControllerButtonDpadRight
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButton Int32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLER_BUTTON_A -> ControllerButtonA
    Raw.SDL_CONTROLLER_BUTTON_B -> ControllerButtonB
    Raw.SDL_CONTROLLER_BUTTON_X -> ControllerButtonX
    Raw.SDL_CONTROLLER_BUTTON_Y -> ControllerButtonY
    Raw.SDL_CONTROLLER_BUTTON_BACK -> ControllerButtonBack
    Raw.SDL_CONTROLLER_BUTTON_GUIDE -> ControllerButtonGuide
    Raw.SDL_CONTROLLER_BUTTON_START -> ControllerButtonStart
    Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK -> ControllerButtonLeftStick
    Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK -> ControllerButtonRightStick
    Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER -> ControllerButtonLeftShoulder
    Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> ControllerButtonRightShoulder
    Raw.SDL_CONTROLLER_BUTTON_DPAD_UP -> ControllerButtonDpadUp
    Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN -> ControllerButtonDpadDown
    Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT -> ControllerButtonDpadLeft
    Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT -> ControllerButtonDpadRight
    _ -> ControllerButtonInvalid

-- | Identifies the state of a controller button.
data ControllerButtonState
  = ControllerButtonPressed
  | ControllerButtonReleased
  | ControllerButtonInvalidState
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButtonState Word32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLERBUTTONDOWN -> ControllerButtonPressed
    Raw.SDL_CONTROLLERBUTTONUP -> ControllerButtonReleased
    _ -> ControllerButtonInvalidState

-- | Identified whether the game controller was added, removed, or remapped.
data ControllerDeviceConnection
  = ControllerDeviceAdded
  | ControllerDeviceRemoved
  | ControllerDeviceRemapped
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerDeviceConnection Word32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLERDEVICEADDED -> ControllerDeviceAdded
    Raw.SDL_CONTROLLERDEVICEREMOVED -> ControllerDeviceRemoved
    _ -> ControllerDeviceRemapped
