module SDL.Internal.Types (WindowID(..), Joystick(..)) where

import Foreign

import qualified SDL.Raw as Raw

newtype WindowID = WindowID Word32
  deriving (Eq,Ord,Show)

newtype Joystick = Joystick { joystickPtr :: Raw.Joystick }
