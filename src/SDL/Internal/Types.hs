module SDL.Internal.Types
  ( WindowID(..)
  , Joystick(..)
  , Window(..)
  , Renderer(..)
  ) where

import Foreign

import qualified SDL.Raw as Raw

newtype WindowID = WindowID Word32
  deriving (Eq,Ord,Show)

newtype Joystick = Joystick { joystickPtr :: Raw.Joystick }

newtype Window = Window (Raw.Window)
  deriving (Eq)

newtype Renderer = Renderer Raw.Renderer
  deriving (Eq)
