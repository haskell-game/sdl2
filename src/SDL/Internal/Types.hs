{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module SDL.Internal.Types
  ( WindowID(..)
  , Joystick(..)
  , Window(..)
  , Renderer(..)
  ) where

import Data.Data (Data)
import Data.Typeable
import Foreign
import GHC.Generics (Generic)

import qualified SDL.Raw as Raw

newtype WindowID = WindowID Word32
  deriving (Data, Eq, Generic, Read, Ord, Show, Typeable)

newtype Joystick = Joystick { joystickPtr :: Raw.Joystick }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

newtype Window = Window (Raw.Window)
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

newtype Renderer = Renderer Raw.Renderer
  deriving (Data, Eq, Generic, Ord, Show, Typeable)
