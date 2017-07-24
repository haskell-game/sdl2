{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SDL.Input.GameController
  ( ControllerButtonState(..)
  ) where

import Data.Data (Data)
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import SDL.Internal.Numbered
import qualified SDL.Raw as Raw

-- | Identifies the state of a controller button.
data ControllerButtonState = ControllerButtonPressed | ControllerButtonReleased
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButtonState Word8 where
  fromNumber n = case n of
    Raw.SDL_PRESSED -> ControllerButtonPressed
    Raw.SDL_RELEASED -> ControllerButtonReleased
    _ -> ControllerButtonPressed
