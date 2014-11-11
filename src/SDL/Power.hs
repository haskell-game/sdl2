{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Power
  ( -- * Power Status
    getPowerInfo
  , PowerState(..)
  , BatteryState(..)
  ) where

import Control.Applicative
import Data.Typeable
import Data.Word
import Foreign.Ptr
import SDL.Exception
import SDL.Internal.Numbered

import qualified SDL.Raw as Raw

-- | Current power supply details.
--
-- Throws 'SDLException' if the current power state can not be determined.
getPowerInfo :: IO PowerState
getPowerInfo = do
  -- TODO: SDL_GetPowerInfo does not set an SDL error
  fromNumber <$> throwIf (== Raw.SDL_POWERSTATE_UNKNOWN)
    "SDL.Power.getPowerInfo" "SDL_GetPowerInfo"
    (Raw.getPowerInfo nullPtr nullPtr)

data PowerState
  = Battery BatteryState
  | Mains
  deriving (Eq, Show, Typeable)

data BatteryState
  = Draining
  | Charged
  | Charging
  deriving (Eq, Show, Typeable)

instance FromNumber PowerState Word32 where
  fromNumber n' = case n' of
    n | n == Raw.SDL_POWERSTATE_ON_BATTERY -> Battery Draining
    n | n == Raw.SDL_POWERSTATE_NO_BATTERY -> Mains
    n | n == Raw.SDL_POWERSTATE_CHARGING -> Battery Charging
    n | n == Raw.SDL_POWERSTATE_CHARGED -> Battery Charged
    _ -> error "fromNumber: not numbered"
