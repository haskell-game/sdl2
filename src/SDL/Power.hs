{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
  fromNumber <$> throwIf (== Raw.powerStateUnknown)
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
    n | n == Raw.powerStateOnBattery -> Battery Draining
    n | n == Raw.powerStateNoBattery -> Mains
    n | n == Raw.powerStateCharging -> Battery Charging
    n | n == Raw.powerStateCharged -> Battery Charged
    _ -> error "fromNumber: not numbered"
