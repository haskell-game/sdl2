{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Power
  ( -- * Power Status
    getPowerInfo
  , PowerState(..)
  , BatteryState(..)
  , Charge(..)
  ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Typeable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Generics (Generic)

import qualified SDL.Raw as Raw

-- | Current power supply details.
--
-- Throws 'SDLException' if the current power state can not be determined.
--
-- See @<https://wiki.libsdl.org/SDL_GetPowerInfo SDL_GetPowerInfo>@ for C documentation.
getPowerInfo :: (Functor m, MonadIO m) => m PowerState
getPowerInfo =
  liftIO $
  alloca $ \secsPtr ->
  alloca $ \pctPtr -> do
    state <- Raw.getPowerInfo secsPtr pctPtr
    let peekCharge = liftA2 Charge (maybePeek peek secsPtr) (maybePeek peek pctPtr)
    case state of
     Raw.SDL_POWERSTATE_ON_BATTERY -> fmap (Battery Draining) peekCharge
     Raw.SDL_POWERSTATE_CHARGING -> fmap (Battery Charging) peekCharge
     Raw.SDL_POWERSTATE_CHARGED -> fmap (Battery Charged) peekCharge
     Raw.SDL_POWERSTATE_NO_BATTERY -> pure Mains
     _ -> pure UnknownPowerState

-- | Information about the power supply for the user's environment
data PowerState
  = Battery BatteryState Charge
    -- ^ The user is on a battery powered device. See 'BatteryState' for charge information, and 'Charge' for charge information
  | Mains
    -- ^ The user is on a device connected to the mains.
  | UnknownPowerState
    -- ^ SDL could not determine the power for the device.
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

-- | Information on battery consumption for battery powered devices
data BatteryState
  = Draining
    -- ^ The battery is currently being drained.
  | Charged
    -- ^ The battery is fully charged.
  | Charging
    -- ^ The device is plugged in and the battery is charging.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | Information about how much charge a battery has.
data Charge =
  Charge {chargeSecondsLeft :: Maybe CInt -- ^ How many seconds of battery life is left
         ,chargePercent :: Maybe CInt -- ^ The percentage of battery charged
         }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)
