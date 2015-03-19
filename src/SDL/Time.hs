{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Time
  ( -- * Time Measurement
    ticks
  , time

    -- * Timer
  , delay
  , TimerCallback
  , TimerRemoval
  , RetriggerTimer(..)
  , addTimer
  , removeTimer
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Typeable
import Data.Word
import Foreign
import GHC.Generics (Generic)

import SDL.Exception

import qualified SDL.Raw.Timer as Raw
import qualified SDL.Raw.Types as Raw

-- | Number of milliseconds since library initialization.
ticks :: MonadIO m => m Word32
ticks = Raw.getTicks

-- | The current time in seconds since some arbitrary starting point.
time :: (Fractional a, MonadIO m) => m a
time = do
  freq <- Raw.getPerformanceFrequency
  cnt <- Raw.getPerformanceCounter
  return $ fromIntegral cnt / fromIntegral freq

delay :: MonadIO m => Word32 -> m ()
delay = Raw.delay

data RetriggerTimer
  = Reschedule Word32
  | Cancel
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

type TimerCallback = Word32 -> IO RetriggerTimer

newtype TimerRemoval = TimerRemoval {
    runTimerRemoval :: IO Bool
  }

addTimer :: MonadIO m => Word32 -> TimerCallback -> m TimerRemoval
addTimer timeout callback = liftIO $ do
    cb <- Raw.mkTimerCallback wrappedCb
    tid <- throwIf0 "addTimer" "SDL_AddTimer" $ Raw.addTimer timeout cb nullPtr
    return (TimerRemoval $ auxRemove cb tid)
  where
    wrappedCb :: Word32 -> Ptr () -> IO Word32
    wrappedCb w _ = do
      next <- callback w
      return $ case next of
        Cancel       -> 0
        Reschedule n -> n

    auxRemove :: Raw.TimerCallback -> Raw.TimerID -> IO Bool
    auxRemove cb tid = do
      isSuccess <- Raw.removeTimer tid
      if (isSuccess)
        then freeHaskellFunPtr cb >> return True
        else return False

removeTimer :: MonadIO m => TimerRemoval -> m Bool
removeTimer f = liftIO $ runTimerRemoval f
