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
  , Timer
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
--
-- See @<https://wiki.libsdl.org/SDL_GetTicks SDL_GetTicks>@ for C documentation.
ticks :: MonadIO m => m Word32
ticks = Raw.getTicks

-- | The current time in seconds since some arbitrary starting point (consist over the life of the application).
--
-- This time is derived from the system's performance counter - see @<https://wiki.libsdl.org/SDL_GetPerformanceFrequency SDL_GetPerformanceFrequency>@ and @<https://wiki.libsdl.org/SDL_GetPerformanceCounter SDL_GetPerformanceCounter>@ for C documentation about the implementation.
time :: (Fractional a, MonadIO m) => m a
time = do
  freq <- Raw.getPerformanceFrequency
  cnt <- Raw.getPerformanceCounter
  return $ fromIntegral cnt / fromIntegral freq

-- | Wait a specified number of milliseconds before returning.
--
-- Users are generally recommended to use 'threadDelay' instead, to take advantage of the abilities of the Haskell runtime.
--
-- See @<https://wiki.libsdl.org/SDL_Delay SDL_Delay>@ for C documentation.
delay :: MonadIO m => Word32 -> m ()
delay = Raw.delay

-- | 'RetriggerTimer' allows a callback to inform SDL if the timer should be retriggered or cancelled
data RetriggerTimer
  = Reschedule Word32
    -- ^ Retrigger the timer again in a given number of milliseconds.
  | Cancel
    -- ^ Cancel future invocations of this timer.
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | A 'TimerCallback' is called with the interval size of the callback. It can return information as to whether or not the timer should continue to exist.
type TimerCallback = Word32 -> IO RetriggerTimer

-- | A timer created by 'addTimer'. This 'Timer' can be removed with 'removeTimer'.
newtype Timer =
  Timer {runTimerRemoval :: IO Bool}

-- | Set up a callback function to be run on a separate thread after the specified number of milliseconds has elapsed.
--
-- See @<https://wiki.libsdl.org/SDL_AddTimer SDL_AddTimer>@ for C documentation.
addTimer :: MonadIO m => Word32 -> TimerCallback -> m Timer
addTimer timeout callback = liftIO $ do
    cb <- Raw.mkTimerCallback wrappedCb
    tid <- throwIf0 "addTimer" "SDL_AddTimer" $ Raw.addTimer timeout cb nullPtr
    return (Timer $ auxRemove cb tid)
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

-- | Remove a 'Timer'.
--
-- See @<https://wiki.libsdl.org/SDL_RemoveTimer SDL_RemoveTimer>@ for C documentation.
removeTimer :: MonadIO m => Timer -> m Bool
removeTimer f = liftIO $ runTimerRemoval f
