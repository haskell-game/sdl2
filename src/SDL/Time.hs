{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Time
  ( -- * Time Measurement
    ticks
  , time

    -- * Timer
  , delay
  , TimerCallback
  , TimerID(..)
  , RetriggerTimer(..)
  , addTimer
  , removeTimer
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C

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
  deriving (Eq, Show, Typeable)

type TimerCallback = Word32 -> IO RetriggerTimer

newtype TimerID = TimerID CInt
  deriving (Eq, Show, Typeable)

addTimer :: MonadIO m => Word32 -> TimerCallback -> m TimerID
addTimer timeout callback = liftIO $
  fmap TimerID $ do
    cb <- Raw.mkTimerCallback $ wrapCb callback
    throwIf0 "addTimer" "SDL_AddTimer" $ Raw.addTimer timeout cb nullPtr
  where
    wrapCb :: TimerCallback -> Word32 -> Ptr () -> IO Word32
    wrapCb cb = \w _ -> do
      next <- cb w
      return $ case next of
        Cancel       -> 0
        Reschedule n -> n

removeTimer :: MonadIO m => TimerID -> m Bool
removeTimer (TimerID t) = Raw.removeTimer t
