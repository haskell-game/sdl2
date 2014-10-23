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

import Data.Word
import Foreign
import Foreign.C

import SDL.Exception

import qualified SDL.Raw.Timer as Raw
import qualified SDL.Raw.Types as Raw


-- | Number of milliseconds since library initialization.
ticks :: IO Word32
ticks = Raw.getTicks

-- | The current time in seconds since some arbitrary starting point.
time :: Fractional a => IO a
time = do
  freq <- Raw.getPerformanceFrequency
  cnt <- Raw.getPerformanceCounter
  return $ fromIntegral cnt / fromIntegral freq

delay :: Word32 -> IO ()
delay = Raw.delay

foreign import ccall "wrapper"
  mkTimerCallback :: (Word32 -> Ptr () -> IO Word32) -> IO Raw.TimerCallback

data RetriggerTimer = Reschedule Word32 | Cancel

type TimerCallback = Word32 -> IO RetriggerTimer

newtype TimerID = TimerID CInt deriving (Eq, Show)

addTimer :: Word32 -> TimerCallback -> IO TimerID
addTimer timeout callback =
  fmap TimerID $ do
    cb <- mkTimerCallback $ wrapCb callback
    throwIf0 "addTimer" "SDL_AddTimer" $ Raw.addTimer timeout cb nullPtr
  where
    wrapCb :: TimerCallback -> Word32 -> Ptr () -> IO Word32
    wrapCb cb = \w _ -> do
      next <- cb w
      return $ case next of
        Cancel       -> 0
        Reschedule n -> n

removeTimer :: TimerID -> IO Bool
removeTimer (TimerID t) = Raw.removeTimer t
