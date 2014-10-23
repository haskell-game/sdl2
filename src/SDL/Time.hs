module SDL.Time
  ( -- * Time Measurement
    ticks
  , time
  ) where

import Data.Word

import qualified SDL.Raw.Timer as Raw

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
