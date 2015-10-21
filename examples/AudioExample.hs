{-# LANGUAGE GADTs #-}

module AudioExample where

import Data.IORef
import Control.Monad
import Control.Concurrent
import Data.Int (Int16)
import SDL
import Data.Vector.Storable.Mutable as V

sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 48000 :: Double
             freq = 440 * 4
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (t * freq)))
      [0 :: Int16 ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         sequence_ (zipWith (write buffer)
                            [0 ..]
                            (Prelude.take n samples'))
         writeIORef samples
                    (Prelude.drop n samples')
    _ -> error "Unsupported audio format"

main :: IO ()
main =
  do initializeAll
     samples <- newIORef sinSamples
     (device,_) <-
       openAudioDevice
         OpenDeviceSpec {SDL.openDeviceFreq =
                           Mandate 48000
                        ,SDL.openDeviceFormat =
                           Mandate Signed16BitNativeAudio
                        ,SDL.openDeviceChannels =
                           Mandate Mono
                        ,SDL.openDeviceSamples = 4096 * 2
                        ,SDL.openDeviceCallback = audioCB samples
                        ,SDL.openDeviceUsage = ForPlayback
                        ,SDL.openDeviceName = Nothing}
     setAudioDevicePlaybackState device Play
     forever (threadDelay maxBound)
