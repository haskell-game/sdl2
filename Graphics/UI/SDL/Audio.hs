module Graphics.UI.SDL.Audio (
  -- * Audio Device Management, Playing and Recording
  audioInit,
  audioQuit,
  buildAudioCVT,
  closeAudio,
  closeAudioDevice,
  convertAudio,
  freeWAV,
  getAudioDeviceName,
  getAudioDeviceStatus,
  getAudioDriver,
  getAudioStatus,
  getCurrentAudioDriver,
  getNumAudioDevices,
  getNumAudioDrivers,
  loadWAV,
  loadWAV_RW,
  lockAudio,
  lockAudioDevice,
  mixAudio,
  mixAudioFormat,
  openAudio,
  openAudioDevice,
  pauseAudio,
  pauseAudioDevice,
  unlockAudio,
  unlockAudioDevice
) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Filesystem
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_AudioInit" audioInit' :: CString -> IO CInt
foreign import ccall "SDL.h SDL_AudioQuit" audioQuit' :: IO ()
foreign import ccall "SDL.h SDL_BuildAudioCVT" buildAudioCVT' :: Ptr AudioCVT -> AudioFormat -> Word8 -> CInt -> AudioFormat -> Word8 -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_CloseAudio" closeAudio' :: IO ()
foreign import ccall "SDL.h SDL_CloseAudioDevice" closeAudioDevice' :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_ConvertAudio" convertAudio' :: Ptr AudioCVT -> IO CInt
foreign import ccall "SDL.h SDL_FreeWAV" freeWAV' :: Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetAudioDeviceName" getAudioDeviceName' :: CInt -> CInt -> IO CString
foreign import ccall "SDL.h SDL_GetAudioDeviceStatus" getAudioDeviceStatus' :: AudioDeviceID -> IO AudioStatus
foreign import ccall "SDL.h SDL_GetAudioDriver" getAudioDriver' :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetAudioStatus" getAudioStatus' :: IO AudioStatus
foreign import ccall "SDL.h SDL_GetCurrentAudioDriver" getCurrentAudioDriver' :: IO CString
foreign import ccall "SDL.h SDL_GetNumAudioDevices" getNumAudioDevices' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetNumAudioDrivers" getNumAudioDrivers' :: IO CInt
foreign import ccall "SDL.h SDL_LoadWAV_RW" loadWAV_RW' :: Ptr RWops -> CInt -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO (Ptr AudioSpec)
foreign import ccall "SDL.h SDL_LockAudio" lockAudio' :: IO ()
foreign import ccall "SDL.h SDL_LockAudioDevice" lockAudioDevice' :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_MixAudio" mixAudio' :: Ptr Word8 -> Ptr Word8 -> Word32 -> CInt -> IO ()
foreign import ccall "SDL.h SDL_MixAudioFormat" mixAudioFormat' :: Ptr Word8 -> Ptr Word8 -> AudioFormat -> Word32 -> CInt -> IO ()
foreign import ccall "SDL.h SDL_OpenAudio" openAudio' :: Ptr AudioSpec -> Ptr AudioSpec -> IO CInt
foreign import ccall "SDL.h SDL_OpenAudioDevice" openAudioDevice' :: CString -> CInt -> Ptr AudioSpec -> Ptr AudioSpec -> CInt -> IO AudioDeviceID
foreign import ccall "SDL.h SDL_PauseAudio" pauseAudio' :: CInt -> IO ()
foreign import ccall "SDL.h SDL_PauseAudioDevice" pauseAudioDevice' :: AudioDeviceID -> CInt -> IO ()
foreign import ccall "SDL.h SDL_UnlockAudio" unlockAudio' :: IO ()
foreign import ccall "SDL.h SDL_UnlockAudioDevice" unlockAudioDevice' :: AudioDeviceID -> IO ()

audioInit :: MonadIO m => CString -> m CInt
audioInit v1 = liftIO $ audioInit' v1
{-# INLINE audioInit #-}

audioQuit :: MonadIO m => m ()
audioQuit = liftIO audioQuit'
{-# INLINE audioQuit #-}

buildAudioCVT :: MonadIO m => Ptr AudioCVT -> AudioFormat -> Word8 -> CInt -> AudioFormat -> Word8 -> CInt -> m CInt
buildAudioCVT v1 v2 v3 v4 v5 v6 v7 = liftIO $ buildAudioCVT' v1 v2 v3 v4 v5 v6 v7
{-# INLINE buildAudioCVT #-}

closeAudio :: MonadIO m => m ()
closeAudio = liftIO closeAudio'
{-# INLINE closeAudio #-}

closeAudioDevice :: MonadIO m => AudioDeviceID -> m ()
closeAudioDevice v1 = liftIO $ closeAudioDevice' v1
{-# INLINE closeAudioDevice #-}

convertAudio :: MonadIO m => Ptr AudioCVT -> m CInt
convertAudio v1 = liftIO $ convertAudio' v1
{-# INLINE convertAudio #-}

freeWAV :: MonadIO m => Ptr Word8 -> m ()
freeWAV v1 = liftIO $ freeWAV' v1
{-# INLINE freeWAV #-}

getAudioDeviceName :: MonadIO m => CInt -> CInt -> m CString
getAudioDeviceName v1 v2 = liftIO $ getAudioDeviceName' v1 v2
{-# INLINE getAudioDeviceName #-}

getAudioDeviceStatus :: MonadIO m => AudioDeviceID -> m AudioStatus
getAudioDeviceStatus v1 = liftIO $ getAudioDeviceStatus' v1
{-# INLINE getAudioDeviceStatus #-}

getAudioDriver :: MonadIO m => CInt -> m CString
getAudioDriver v1 = liftIO $ getAudioDriver' v1
{-# INLINE getAudioDriver #-}

getAudioStatus :: MonadIO m => m AudioStatus
getAudioStatus = liftIO getAudioStatus'
{-# INLINE getAudioStatus #-}

getCurrentAudioDriver :: MonadIO m => m CString
getCurrentAudioDriver = liftIO getCurrentAudioDriver'
{-# INLINE getCurrentAudioDriver #-}

getNumAudioDevices :: MonadIO m => CInt -> m CInt
getNumAudioDevices v1 = liftIO $ getNumAudioDevices' v1
{-# INLINE getNumAudioDevices #-}

getNumAudioDrivers :: MonadIO m => m CInt
getNumAudioDrivers = liftIO getNumAudioDrivers'
{-# INLINE getNumAudioDrivers #-}

loadWAV :: MonadIO m => CString -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> m (Ptr AudioSpec)
loadWAV file spec audio_buf audio_len = liftIO $ do
  rw <- withCString "rb" $ rwFromFile file
  loadWAV_RW rw 1 spec audio_buf audio_len
{-# INLINE loadWAV #-}

loadWAV_RW :: MonadIO m => Ptr RWops -> CInt -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> m (Ptr AudioSpec)
loadWAV_RW v1 v2 v3 v4 v5 = liftIO $ loadWAV_RW' v1 v2 v3 v4 v5
{-# INLINE loadWAV_RW #-}

lockAudio :: MonadIO m => m ()
lockAudio = liftIO lockAudio'
{-# INLINE lockAudio #-}

lockAudioDevice :: MonadIO m => AudioDeviceID -> m ()
lockAudioDevice v1 = liftIO $ lockAudioDevice' v1
{-# INLINE lockAudioDevice #-}

mixAudio :: MonadIO m => Ptr Word8 -> Ptr Word8 -> Word32 -> CInt -> m ()
mixAudio v1 v2 v3 v4 = liftIO $ mixAudio' v1 v2 v3 v4
{-# INLINE mixAudio #-}

mixAudioFormat :: MonadIO m => Ptr Word8 -> Ptr Word8 -> AudioFormat -> Word32 -> CInt -> m ()
mixAudioFormat v1 v2 v3 v4 v5 = liftIO $ mixAudioFormat' v1 v2 v3 v4 v5
{-# INLINE mixAudioFormat #-}

openAudio :: MonadIO m => Ptr AudioSpec -> Ptr AudioSpec -> m CInt
openAudio v1 v2 = liftIO $ openAudio' v1 v2
{-# INLINE openAudio #-}

openAudioDevice :: MonadIO m => CString -> CInt -> Ptr AudioSpec -> Ptr AudioSpec -> CInt -> m AudioDeviceID
openAudioDevice v1 v2 v3 v4 v5 = liftIO $ openAudioDevice' v1 v2 v3 v4 v5
{-# INLINE openAudioDevice #-}

pauseAudio :: MonadIO m => CInt -> m ()
pauseAudio v1 = liftIO $ pauseAudio' v1
{-# INLINE pauseAudio #-}

pauseAudioDevice :: MonadIO m => AudioDeviceID -> CInt -> m ()
pauseAudioDevice v1 v2 = liftIO $ pauseAudioDevice' v1 v2
{-# INLINE pauseAudioDevice #-}

unlockAudio :: MonadIO m => m ()
unlockAudio = liftIO unlockAudio'
{-# INLINE unlockAudio #-}

unlockAudioDevice :: MonadIO m => AudioDeviceID -> m ()
unlockAudioDevice v1 = liftIO $ unlockAudioDevice' v1
{-# INLINE unlockAudioDevice #-}
