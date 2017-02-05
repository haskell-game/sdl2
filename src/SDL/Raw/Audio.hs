module SDL.Raw.Audio (
  -- * Audio Device Management, Playing and Recording
  audioInit,
  audioQuit,
  buildAudioCVT,
  clearQueuedAudio,
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
  getQueuedAudioSize,
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
  queueAudio,
  unlockAudio,
  unlockAudioDevice
) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum
import SDL.Raw.Filesystem
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_AudioInit" audioInitFFI :: CString -> IO CInt
foreign import ccall "SDL.h SDL_AudioQuit" audioQuitFFI :: IO ()
foreign import ccall "SDL.h SDL_BuildAudioCVT" buildAudioCVTFFI :: Ptr AudioCVT -> AudioFormat -> Word8 -> CInt -> AudioFormat -> Word8 -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_ClearQueuedAudio" clearQueuedAudioFFI :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_CloseAudio" closeAudioFFI :: IO ()
foreign import ccall "SDL.h SDL_CloseAudioDevice" closeAudioDeviceFFI :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_ConvertAudio" convertAudioFFI :: Ptr AudioCVT -> IO CInt
foreign import ccall "SDL.h SDL_FreeWAV" freeWAVFFI :: Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetAudioDeviceName" getAudioDeviceNameFFI :: CInt -> CInt -> IO CString
foreign import ccall "SDL.h SDL_GetAudioDeviceStatus" getAudioDeviceStatusFFI :: AudioDeviceID -> IO AudioStatus
foreign import ccall "SDL.h SDL_GetAudioDriver" getAudioDriverFFI :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetAudioStatus" getAudioStatusFFI :: IO AudioStatus
foreign import ccall "SDL.h SDL_GetCurrentAudioDriver" getCurrentAudioDriverFFI :: IO CString
foreign import ccall "SDL.h SDL_GetNumAudioDevices" getNumAudioDevicesFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetNumAudioDrivers" getNumAudioDriversFFI :: IO CInt
foreign import ccall "SDL.h SDL_GetQueuedAudioSize" getQueuedAudioSizeFFI :: AudioDeviceID -> IO Word32
foreign import ccall "SDL.h SDL_LoadWAV_RW" loadWAV_RWFFI :: Ptr RWops -> CInt -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO (Ptr AudioSpec)
foreign import ccall "SDL.h SDL_LockAudio" lockAudioFFI :: IO ()
foreign import ccall "SDL.h SDL_LockAudioDevice" lockAudioDeviceFFI :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_MixAudio" mixAudioFFI :: Ptr Word8 -> Ptr Word8 -> Word32 -> CInt -> IO ()
foreign import ccall "SDL.h SDL_MixAudioFormat" mixAudioFormatFFI :: Ptr Word8 -> Ptr Word8 -> AudioFormat -> Word32 -> CInt -> IO ()
foreign import ccall "SDL.h SDL_OpenAudio" openAudioFFI :: Ptr AudioSpec -> Ptr AudioSpec -> IO CInt
foreign import ccall "SDL.h SDL_OpenAudioDevice" openAudioDeviceFFI :: CString -> CInt -> Ptr AudioSpec -> Ptr AudioSpec -> CInt -> IO AudioDeviceID
foreign import ccall "SDL.h SDL_PauseAudio" pauseAudioFFI :: CInt -> IO ()
foreign import ccall "SDL.h SDL_PauseAudioDevice" pauseAudioDeviceFFI :: AudioDeviceID -> CInt -> IO ()
foreign import ccall "SDL.h SDL_QueueAudio" queueAudioFFI :: AudioDeviceID -> Ptr () -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_UnlockAudio" unlockAudioFFI :: IO ()
foreign import ccall "SDL.h SDL_UnlockAudioDevice" unlockAudioDeviceFFI :: AudioDeviceID -> IO ()

audioInit :: MonadIO m => CString -> m CInt
audioInit v1 = liftIO $ audioInitFFI v1
{-# INLINE audioInit #-}

audioQuit :: MonadIO m => m ()
audioQuit = liftIO audioQuitFFI
{-# INLINE audioQuit #-}

buildAudioCVT :: MonadIO m => Ptr AudioCVT -> AudioFormat -> Word8 -> CInt -> AudioFormat -> Word8 -> CInt -> m CInt
buildAudioCVT v1 v2 v3 v4 v5 v6 v7 = liftIO $ buildAudioCVTFFI v1 v2 v3 v4 v5 v6 v7
{-# INLINE buildAudioCVT #-}

clearQueuedAudio :: MonadIO m => AudioDeviceID -> m ()
clearQueuedAudio v1 = liftIO $ clearQueuedAudioFFI v1
{-# INLINE clearQueuedAudio #-}

closeAudio :: MonadIO m => m ()
closeAudio = liftIO closeAudioFFI
{-# INLINE closeAudio #-}

closeAudioDevice :: MonadIO m => AudioDeviceID -> m ()
closeAudioDevice v1 = liftIO $ closeAudioDeviceFFI v1
{-# INLINE closeAudioDevice #-}

convertAudio :: MonadIO m => Ptr AudioCVT -> m CInt
convertAudio v1 = liftIO $ convertAudioFFI v1
{-# INLINE convertAudio #-}

freeWAV :: MonadIO m => Ptr Word8 -> m ()
freeWAV v1 = liftIO $ freeWAVFFI v1
{-# INLINE freeWAV #-}

getAudioDeviceName :: MonadIO m => CInt -> CInt -> m CString
getAudioDeviceName v1 v2 = liftIO $ getAudioDeviceNameFFI v1 v2
{-# INLINE getAudioDeviceName #-}

getAudioDeviceStatus :: MonadIO m => AudioDeviceID -> m AudioStatus
getAudioDeviceStatus v1 = liftIO $ getAudioDeviceStatusFFI v1
{-# INLINE getAudioDeviceStatus #-}

getAudioDriver :: MonadIO m => CInt -> m CString
getAudioDriver v1 = liftIO $ getAudioDriverFFI v1
{-# INLINE getAudioDriver #-}

getAudioStatus :: MonadIO m => m AudioStatus
getAudioStatus = liftIO getAudioStatusFFI
{-# INLINE getAudioStatus #-}

getCurrentAudioDriver :: MonadIO m => m CString
getCurrentAudioDriver = liftIO getCurrentAudioDriverFFI
{-# INLINE getCurrentAudioDriver #-}

getNumAudioDevices :: MonadIO m => CInt -> m CInt
getNumAudioDevices v1 = liftIO $ getNumAudioDevicesFFI v1
{-# INLINE getNumAudioDevices #-}

getNumAudioDrivers :: MonadIO m => m CInt
getNumAudioDrivers = liftIO getNumAudioDriversFFI
{-# INLINE getNumAudioDrivers #-}

getQueuedAudioSize :: MonadIO m => AudioDeviceID -> m Word32
getQueuedAudioSize v1 = liftIO $ getQueuedAudioSizeFFI v1
{-# INLINE getQueuedAudioSize #-}

loadWAV :: MonadIO m => CString -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> m (Ptr AudioSpec)
loadWAV file spec audio_buf audio_len = liftIO $ do
  rw <- withCString "rb" $ rwFromFile file
  loadWAV_RW rw 1 spec audio_buf audio_len
{-# INLINE loadWAV #-}

loadWAV_RW :: MonadIO m => Ptr RWops -> CInt -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> m (Ptr AudioSpec)
loadWAV_RW v1 v2 v3 v4 v5 = liftIO $ loadWAV_RWFFI v1 v2 v3 v4 v5
{-# INLINE loadWAV_RW #-}

lockAudio :: MonadIO m => m ()
lockAudio = liftIO lockAudioFFI
{-# INLINE lockAudio #-}

lockAudioDevice :: MonadIO m => AudioDeviceID -> m ()
lockAudioDevice v1 = liftIO $ lockAudioDeviceFFI v1
{-# INLINE lockAudioDevice #-}

mixAudio :: MonadIO m => Ptr Word8 -> Ptr Word8 -> Word32 -> CInt -> m ()
mixAudio v1 v2 v3 v4 = liftIO $ mixAudioFFI v1 v2 v3 v4
{-# INLINE mixAudio #-}

mixAudioFormat :: MonadIO m => Ptr Word8 -> Ptr Word8 -> AudioFormat -> Word32 -> CInt -> m ()
mixAudioFormat v1 v2 v3 v4 v5 = liftIO $ mixAudioFormatFFI v1 v2 v3 v4 v5
{-# INLINE mixAudioFormat #-}

openAudio :: MonadIO m => Ptr AudioSpec -> Ptr AudioSpec -> m CInt
openAudio v1 v2 = liftIO $ openAudioFFI v1 v2
{-# INLINE openAudio #-}

openAudioDevice :: MonadIO m => CString -> CInt -> Ptr AudioSpec -> Ptr AudioSpec -> CInt -> m AudioDeviceID
openAudioDevice v1 v2 v3 v4 v5 = liftIO $ openAudioDeviceFFI v1 v2 v3 v4 v5
{-# INLINE openAudioDevice #-}

pauseAudio :: MonadIO m => CInt -> m ()
pauseAudio v1 = liftIO $ pauseAudioFFI v1
{-# INLINE pauseAudio #-}

pauseAudioDevice :: MonadIO m => AudioDeviceID -> CInt -> m ()
pauseAudioDevice v1 v2 = liftIO $ pauseAudioDeviceFFI v1 v2
{-# INLINE pauseAudioDevice #-}

queueAudio :: MonadIO m => AudioDeviceID -> Ptr () -> Word32 -> m CInt
queueAudio v1 v2 v3 = liftIO $ queueAudioFFI v1 v2 v3
{-# INLINE queueAudio #-}

unlockAudio :: MonadIO m => m ()
unlockAudio = liftIO unlockAudioFFI
{-# INLINE unlockAudio #-}

unlockAudioDevice :: MonadIO m => AudioDeviceID -> m ()
unlockAudioDevice v1 = liftIO $ unlockAudioDeviceFFI v1
{-# INLINE unlockAudioDevice #-}
