module SDL.Raw.Audio (
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

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum
import SDL.Raw.Filesystem
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_AudioInit" audioInit :: CString -> IO CInt
foreign import ccall "SDL.h SDL_AudioQuit" audioQuit :: IO ()
foreign import ccall "SDL.h SDL_BuildAudioCVT" buildAudioCVT :: Ptr AudioCVT -> AudioFormat -> Word8 -> CInt -> AudioFormat -> Word8 -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_CloseAudio" closeAudio :: IO ()
foreign import ccall "SDL.h SDL_CloseAudioDevice" closeAudioDevice :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_ConvertAudio" convertAudio :: Ptr AudioCVT -> IO CInt
foreign import ccall "SDL.h SDL_FreeWAV" freeWAV :: Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetAudioDeviceName" getAudioDeviceName :: CInt -> CInt -> IO CString
foreign import ccall "SDL.h SDL_GetAudioDeviceStatus" getAudioDeviceStatus :: AudioDeviceID -> IO AudioStatus
foreign import ccall "SDL.h SDL_GetAudioDriver" getAudioDriver :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetAudioStatus" getAudioStatus :: IO AudioStatus
foreign import ccall "SDL.h SDL_GetCurrentAudioDriver" getCurrentAudioDriver :: IO CString
foreign import ccall "SDL.h SDL_GetNumAudioDevices" getNumAudioDevices :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetNumAudioDrivers" getNumAudioDrivers :: IO CInt
foreign import ccall "SDL.h SDL_LoadWAV_RW" loadWAV_RW :: Ptr RWops -> CInt -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO (Ptr AudioSpec)
foreign import ccall "SDL.h SDL_LockAudio" lockAudio :: IO ()
foreign import ccall "SDL.h SDL_LockAudioDevice" lockAudioDevice :: AudioDeviceID -> IO ()
foreign import ccall "SDL.h SDL_MixAudio" mixAudio :: Ptr Word8 -> Ptr Word8 -> Word32 -> CInt -> IO ()
foreign import ccall "SDL.h SDL_MixAudioFormat" mixAudioFormat :: Ptr Word8 -> Ptr Word8 -> AudioFormat -> Word32 -> CInt -> IO ()
foreign import ccall "SDL.h SDL_OpenAudio" openAudio :: Ptr AudioSpec -> Ptr AudioSpec -> IO CInt
foreign import ccall "SDL.h SDL_OpenAudioDevice" openAudioDevice :: CString -> CInt -> Ptr AudioSpec -> Ptr AudioSpec -> CInt -> IO AudioDeviceID
foreign import ccall "SDL.h SDL_PauseAudio" pauseAudio :: CInt -> IO ()
foreign import ccall "SDL.h SDL_PauseAudioDevice" pauseAudioDevice :: AudioDeviceID -> CInt -> IO ()
foreign import ccall "SDL.h SDL_UnlockAudio" unlockAudio :: IO ()
foreign import ccall "SDL.h SDL_UnlockAudioDevice" unlockAudioDevice :: AudioDeviceID -> IO ()

loadWAV :: CString -> Ptr AudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO (Ptr AudioSpec)
loadWAV file spec audio_buf audio_len = do
	rw <- withCString "rb" $ rwFromFile file
	loadWAV_RW rw 1 spec audio_buf audio_len
