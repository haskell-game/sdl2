module Graphics.UI.SDL.Haptic (
	-- * Force Feedback Support
	hapticClose,
	hapticDestroyEffect,
	hapticEffectSupported,
	hapticGetEffectStatus,
	hapticIndex,
	hapticName,
	hapticNewEffect,
	hapticNumAxes,
	hapticNumEffects,
	hapticNumEffectsPlaying,
	hapticOpen,
	hapticOpenFromJoystick,
	hapticOpenFromMouse,
	hapticOpened,
	hapticPause,
	hapticQuery,
	hapticRumbleInit,
	hapticRumblePlay,
	hapticRumbleStop,
	hapticRumbleSupported,
	hapticRunEffect,
	hapticSetAutocenter,
	hapticSetGain,
	hapticStopAll,
	hapticStopEffect,
	hapticUnpause,
	hapticUpdateEffect,
	joystickIsHaptic,
	mouseIsHaptic,
	numHaptics
) where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_HapticClose" hapticClose :: Haptic -> IO ()
foreign import ccall "SDL.h SDL_HapticDestroyEffect" hapticDestroyEffect :: Haptic -> CInt -> IO ()
foreign import ccall "SDL.h SDL_HapticEffectSupported" hapticEffectSupported :: Haptic -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_HapticGetEffectStatus" hapticGetEffectStatus :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticIndex" hapticIndex :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticName" hapticName :: CInt -> IO CString
foreign import ccall "SDL.h SDL_HapticNewEffect" hapticNewEffect :: Haptic -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumAxes" hapticNumAxes :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumEffects" hapticNumEffects :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumEffectsPlaying" hapticNumEffectsPlaying :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticOpen" hapticOpen :: CInt -> IO Haptic
foreign import ccall "SDL.h SDL_HapticOpenFromJoystick" hapticOpenFromJoystick :: Joystick -> IO Haptic
foreign import ccall "SDL.h SDL_HapticOpenFromMouse" hapticOpenFromMouse :: IO Haptic
foreign import ccall "SDL.h SDL_HapticOpened" hapticOpened :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticPause" hapticPause :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticQuery" hapticQuery :: Haptic -> IO CUInt
foreign import ccall "SDL.h SDL_HapticRumbleInit" hapticRumbleInit :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumblePlay" hapticRumblePlay :: Haptic -> CFloat -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumbleStop" hapticRumbleStop :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumbleSupported" hapticRumbleSupported :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRunEffect" hapticRunEffect :: Haptic -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_HapticSetAutocenter" hapticSetAutocenter :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticSetGain" hapticSetGain :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticStopAll" hapticStopAll :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticStopEffect" hapticStopEffect :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticUnpause" hapticUnpause :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticUpdateEffect" hapticUpdateEffect :: Haptic -> CInt -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_JoystickIsHaptic" joystickIsHaptic :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_MouseIsHaptic" mouseIsHaptic :: IO CInt
foreign import ccall "SDL.h SDL_NumHaptics" numHaptics :: IO CInt
