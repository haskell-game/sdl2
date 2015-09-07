module SDL.Raw.Haptic (
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

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_HapticClose" hapticCloseFFI :: Haptic -> IO ()
foreign import ccall "SDL.h SDL_HapticDestroyEffect" hapticDestroyEffectFFI :: Haptic -> CInt -> IO ()
foreign import ccall "SDL.h SDL_HapticEffectSupported" hapticEffectSupportedFFI :: Haptic -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_HapticGetEffectStatus" hapticGetEffectStatusFFI :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticIndex" hapticIndexFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticName" hapticNameFFI :: CInt -> IO CString
foreign import ccall "SDL.h SDL_HapticNewEffect" hapticNewEffectFFI :: Haptic -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumAxes" hapticNumAxesFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumEffects" hapticNumEffectsFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumEffectsPlaying" hapticNumEffectsPlayingFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticOpen" hapticOpenFFI :: CInt -> IO Haptic
foreign import ccall "SDL.h SDL_HapticOpenFromJoystick" hapticOpenFromJoystickFFI :: Joystick -> IO Haptic
foreign import ccall "SDL.h SDL_HapticOpenFromMouse" hapticOpenFromMouseFFI :: IO Haptic
foreign import ccall "SDL.h SDL_HapticOpened" hapticOpenedFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticPause" hapticPauseFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticQuery" hapticQueryFFI :: Haptic -> IO CUInt
foreign import ccall "SDL.h SDL_HapticRumbleInit" hapticRumbleInitFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumblePlay" hapticRumblePlayFFI :: Haptic -> CFloat -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumbleStop" hapticRumbleStopFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumbleSupported" hapticRumbleSupportedFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRunEffect" hapticRunEffectFFI :: Haptic -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_HapticSetAutocenter" hapticSetAutocenterFFI :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticSetGain" hapticSetGainFFI :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticStopAll" hapticStopAllFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticStopEffect" hapticStopEffectFFI :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticUnpause" hapticUnpauseFFI :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticUpdateEffect" hapticUpdateEffectFFI :: Haptic -> CInt -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_JoystickIsHaptic" joystickIsHapticFFI :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_MouseIsHaptic" mouseIsHapticFFI :: IO CInt
foreign import ccall "SDL.h SDL_NumHaptics" numHapticsFFI :: IO CInt

hapticClose :: MonadIO m => Haptic -> m ()
hapticClose v1 = liftIO $ hapticCloseFFI v1
{-# INLINE hapticClose #-}

hapticDestroyEffect :: MonadIO m => Haptic -> CInt -> m ()
hapticDestroyEffect v1 v2 = liftIO $ hapticDestroyEffectFFI v1 v2
{-# INLINE hapticDestroyEffect #-}

hapticEffectSupported :: MonadIO m => Haptic -> Ptr HapticEffect -> m CInt
hapticEffectSupported v1 v2 = liftIO $ hapticEffectSupportedFFI v1 v2
{-# INLINE hapticEffectSupported #-}

hapticGetEffectStatus :: MonadIO m => Haptic -> CInt -> m CInt
hapticGetEffectStatus v1 v2 = liftIO $ hapticGetEffectStatusFFI v1 v2
{-# INLINE hapticGetEffectStatus #-}

hapticIndex :: MonadIO m => Haptic -> m CInt
hapticIndex v1 = liftIO $ hapticIndexFFI v1
{-# INLINE hapticIndex #-}

hapticName :: MonadIO m => CInt -> m CString
hapticName v1 = liftIO $ hapticNameFFI v1
{-# INLINE hapticName #-}

hapticNewEffect :: MonadIO m => Haptic -> Ptr HapticEffect -> m CInt
hapticNewEffect v1 v2 = liftIO $ hapticNewEffectFFI v1 v2
{-# INLINE hapticNewEffect #-}

hapticNumAxes :: MonadIO m => Haptic -> m CInt
hapticNumAxes v1 = liftIO $ hapticNumAxesFFI v1
{-# INLINE hapticNumAxes #-}

hapticNumEffects :: MonadIO m => Haptic -> m CInt
hapticNumEffects v1 = liftIO $ hapticNumEffectsFFI v1
{-# INLINE hapticNumEffects #-}

hapticNumEffectsPlaying :: MonadIO m => Haptic -> m CInt
hapticNumEffectsPlaying v1 = liftIO $ hapticNumEffectsPlayingFFI v1
{-# INLINE hapticNumEffectsPlaying #-}

hapticOpen :: MonadIO m => CInt -> m Haptic
hapticOpen v1 = liftIO $ hapticOpenFFI v1
{-# INLINE hapticOpen #-}

hapticOpenFromJoystick :: MonadIO m => Joystick -> m Haptic
hapticOpenFromJoystick v1 = liftIO $ hapticOpenFromJoystickFFI v1
{-# INLINE hapticOpenFromJoystick #-}

hapticOpenFromMouse :: MonadIO m => m Haptic
hapticOpenFromMouse = liftIO hapticOpenFromMouseFFI
{-# INLINE hapticOpenFromMouse #-}

hapticOpened :: MonadIO m => CInt -> m CInt
hapticOpened v1 = liftIO $ hapticOpenedFFI v1
{-# INLINE hapticOpened #-}

hapticPause :: MonadIO m => Haptic -> m CInt
hapticPause v1 = liftIO $ hapticPauseFFI v1
{-# INLINE hapticPause #-}

hapticQuery :: MonadIO m => Haptic -> m CUInt
hapticQuery v1 = liftIO $ hapticQueryFFI v1
{-# INLINE hapticQuery #-}

hapticRumbleInit :: MonadIO m => Haptic -> m CInt
hapticRumbleInit v1 = liftIO $ hapticRumbleInitFFI v1
{-# INLINE hapticRumbleInit #-}

hapticRumblePlay :: MonadIO m => Haptic -> CFloat -> Word32 -> m CInt
hapticRumblePlay v1 v2 v3 = liftIO $ hapticRumblePlayFFI v1 v2 v3
{-# INLINE hapticRumblePlay #-}

hapticRumbleStop :: MonadIO m => Haptic -> m CInt
hapticRumbleStop v1 = liftIO $ hapticRumbleStopFFI v1
{-# INLINE hapticRumbleStop #-}

hapticRumbleSupported :: MonadIO m => Haptic -> m CInt
hapticRumbleSupported v1 = liftIO $ hapticRumbleSupportedFFI v1
{-# INLINE hapticRumbleSupported #-}

hapticRunEffect :: MonadIO m => Haptic -> CInt -> Word32 -> m CInt
hapticRunEffect v1 v2 v3 = liftIO $ hapticRunEffectFFI v1 v2 v3
{-# INLINE hapticRunEffect #-}

hapticSetAutocenter :: MonadIO m => Haptic -> CInt -> m CInt
hapticSetAutocenter v1 v2 = liftIO $ hapticSetAutocenterFFI v1 v2
{-# INLINE hapticSetAutocenter #-}

hapticSetGain :: MonadIO m => Haptic -> CInt -> m CInt
hapticSetGain v1 v2 = liftIO $ hapticSetGainFFI v1 v2
{-# INLINE hapticSetGain #-}

hapticStopAll :: MonadIO m => Haptic -> m CInt
hapticStopAll v1 = liftIO $ hapticStopAllFFI v1
{-# INLINE hapticStopAll #-}

hapticStopEffect :: MonadIO m => Haptic -> CInt -> m CInt
hapticStopEffect v1 v2 = liftIO $ hapticStopEffectFFI v1 v2
{-# INLINE hapticStopEffect #-}

hapticUnpause :: MonadIO m => Haptic -> m CInt
hapticUnpause v1 = liftIO $ hapticUnpauseFFI v1
{-# INLINE hapticUnpause #-}

hapticUpdateEffect :: MonadIO m => Haptic -> CInt -> Ptr HapticEffect -> m CInt
hapticUpdateEffect v1 v2 v3 = liftIO $ hapticUpdateEffectFFI v1 v2 v3
{-# INLINE hapticUpdateEffect #-}

joystickIsHaptic :: MonadIO m => Joystick -> m CInt
joystickIsHaptic v1 = liftIO $ joystickIsHapticFFI v1
{-# INLINE joystickIsHaptic #-}

mouseIsHaptic :: MonadIO m => m CInt
mouseIsHaptic = liftIO mouseIsHapticFFI
{-# INLINE mouseIsHaptic #-}

numHaptics :: MonadIO m => m CInt
numHaptics = liftIO numHapticsFFI
{-# INLINE numHaptics #-}
