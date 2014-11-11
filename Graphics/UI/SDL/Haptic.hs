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

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_HapticClose" hapticClose' :: Haptic -> IO ()
foreign import ccall "SDL.h SDL_HapticDestroyEffect" hapticDestroyEffect' :: Haptic -> CInt -> IO ()
foreign import ccall "SDL.h SDL_HapticEffectSupported" hapticEffectSupported' :: Haptic -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_HapticGetEffectStatus" hapticGetEffectStatus' :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticIndex" hapticIndex' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticName" hapticName' :: CInt -> IO CString
foreign import ccall "SDL.h SDL_HapticNewEffect" hapticNewEffect' :: Haptic -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumAxes" hapticNumAxes' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumEffects" hapticNumEffects' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticNumEffectsPlaying" hapticNumEffectsPlaying' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticOpen" hapticOpen' :: CInt -> IO Haptic
foreign import ccall "SDL.h SDL_HapticOpenFromJoystick" hapticOpenFromJoystick' :: Joystick -> IO Haptic
foreign import ccall "SDL.h SDL_HapticOpenFromMouse" hapticOpenFromMouse' :: IO Haptic
foreign import ccall "SDL.h SDL_HapticOpened" hapticOpened' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticPause" hapticPause' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticQuery" hapticQuery' :: Haptic -> IO CUInt
foreign import ccall "SDL.h SDL_HapticRumbleInit" hapticRumbleInit' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumblePlay" hapticRumblePlay' :: Haptic -> CFloat -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumbleStop" hapticRumbleStop' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRumbleSupported" hapticRumbleSupported' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticRunEffect" hapticRunEffect' :: Haptic -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_HapticSetAutocenter" hapticSetAutocenter' :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticSetGain" hapticSetGain' :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticStopAll" hapticStopAll' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticStopEffect" hapticStopEffect' :: Haptic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_HapticUnpause" hapticUnpause' :: Haptic -> IO CInt
foreign import ccall "SDL.h SDL_HapticUpdateEffect" hapticUpdateEffect' :: Haptic -> CInt -> Ptr HapticEffect -> IO CInt
foreign import ccall "SDL.h SDL_JoystickIsHaptic" joystickIsHaptic' :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_MouseIsHaptic" mouseIsHaptic' :: IO CInt
foreign import ccall "SDL.h SDL_NumHaptics" numHaptics' :: IO CInt

hapticClose :: MonadIO m => Haptic -> m ()
hapticClose v1 = liftIO $ hapticClose' v1
{-# INLINE hapticClose #-}

hapticDestroyEffect :: MonadIO m => Haptic -> CInt -> m ()
hapticDestroyEffect v1 v2 = liftIO $ hapticDestroyEffect' v1 v2
{-# INLINE hapticDestroyEffect #-}

hapticEffectSupported :: MonadIO m => Haptic -> Ptr HapticEffect -> m CInt
hapticEffectSupported v1 v2 = liftIO $ hapticEffectSupported' v1 v2
{-# INLINE hapticEffectSupported #-}

hapticGetEffectStatus :: MonadIO m => Haptic -> CInt -> m CInt
hapticGetEffectStatus v1 v2 = liftIO $ hapticGetEffectStatus' v1 v2
{-# INLINE hapticGetEffectStatus #-}

hapticIndex :: MonadIO m => Haptic -> m CInt
hapticIndex v1 = liftIO $ hapticIndex' v1
{-# INLINE hapticIndex #-}

hapticName :: MonadIO m => CInt -> m CString
hapticName v1 = liftIO $ hapticName' v1
{-# INLINE hapticName #-}

hapticNewEffect :: MonadIO m => Haptic -> Ptr HapticEffect -> m CInt
hapticNewEffect v1 v2 = liftIO $ hapticNewEffect' v1 v2
{-# INLINE hapticNewEffect #-}

hapticNumAxes :: MonadIO m => Haptic -> m CInt
hapticNumAxes v1 = liftIO $ hapticNumAxes' v1
{-# INLINE hapticNumAxes #-}

hapticNumEffects :: MonadIO m => Haptic -> m CInt
hapticNumEffects v1 = liftIO $ hapticNumEffects' v1
{-# INLINE hapticNumEffects #-}

hapticNumEffectsPlaying :: MonadIO m => Haptic -> m CInt
hapticNumEffectsPlaying v1 = liftIO $ hapticNumEffectsPlaying' v1
{-# INLINE hapticNumEffectsPlaying #-}

hapticOpen :: MonadIO m => CInt -> m Haptic
hapticOpen v1 = liftIO $ hapticOpen' v1
{-# INLINE hapticOpen #-}

hapticOpenFromJoystick :: MonadIO m => Joystick -> m Haptic
hapticOpenFromJoystick v1 = liftIO $ hapticOpenFromJoystick' v1
{-# INLINE hapticOpenFromJoystick #-}

hapticOpenFromMouse :: MonadIO m => m Haptic
hapticOpenFromMouse = liftIO hapticOpenFromMouse'
{-# INLINE hapticOpenFromMouse #-}

hapticOpened :: MonadIO m => CInt -> m CInt
hapticOpened v1 = liftIO $ hapticOpened' v1
{-# INLINE hapticOpened #-}

hapticPause :: MonadIO m => Haptic -> m CInt
hapticPause v1 = liftIO $ hapticPause' v1
{-# INLINE hapticPause #-}

hapticQuery :: MonadIO m => Haptic -> m CUInt
hapticQuery v1 = liftIO $ hapticQuery' v1
{-# INLINE hapticQuery #-}

hapticRumbleInit :: MonadIO m => Haptic -> m CInt
hapticRumbleInit v1 = liftIO $ hapticRumbleInit' v1
{-# INLINE hapticRumbleInit #-}

hapticRumblePlay :: MonadIO m => Haptic -> CFloat -> Word32 -> m CInt
hapticRumblePlay v1 v2 v3 = liftIO $ hapticRumblePlay' v1 v2 v3
{-# INLINE hapticRumblePlay #-}

hapticRumbleStop :: MonadIO m => Haptic -> m CInt
hapticRumbleStop v1 = liftIO $ hapticRumbleStop' v1
{-# INLINE hapticRumbleStop #-}

hapticRumbleSupported :: MonadIO m => Haptic -> m CInt
hapticRumbleSupported v1 = liftIO $ hapticRumbleSupported' v1
{-# INLINE hapticRumbleSupported #-}

hapticRunEffect :: MonadIO m => Haptic -> CInt -> Word32 -> m CInt
hapticRunEffect v1 v2 v3 = liftIO $ hapticRunEffect' v1 v2 v3
{-# INLINE hapticRunEffect #-}

hapticSetAutocenter :: MonadIO m => Haptic -> CInt -> m CInt
hapticSetAutocenter v1 v2 = liftIO $ hapticSetAutocenter' v1 v2
{-# INLINE hapticSetAutocenter #-}

hapticSetGain :: MonadIO m => Haptic -> CInt -> m CInt
hapticSetGain v1 v2 = liftIO $ hapticSetGain' v1 v2
{-# INLINE hapticSetGain #-}

hapticStopAll :: MonadIO m => Haptic -> m CInt
hapticStopAll v1 = liftIO $ hapticStopAll' v1
{-# INLINE hapticStopAll #-}

hapticStopEffect :: MonadIO m => Haptic -> CInt -> m CInt
hapticStopEffect v1 v2 = liftIO $ hapticStopEffect' v1 v2
{-# INLINE hapticStopEffect #-}

hapticUnpause :: MonadIO m => Haptic -> m CInt
hapticUnpause v1 = liftIO $ hapticUnpause' v1
{-# INLINE hapticUnpause #-}

hapticUpdateEffect :: MonadIO m => Haptic -> CInt -> Ptr HapticEffect -> m CInt
hapticUpdateEffect v1 v2 v3 = liftIO $ hapticUpdateEffect' v1 v2 v3
{-# INLINE hapticUpdateEffect #-}

joystickIsHaptic :: MonadIO m => Joystick -> m CInt
joystickIsHaptic v1 = liftIO $ joystickIsHaptic' v1
{-# INLINE joystickIsHaptic #-}

mouseIsHaptic :: MonadIO m => m CInt
mouseIsHaptic = liftIO mouseIsHaptic'
{-# INLINE mouseIsHaptic #-}

numHaptics :: MonadIO m => m CInt
numHaptics = liftIO numHaptics'
{-# INLINE numHaptics #-}
