{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module SDL.Haptic
  ( AvailableHapticDevice
  , availableHapticDeviceName
  , availableHapticDeviceIds
  , OpenHapticDevice(..)
  , openHaptic
  , closeHaptic
  , hapticRumbleInit
  , hapticRumblePlay
  , hapticRumbleStop
  , HapticDevice
  , hapticDeviceName
  , hapticDeviceNumAxes
  , Effect(..)
  , EffectEnvelope(..)
  , EffectType(..)
  , uploadEffect
  , runEffect
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable
import Foreign
import Foreign.C
import GHC.Generics (Generic)
import SDL.Internal.Types (Joystick(..))
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

data AvailableHapticDevice = AvailableHapticDevice
  { availableHapticDeviceName :: Text
  , availableHapticDeviceIndex :: CInt
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

availableHapticDeviceIds :: MonadIO m => m (V.Vector AvailableHapticDevice)
availableHapticDeviceIds = liftIO $ do
  n <- SDLEx.throwIfNeg "SDL.Haptic.availableHapticDevices" "SDL_NumHaptics" Raw.numHaptics
  fmap V.fromList $
    for [0 .. (n - 1)] $ \i -> do
      cstr <- SDLEx.throwIfNull "SDL.Haptic.availableHapticDevices" "SDL_HapticName" $
        Raw.hapticName i
      name <- Text.decodeUtf8 <$> BS.packCString cstr
      return (AvailableHapticDevice name i)

data OpenHapticDevice = OpenHapticMouse | OpenHapticJoystick Joystick | OpenHapticDevice AvailableHapticDevice
  deriving (Eq, Generic, Ord, Show, Typeable)

data HapticDevice = HapticDevice
  { hapticDevicePtr :: Raw.Haptic
  , hapticDeviceName :: Text
  , hapticDeviceNumAxes :: CInt
  } deriving (Eq, Ord, Show, Typeable)

openHaptic :: MonadIO m => OpenHapticDevice -> m HapticDevice
openHaptic o = liftIO $ do
  ptr <-
    case o of
      OpenHapticMouse ->
        SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_OpenHapticFromMouse" $
        Raw.hapticOpenFromMouse

      OpenHapticJoystick j ->
        SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_OpenHapticFromJoystick" $
        Raw.hapticOpenFromJoystick (joystickPtr j)

      OpenHapticDevice d ->
        SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_OpenHaptic" $
        Raw.hapticOpen (availableHapticDeviceIndex d)

  i <- SDLEx.throwIfNeg "SDL.Haptic.openHaptic" "SDL_HapticIndex" $
        Raw.hapticIndex ptr

  n <- do
    cstr <- SDLEx.throwIfNull "SDL.Haptic.openHaptic" "SDL_HapticName" $
            Raw.hapticName i
    Text.decodeUtf8 <$> BS.packCString cstr

  axes <- SDLEx.throwIfNeg "SDL.Haptic.openHaptic" "SDL_HapticNumAxes" $
          Raw.hapticNumAxes ptr

  return (HapticDevice ptr n axes)

closeHaptic :: MonadIO m => HapticDevice -> m ()
closeHaptic (HapticDevice h _ _) = Raw.hapticClose h

hapticRumbleInit :: MonadIO m => HapticDevice -> m ()
hapticRumbleInit (HapticDevice h _ _) =
  liftIO $
  SDLEx.throwIfNeg_ "SDL.Haptic.hapticRumbleInit" "SDL_HapticRumbleInit" $
  Raw.hapticRumbleInit h

hapticRumblePlay :: MonadIO m => HapticDevice -> CFloat -> Word32 -> m ()
hapticRumblePlay (HapticDevice h _ _) strength length =
  liftIO $
  SDLEx.throwIfNot0_ "SDL.Haptic.hapticRumblePlay" "SDL_HapticRumblePlay" $
  Raw.hapticRumblePlay h strength length

hapticRumbleStop :: MonadIO m => HapticDevice -> m ()
hapticRumbleStop (HapticDevice h _ _) =
  liftIO $
  SDLEx.throwIfNot0_ "SDL.Haptic.hapticRumbleStop" "SDL_HapticRumbleStop" $
  Raw.hapticRumbleStop h

newtype EffectId = EffectId CInt

uploadEffect :: (MonadIO m) => HapticDevice -> Effect -> m EffectId
uploadEffect (HapticDevice h _ _) effect =
  liftIO (do ptr <-
               new (case effectType effect of
                      HapticConstant dir lev (EffectEnvelope attackLen attackLev fadeLen fadeLev) ->
                        Raw.HapticConstant {Raw.hapticEffectType = Raw.SDL_HAPTIC_CONSTANT
                                           ,Raw.hapticConstantLength = effectLength effect
                                           ,Raw.hapticConstantDelay = effectDelay effect
                                           ,Raw.hapticConstantButton = effectButton effect
                                           ,Raw.hapticConstantInterval = effectInterval effect
                                           ,Raw.hapticConstantLevel = lev
                                           ,Raw.hapticConstantAttackLength = attackLen
                                           ,Raw.hapticConstantAttackLevel = attackLev
                                           ,Raw.hapticConstantFadeLength = fadeLen
                                           ,Raw.hapticConstantFadeLevel = fadeLev
                                           ,Raw.hapticConstantDirection = dir}
                      HapticPeriodic shape dir period mag offset phase (EffectEnvelope attackLen attackLev fadeLen fadeLev) ->
                        Raw.HapticPeriodic {Raw.hapticEffectType =
                                              case shape of
                                                HapticSine -> 2
                                           ,Raw.hapticPeriodicLength = effectLength effect
                                           ,Raw.hapticPeriodicDelay = effectDelay effect
                                           ,Raw.hapticPeriodicButton = effectButton effect
                                           ,Raw.hapticPeriodicInterval = effectInterval effect
                                           ,
                                              --,Raw.hapticPeriodicLevel = lev
                                              Raw.hapticPeriodicAttackLength = attackLen
                                           ,Raw.hapticPeriodicAttackLevel = attackLev
                                           ,Raw.hapticPeriodicFadeLength = fadeLen
                                           ,Raw.hapticPeriodicFadeLevel = fadeLev
                                           ,Raw.hapticPeriodicDirection = dir
                                           ,Raw.hapticPeriodicMagnitude = mag
                                           ,Raw.hapticPeriodicPeriod = period
                                           ,Raw.hapticPeriodicOffset = offset
                                           ,Raw.hapticPeriodicPhase = phase})
             fmap EffectId
                  (SDLEx.throwIfNeg "SDL.Haptic.uploadEffect"
                                    "SDL_HapticNewEffect"
                                    (Raw.hapticNewEffect h ptr)))

runEffect :: (Functor m, MonadIO m) => HapticDevice -> EffectId -> Word32 -> m ()
runEffect (HapticDevice h _ _) (EffectId e) x =
  SDLEx.throwIfNeg_ "SDL.Haptic.runEffect"
                    "SDL_HapticRunEffect"
                    (Raw.hapticRunEffect h e x)

data EffectEnvelope = EffectEnvelope
  { envelopeAttackLength :: Word16
  , envelopeAttackLevel :: Word16
  , envelopeFadeLength :: Word16
  , envelopeFadeLevel :: Word16
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

data Effect = Haptic
  { effectLength :: Word32
  , effectDelay :: Word16
  , effectButton :: Word16
  , effectInterval :: Word16
  , effectType :: EffectType
  } deriving (Eq, Generic, Show, Typeable)

data EffectShape
  = HapticSine
  | HapticSquare
  | HapticTriangle
  | HapticSawtoothUp
  | HapticSawtoothDown
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data ConditionType = Spring | Damper | Inertia | Friction
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

data EffectType
  = HapticConstant {hapticConstantDirection :: Raw.HapticDirection
                   ,hapticConstantLevel :: Int16
                   ,hapticConstantEnvelope :: EffectEnvelope}
  | HapticPeriodic {hapticPeriodicShape :: EffectShape
                   ,hapticPeriodicDirection :: Raw.HapticDirection
                   ,hapticPeriodicPeriod :: Word16
                   ,hapticPeriodicMagnitude :: Int16
                   ,hapticPeriodicOffset :: Int16
                   ,hapticPeriodicPhase :: Word16
                   ,hapticPeriodicEnvelope :: EffectEnvelope}
  | HapticCondition {hapticConditionType :: ConditionType
                    ,hapticConditionRightSat :: [Word16]
                    ,hapticConditionLeftSat :: [Word16]
                    ,hapticConditionRightCoeff :: [Int16]
                    ,hapticConditionLeftCoeff :: [Int16]
                    ,hapticConditionDeadband :: [Word16]
                    ,hapticConditionCenter :: [Int16]}
  | HapticRamp {hapticRampDirection :: Raw.HapticDirection
               ,hapticRampStart :: Int16
               ,hapticRampEnd :: Int16
               ,hapticRampEnvelope :: EffectEnvelope}
  | HapticLeftRight {hapticLeftRightLength :: Word32
                    ,hapticLeftRightLargeMagnitude :: Word16
                    ,hapticLeftRightSmallMagnitude :: Word16}
  | HapticCustom {hapticCustomDirection :: Raw.HapticDirection
                 ,hapticCustomChannels :: Word8
                 ,hapticCustomPeriod :: Word16
                 ,hapticCustomSamples :: V.Vector Word16
                 ,hapticCustomEnvelope :: EffectEnvelope}
  deriving (Eq, Generic, Show, Typeable)
