{-# LANGUAGE DeriveDataTypeable #-}
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
  , EffectType(..)
  ) where

import Control.Applicative
import Data.Text (Text)
import Data.Typeable
import Foreign
import Foreign.C
import Data.Traversable (for)
import SDL.Internal.Types (Joystick(..))

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

data AvailableHapticDevice = AvailableHapticDevice
  { availableHapticDeviceName :: Text
  , availableHapticDeviceIndex :: CInt
  } deriving (Eq, Show, Typeable)

availableHapticDeviceIds :: IO (V.Vector AvailableHapticDevice)
availableHapticDeviceIds = do
  n <- SDLEx.throwIfNeg "SDL.Haptic.availableHapticDevices" "SDL_NumHaptics" Raw.numHaptics
  fmap V.fromList $
    for [0 .. (n - 1)] $ \i -> do
      cstr <- SDLEx.throwIfNull "SDL.Haptic.availableHapticDevices" "SDL_HapticName" $
        Raw.hapticName i
      name <- Text.decodeUtf8 <$> BS.packCString cstr
      return (AvailableHapticDevice name i)

data OpenHapticDevice = OpenHapticMouse | OpenHapticJoystick Joystick | OpenHapticDevice AvailableHapticDevice

data HapticDevice = HapticDevice
  { hapticDevicePtr :: Raw.Haptic
  , hapticDeviceName :: Text
  , hapticDeviceNumAxes :: CInt
  } deriving (Eq, Show, Typeable)

openHaptic :: OpenHapticDevice -> IO HapticDevice
openHaptic o = do
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

closeHaptic :: HapticDevice -> IO ()
closeHaptic (HapticDevice h _ _) = Raw.hapticClose h

hapticRumbleInit :: HapticDevice -> IO ()
hapticRumbleInit (HapticDevice h _ _) =
  SDLEx.throwIfNeg_ "SDL.Haptic.hapticRumbleInit" "SDL_HapticRumbleInit" $
  Raw.hapticRumbleInit h

hapticRumblePlay :: HapticDevice -> CFloat -> Word32 -> IO ()
hapticRumblePlay (HapticDevice h _ _) strength length =
  SDLEx.throwIfNot0_ "SDL.Haptic.hapticRumblePlay" "SDL_HapticRumblePlay" $
  Raw.hapticRumblePlay h strength length

hapticRumbleStop :: HapticDevice -> IO ()
hapticRumbleStop (HapticDevice h _ _) =
  SDLEx.throwIfNot0_ "SDL.Haptic.hapticRumbleStop" "SDL_HapticRumbleStop" $
  Raw.hapticRumbleStop h

data EffectEnvelope = EffectEnvelope
  { envelopeAttackLength :: Word16
  , envelopeAttackLevel :: Word16
  , envelopeFadeLength :: Word16
  , envelopeFadeLevel :: Word16
  } deriving (Eq, Show, Typeable)

data Effect = Haptic
  { effectLength :: Word32
  , effectDelay :: Word16
  , effectButton :: Word16
  , effectInterval :: Word16
  , effectType :: EffectType
  } deriving (Eq, Show, Typeable)

data EffectShape
  = HapticSine
  | HapticSquare
  | HapticTriangle
  | HapticSawtoothUp
  | HapticSawtoothDown
  deriving (Eq, Show, Typeable)

data ConditionType = Spring | Damper | Inertia | Friction
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)
