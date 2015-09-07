{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

"SDL.Audio" provides a high-level API to SDL's audio device capabilities.

-}

module SDL.Audio
  ( -- * Managing 'AudioDevice's
    -- $audioDevice
    AudioDevice

    -- ** Opening and Closing 'AudioDevice's
  , openAudioDevice
  , closeAudioDevice
  , OpenDeviceSpec(..)
  , AudioDeviceUsage(..)
  , Channels(..)
  , Changeable(..)

    -- ** Working with Opened Devices
    -- *** Locking 'AudioDevice's
  , setAudioDeviceLocked
  , LockState(..)

    -- *** Switching Playback States
  , PlaybackState(..)
  , setAudioDevicePlaybackState

    -- *** Querying an 'AudioDevice's Status.
  , AudioDeviceStatus(..)
  , audioDeviceStatus

    -- ** 'AudioFormat'
  , AudioFormat(..)

    -- ** Enumerating 'AudioDevice's
  , getAudioDeviceNames

    -- * 'AudioSpec'
  , AudioSpec
  , audioSpecFreq
  , audioSpecFormat
  , audioSpecChannels
  , audioSpecSilence
  , audioSpecSize
  , audioSpecCallback

    -- * Audio Drivers
  , getAudioDrivers
  , currentAudioDriver
  , AudioDriver
  , audioDriverName

    -- * Explicit Initialization
  , audioInit
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Data (Data)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Int (Int8, Int16, Int32)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import SDL.Exception
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified SDL.Raw.Audio as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Types as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

{-

$audioDevice

In order to produce sound, you must first open an 'AudioDevice'. To do so,
pass an 'OpenDeviceSpec' to 'openAudioDevice'.

-}

-- | A specification to 'openAudioDevice', indicating the desired output format.
-- Note that many of these properties are 'Changeable', meaning that you can
-- choose whether or not SDL should interpret your specification as an
-- unbreakable request ('Mandate'), or as an approximation 'Desire'.
data OpenDeviceSpec = forall sampleType. OpenDeviceSpec
  { openDeviceFreq :: !(Changeable CInt)
    -- ^ The output audio frequency in herts.
  , openDeviceFormat :: !(Changeable (AudioFormat sampleType))
    -- ^ The format of audio that will be sampled from the output buffer.
  , openDeviceChannels :: !(Changeable Channels)
    -- ^ The amount of audio channels.
  , openDeviceSamples :: !Word16
    -- ^ Output audio buffer size in samples. This should be a power of 2.
  , openDeviceCallback :: forall actualSampleType. AudioFormat actualSampleType -> MV.IOVector actualSampleType -> IO ()
    -- ^ A callback to invoke whenever new sample data is required. The callback
    -- will be passed a single 'MV.MVector' that must be filled with audio data.
  , openDeviceUsage :: !AudioDeviceUsage
    -- ^ How you intend to use the opened 'AudioDevice' - either for outputting
    -- or capturing audio.
  , openDeviceName :: !(Maybe Text)
    -- ^ The name of the 'AudioDevice' that should be opened. If 'Nothing',
    -- any suitable 'AudioDevice' will be used.
  } deriving (Typeable)

-- | Attempt to open the closest matching 'AudioDevice', as specified by the
-- given 'OpenDeviceSpec'.
--
-- See @<https://wiki.libsdl.org/SDL_OpenAudioDevice SDL_OpenAudioDevice>@ for C documentation.
openAudioDevice :: MonadIO m => OpenDeviceSpec -> m (AudioDevice, AudioSpec)
openAudioDevice OpenDeviceSpec{..} = liftIO $
  maybeWith (BS.useAsCString . Text.encodeUtf8) openDeviceName $ \cDevName -> do
    anAudioFormatRef <- newIORef undefined
    cb <- Raw.mkAudioCallback $ \_ buffer len -> do
      fp <- newForeignPtr_ buffer
      anAudioFormat <- readIORef anAudioFormatRef
      case anAudioFormat of
        AnAudioFormat audioFormat ->
          case audioFormatStorable audioFormat of
            Dict -> openDeviceCallback audioFormat
                                       (MV.unsafeCast (MV.unsafeFromForeignPtr0 fp (fromIntegral len)))
    with (desiredSpec cb) $ \desiredSpecPtr ->
      alloca $ \actualSpecPtr -> do
        devId <- throwIf0 "SDL.Audio.openAudioDevice" "SDL_OpenAudioDevice" $
          Raw.openAudioDevice cDevName (encodeUsage openDeviceUsage) desiredSpecPtr actualSpecPtr changes
        actual <- peek actualSpecPtr
        let audioDevice = AudioDevice devId
            anAudioFormat = decodeAudioFormat (Raw.audioSpecFormat actual)
            spec =
              case anAudioFormat of
                AnAudioFormat audioFormat ->
                  AudioSpec { audioSpecFreq = Raw.audioSpecFreq actual
                            , audioSpecFormat = audioFormat
                            , audioSpecChannels = fromC "SDL.Audio.openAudioDevice" "audioSpecChannels" readChannels (Raw.audioSpecChannels actual)
                            , audioSpecSilence = Raw.audioSpecSilence actual
                            , audioSpecSize = Raw.audioSpecSize actual
                            , audioSpecSamples = Raw.audioSpecSamples actual
                            , audioSpecCallback = openDeviceCallback
                            }
        writeIORef anAudioFormatRef anAudioFormat
        return (audioDevice, spec)

  where
  changes = foldl (.|.) 0 [ foldChangeable (const Raw.SDL_AUDIO_ALLOW_FREQUENCY_CHANGE) (const 0) openDeviceFreq
                          , foldChangeable (const Raw.SDL_AUDIO_ALLOW_FORMAT_CHANGE) (const 0) openDeviceFormat
                          , foldChangeable (const Raw.SDL_AUDIO_ALLOW_CHANNELS_CHANGE) (const 0) openDeviceChannels
                          ]

  channelsToWord8 Mono = 1
  channelsToWord8 Stereo = 2
  channelsToWord8 Quad = 4
  channelsToWord8 FivePointOne = 6

  readChannels 1 = Just Mono
  readChannels 2 = Just Stereo
  readChannels 4 = Just Quad
  readChannels 6 = Just FivePointOne
  readChannels _ = Nothing

  desiredSpec cb = Raw.AudioSpec
    { Raw.audioSpecFreq = unpackChangeable openDeviceFreq
    , Raw.audioSpecFormat = encodeAudioFormat (unpackChangeable openDeviceFormat)
    , Raw.audioSpecChannels = channelsToWord8 (unpackChangeable openDeviceChannels)
    , Raw.audioSpecSilence = 0
    , Raw.audioSpecSize = 0
    , Raw.audioSpecSamples = openDeviceSamples
    , Raw.audioSpecCallback = cb
    , Raw.audioSpecUserdata = nullPtr
    }

  -- Witness the 'Storable' instance for the sample type of any audio format. Needed in order to use 'MV.unsafeCast'
  audioFormatStorable :: AudioFormat sampleType -> Dict (Storable sampleType)
  audioFormatStorable Signed8BitAudio = Dict
  audioFormatStorable Unsigned8BitAudio = Dict
  audioFormatStorable Signed16BitLEAudio = Dict
  audioFormatStorable Signed16BitBEAudio = Dict
  audioFormatStorable Signed16BitNativeAudio = Dict
  audioFormatStorable Unsigned16BitLEAudio = Dict
  audioFormatStorable Unsigned16BitBEAudio = Dict
  audioFormatStorable Unsigned16BitNativeAudio = Dict
  audioFormatStorable Signed32BitLEAudio = Dict
  audioFormatStorable Signed32BitBEAudio = Dict
  audioFormatStorable Signed32BitNativeAudio = Dict
  audioFormatStorable FloatingLEAudio = Dict
  audioFormatStorable FloatingBEAudio = Dict
  audioFormatStorable FloatingNativeAudio = Dict

data Dict :: Constraint -> * where
  Dict :: c => Dict c

-- |
--
-- See @<https://wiki.libsdl.org/SDL_CloseAudioDevice SDL_CloseAudioDevice>@ for C documentation.
closeAudioDevice :: MonadIO m => AudioDevice -> m ()
closeAudioDevice (AudioDevice d) = Raw.closeAudioDevice d

-- | An open audio device. These can be created via 'openAudioDevice' and should be closed with 'closeAudioDevice'
newtype AudioDevice = AudioDevice (Raw.AudioDeviceID)
  deriving (Eq, Typeable)

-- | Enumerate all 'AudioDevice's attached to this system, that can be used as
-- specified by the given 'AudioDeviceUsage'. SDL cannot always guarantee
-- that this list can be produced, in which case 'Nothing' will be returned.
getAudioDeviceNames :: MonadIO m => AudioDeviceUsage -> m (Maybe (V.Vector Text))
getAudioDeviceNames usage = liftIO $ do
  n <- Raw.getNumAudioDevices usage'
  if n == -1
    then return Nothing
    else fmap (Just . V.fromList) $
         for [0 .. (n - 1)] $ \i -> do
           cstr <- throwIfNull "SDL.Audio.getAudioDeviceNames" "SDL_GetAudioDeviceName" $
             Raw.getAudioDeviceName i usage'
           Text.decodeUtf8 <$> BS.packCString cstr

  where usage' = encodeUsage usage

-- | Information about what format an audio bytestream is. The type variable
-- @t@ indicates the type used for audio buffer samples. It is determined
-- by the choice of the provided 'SampleBitSize'. For example:
--
-- @AudioFormat UnsignedInteger Sample8Bit Native :: AudioFormat Word8@
--
-- Indicating that an 8-bit audio format in the platforms native endianness
-- uses a buffer of 'Word8' values.
data AudioFormat sampleType where
  Signed8BitAudio :: AudioFormat Int8
  Unsigned8BitAudio :: AudioFormat Word8
  Signed16BitLEAudio :: AudioFormat Int16
  Signed16BitBEAudio :: AudioFormat Int16
  Signed16BitNativeAudio :: AudioFormat Int16
  Unsigned16BitLEAudio :: AudioFormat Word16
  Unsigned16BitBEAudio :: AudioFormat Word16
  Unsigned16BitNativeAudio :: AudioFormat Word16
  Signed32BitLEAudio :: AudioFormat Int32
  Signed32BitBEAudio :: AudioFormat Int32
  Signed32BitNativeAudio :: AudioFormat Int32
  FloatingLEAudio :: AudioFormat Float
  FloatingBEAudio :: AudioFormat Float
  FloatingNativeAudio :: AudioFormat Float

deriving instance Eq (AudioFormat sampleType)
deriving instance Ord (AudioFormat sampleType)
deriving instance Show (AudioFormat sampleType)

data AnAudioFormat where
  AnAudioFormat :: AudioFormat sampleType -> AnAudioFormat

encodeAudioFormat :: AudioFormat sampleType -> Word16
encodeAudioFormat Signed8BitAudio = Raw.SDL_AUDIO_S8
encodeAudioFormat Unsigned8BitAudio = Raw.SDL_AUDIO_U8
encodeAudioFormat Signed16BitLEAudio = Raw.SDL_AUDIO_S16LSB
encodeAudioFormat Signed16BitBEAudio = Raw.SDL_AUDIO_S16MSB
encodeAudioFormat Signed16BitNativeAudio = Raw.SDL_AUDIO_S16SYS
encodeAudioFormat Unsigned16BitLEAudio = Raw.SDL_AUDIO_U16LSB
encodeAudioFormat Unsigned16BitBEAudio = Raw.SDL_AUDIO_U16MSB
encodeAudioFormat Unsigned16BitNativeAudio = Raw.SDL_AUDIO_U16SYS
encodeAudioFormat Signed32BitLEAudio = Raw.SDL_AUDIO_S32LSB
encodeAudioFormat Signed32BitBEAudio = Raw.SDL_AUDIO_S32MSB
encodeAudioFormat Signed32BitNativeAudio = Raw.SDL_AUDIO_S32SYS
encodeAudioFormat FloatingLEAudio = Raw.SDL_AUDIO_F32LSB
encodeAudioFormat FloatingBEAudio = Raw.SDL_AUDIO_F32MSB
encodeAudioFormat FloatingNativeAudio = Raw.SDL_AUDIO_F32SYS

decodeAudioFormat :: Word16 -> AnAudioFormat
decodeAudioFormat Raw.SDL_AUDIO_S8 = AnAudioFormat Signed8BitAudio
decodeAudioFormat Raw.SDL_AUDIO_U8 = AnAudioFormat Unsigned8BitAudio
decodeAudioFormat Raw.SDL_AUDIO_S16LSB = AnAudioFormat Signed16BitLEAudio
decodeAudioFormat Raw.SDL_AUDIO_S16MSB = AnAudioFormat Signed16BitBEAudio
decodeAudioFormat Raw.SDL_AUDIO_S16SYS = AnAudioFormat Signed16BitNativeAudio
decodeAudioFormat Raw.SDL_AUDIO_U16LSB = AnAudioFormat Unsigned16BitLEAudio
decodeAudioFormat Raw.SDL_AUDIO_U16MSB = AnAudioFormat Unsigned16BitBEAudio
decodeAudioFormat Raw.SDL_AUDIO_U16SYS = AnAudioFormat Unsigned16BitNativeAudio
decodeAudioFormat Raw.SDL_AUDIO_S32LSB = AnAudioFormat Signed32BitLEAudio
decodeAudioFormat Raw.SDL_AUDIO_S32MSB = AnAudioFormat Signed32BitBEAudio
decodeAudioFormat Raw.SDL_AUDIO_S32SYS = AnAudioFormat Signed32BitNativeAudio
decodeAudioFormat Raw.SDL_AUDIO_F32LSB = AnAudioFormat FloatingLEAudio
decodeAudioFormat Raw.SDL_AUDIO_F32MSB = AnAudioFormat FloatingBEAudio
decodeAudioFormat Raw.SDL_AUDIO_F32SYS = AnAudioFormat FloatingNativeAudio
decodeAudioFormat x = error ("decodeAudioFormat failed: Unknown format " ++ show x)

-- | How many channels audio should be played on
data Channels
  = Mono -- ^ A single speaker configuration
  | Stereo -- ^ A traditional left/right stereo system
  | Quad
  | FivePointOne -- ^ 5.1 surround sound
  deriving (Bounded,Data,Enum,Eq,Generic,Ord,Read,Show,Typeable)

-- | 'AudioSpec' is the concrete specification of how an 'AudioDevice' was
-- sucessfully opened. Unlike 'OpenDeviceSpec', which specifies what you
-- /want/, 'AudioSpec' specifies what you /have/.
data AudioSpec = forall sampleType. AudioSpec
  { audioSpecFreq :: !CInt
    -- ^ DSP frequency (samples per second)
  , audioSpecFormat :: !(AudioFormat sampleType)
    -- ^ Audio data format
  , audioSpecChannels :: !Channels
    -- ^ Number of separate sound channels
  , audioSpecSilence :: !Word8
    -- ^ Calculated udio buffer silence value
  , audioSpecSamples :: !Word16
    -- ^ Audio buffer size in samples (power of 2)
  , audioSpecSize :: !Word32
    -- ^ Calculated audio buffer size in bytes
  , audioSpecCallback :: AudioFormat sampleType -> MV.IOVector sampleType -> IO ()
    -- ^ The function to call when the audio device needs more data
  }
  deriving (Typeable)

-- | How you intend to use an 'AudioDevice'
data AudioDeviceUsage
  = ForPlayback -- ^ The device will be used for sample playback.
  | ForCapture -- ^ The device will be used for sample capture.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

encodeUsage :: Num a => AudioDeviceUsage -> a
encodeUsage usage =
  case usage of
    ForPlayback -> 0
    ForCapture -> 1

-- | Used to indicate to SDL whether it is allowed to open other audio devices (if a property is marked as a 'Desire') or if it should fail if the device is unavailable ('Mandate').
data Changeable a
  = Mandate !a
    -- ^ 'Mandate' this exact property value, and fail if a matching audio device cannot be found.
  | Desire !a
    -- ^ 'Desire' this property value, but allow other audio devices to be opened.
  deriving (Data, Foldable, Functor, Eq, Generic, Read, Show, Traversable, Typeable)

foldChangeable :: (a -> b) -> (a -> b) -> Changeable a -> b
foldChangeable f _ (Mandate a) = f a
foldChangeable _ g (Desire a) = g a

unpackChangeable :: Changeable a -> a
unpackChangeable = foldChangeable id id

-- | Whether a device should be locked or unlocked.
data LockState
  = Locked  -- ^ Lock the device, preventing the callback from producing data.
  | Unlocked -- ^ Unlock the device, resuming calls to the callback.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | Lock an 'AudioDevice' such that its associated callback will not be called
-- until the device is unlocked.
setAudioDeviceLocked :: MonadIO m => AudioDevice -> LockState -> m ()
setAudioDeviceLocked (AudioDevice d) Locked = Raw.lockAudioDevice d
setAudioDeviceLocked (AudioDevice d) Unlocked = Raw.unlockAudioDevice d

-- | Whether to allow an 'AudioDevice' to play sound or remain paused.
data PlaybackState
  = Pause -- ^ Pause the 'AudioDevice', which will stop producing/capturing audio.
  | Play -- ^ Resume the 'AudioDevice'.
  deriving (Bounded, Enum, Eq, Ord, Read, Data, Generic, Show, Typeable)

-- | Change the playback state of an 'AudioDevice'.
setAudioDevicePlaybackState :: MonadIO m => AudioDevice -> PlaybackState -> m ()
setAudioDevicePlaybackState (AudioDevice d) Pause = Raw.pauseAudioDevice d 1
setAudioDevicePlaybackState (AudioDevice d) Play = Raw.pauseAudioDevice d 0

-- | Opened devices are always 'Playing' or 'Paused' in normal circumstances. A
-- failing device may change its status to 'Stopped' at any time, and closing a
-- device will progress to 'Stopped' too.
data AudioDeviceStatus
  = Playing -- ^ The 'AudioDevice' is playing.
  | Paused -- ^ The 'AudioDevice' is paused.
  | Stopped -- ^ The 'AudioDevice' is stopped.
  deriving (Bounded, Enum, Eq, Ord, Read, Data, Generic, Show, Typeable)

-- | Query the state of an 'AudioDevice'.
audioDeviceStatus :: MonadIO m => AudioDevice -> m AudioDeviceStatus
audioDeviceStatus (AudioDevice d) = liftIO $
  fromC "SDL.Audio.audioDeviceStatus" "SDL_AudioStatus" readStatus <$> Raw.getAudioDeviceStatus d
  where
  readStatus n = case n of
    Raw.SDL_AUDIO_PLAYING -> Just Playing
    Raw.SDL_AUDIO_STOPPED -> Just Stopped
    Raw.SDL_AUDIO_PAUSED -> Just Paused
    _ -> Nothing

-- clearQueuedAudio :: AudioDevice -> IO ()
-- clearQueuedAudio (AudioDevice d) = Raw.clearQueuedAudio d

-- | An abstract description of an audio driver on the host machine.
newtype AudioDriver = AudioDriver Text
  deriving (Eq, Show, Typeable)

-- | Get the human readable name of an 'AudioDriver'
audioDriverName :: AudioDriver -> Text
audioDriverName (AudioDriver t) = t

-- | Obtain a list of all possible audio drivers for this system. These drivers
-- can be used to specificially initialize the audio system.
getAudioDrivers :: MonadIO m => m (V.Vector AudioDriver)
getAudioDrivers = liftIO $ do
  n <- Raw.getNumAudioDrivers
  fmap V.fromList $
    for [0 .. (n - 1)] $ \i -> do
      -- TODO This could return null if i is invalid, but it's not an SDL_Error.
      cstr <- Raw.getAudioDriver i
      AudioDriver . Text.decodeUtf8 <$> BS.packCString cstr

-- | Explicitly initialize the audio system against a specific
-- 'AudioDriver'. Note that most users will not need to do this, as the normal
-- initialization routines will already take care of this for you.
audioInit :: MonadIO m => AudioDriver -> m ()
audioInit (AudioDriver n) = liftIO $ BS.useAsCString (Text.encodeUtf8 n) $
  throwIfNeg_ "SDL.Audio.audioInit" "SDL_AudioInit" . Raw.audioInit

-- | Query SDL for the name of the currently initialized audio driver, if
-- possible. This will return 'Nothing' if no driver has been initialized.
currentAudioDriver :: MonadIO m => m (Maybe Text)
currentAudioDriver =
  liftIO $ maybePeek (fmap Text.decodeUtf8 . BS.packCString) =<< Raw.getCurrentAudioDriver
