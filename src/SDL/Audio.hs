{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{-|

"SDL.Audio" provides a high-level API to SDL's audio device capabilities.

-}

module SDL.Audio
  ( -- * Managing 'AudioDevice's
    -- $audioDevice
    AudioDevice

    -- ** Opening and Closing 'AudioDevice's
  , OpenDeviceSpec(..)
  , AudioDeviceUsage(..)
  , Channels(..)
  , Changeable(..)
  , openAudioDevice
  , closeAudioDevice

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
  , AudioFormat

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
  , Raw.audioQuit
  ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Data (Data)
import Data.Foldable (Foldable)
import Data.Text (Text)
import Data.Traversable (Traversable, for)
import Data.Typeable
import Data.Vector.Storable (Vector)
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import SDL.Exception

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified SDL.Raw.Audio as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Types as Raw

{-

$audioDevice

In order to produce sound, you must first open an 'AudioDevice'. To do so,
pass an 'OpenDeviceSpec' to 'openAudioDevice'.

-}

-- | A specification to 'openAudioDevice', indicating the desired output format.
-- Note that many of these properties are 'Changeable', meaning that you can
-- choose whether or not SDL should interpret your specification as an
-- unbreakable request ('Mandate'), or as an approximation 'Desire'.
data OpenDeviceSpec = OpenDeviceSpec
  { openDeviceFreq :: !(Changeable CInt)
    -- ^ The output audio frequency in herts.
  , openDeviceFormat :: !(Changeable AudioFormat)
    -- ^ The format of audio that will be sampled from the output buffer.
  , openDeviceChannels :: !(Changeable Channels)
    -- ^ The amount of audio channels.
  , openDeviceSamples :: !Word16
    -- ^ Output audio buffer size in samples. This should be a power of 2.
  , openDeviceCallback :: !(CInt -> IO (Vector Word8))
    -- ^ A callback to invoke whenever new sample data is required. The callback
    -- will be passed a single 'CInt' - the number of bytes of data to produce -
    -- and should return a 'Vector' of those audio samples.
  , openDeviceUsage :: !AudioDeviceUsage
    -- ^ How you intend to use the opened 'AudioDevice' - either for outputting
    -- or capturing audio.
  , openDeviceName :: !(Maybe Text)
    -- ^ The name of the 'AudioDevice' that should be opened. If 'Nothing',
    -- any suitable 'AudioDevice' will be used.
  } deriving (Typeable)

-- | Attempt to open the closest matching 'AudioDevice', as specified by the
-- given 'OpenDeviceSpec'.
openAudioDevice :: MonadIO m => OpenDeviceSpec -> m (AudioDevice, AudioSpec)
openAudioDevice OpenDeviceSpec{..} = liftIO $
  maybeWith (BS.useAsCString . Text.encodeUtf8) openDeviceName $ \cDevName -> do
    cb <- Raw.mkAudioCallback $ \_ buffer len -> do
      v <- openDeviceCallback len
      let (vForeignPtr, len') = SV.unsafeToForeignPtr0 v
      withForeignPtr vForeignPtr $ \vPtr ->
        copyBytes buffer vPtr (min (fromIntegral len) (fromIntegral len'))
    with (desiredSpec cb) $ \desiredSpecPtr ->
      alloca $ \actualSpecPtr -> do
        devId <- throwIf0 "SDL.Audio.openAudioDevice" "SDL_OpenAudioDevice" $
          Raw.openAudioDevice cDevName (encodeUsage openDeviceUsage) desiredSpecPtr actualSpecPtr changes
        actual <- peek actualSpecPtr
        let audioDevice = AudioDevice devId
            spec = AudioSpec { audioSpecFreq = Raw.audioSpecFreq actual
                             , audioSpecFormat = AudioFormat (Raw.audioSpecFormat actual)
                             , audioSpecChannels = fromC "SDL.Audio.openAudioDevice" "audioSpecChannels" readChannels (Raw.audioSpecChannels actual)
                             , audioSpecSilence = Raw.audioSpecSilence actual
                             , audioSpecSize = Raw.audioSpecSize actual
                             , audioSpecSamples = Raw.audioSpecSamples actual
                             , audioSpecCallback = openDeviceCallback
                             }
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
    , Raw.audioSpecFormat = unAudioFormat (unpackChangeable openDeviceFormat)
    , Raw.audioSpecChannels = channelsToWord8 (unpackChangeable openDeviceChannels)
    , Raw.audioSpecSilence = 0
    , Raw.audioSpecSize = 0
    , Raw.audioSpecSamples = openDeviceSamples
    , Raw.audioSpecCallback = cb
    , Raw.audioSpecUserdata = nullPtr
    }

newtype AudioFormat = AudioFormat { unAudioFormat :: Word16 }
  deriving (Eq, Ord, Read, Show, Typeable)

closeAudioDevice :: MonadIO m => AudioDevice -> m ()
closeAudioDevice (AudioDevice d) = Raw.closeAudioDevice d

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

{-

audioFormatBitSize :: Lens' AudioFormat Word8
audioFormatFloat :: Lens' AudioFormat Bool
audioFormatBigEndian :: Lens' AudioFormat Bool
audioFormatSigned :: Lens' AudioFormat Bool

audioFormatU8 = AudioFormat 0 & audioFormatBitSize .~ 8
audioFormatS8 = audioFormatU8 & audioFormatSigned .~ True

audioFormatS16LSB = audioFormatS8 & audioFormatBitSize .~ 16
audioFormatS16MSB = audioFormatS16LSB & audioFormatBigEndian .~ True
audioFormatS16Sys = _
audioFormatS16 = audioFormatS16LSB
audioFormatU16LSB = audioFormatS16LSB & audioFormatSigned .~ False
audioFormatU16MSB = audioFormatS16MSB & audioFormatSigned .~ False
audioFormatU16Sys = _
audioFormatU16 = audioFormatU16LSB

audioFormatS32LSB = audioFormatS16LSB & audioFormatBitSize .~ 32
audioFormatS32MSB = audioFormatS16MSB & audioFormatBitSize .~ 32
audioFormatS32Sys = _
audioFormatS32 = audioFormatS32LSB

audioFormatF32LSB = audioFormatS32LSB & audioFormatFloat .~ True
audioFormatF32MSB = audioFormatS32MSB & audioFormatFloat .~ True
audioFormatF32Sys = _
audioFormatF32 = audioFormatF32LSB

-}

data Channels = Mono | Stereo | Quad | FivePointOne
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | 'AudioSpec' is the concrete specification of how an 'AudioDevice' was
-- sucessfully opened. Unlike 'OpenDeviceSpec', which specifies what you
-- /want/, 'AudioSpec' specifies what you /have/.
data AudioSpec = AudioSpec
  { audioSpecFreq :: !CInt
  , audioSpecFormat :: !AudioFormat
  , audioSpecChannels :: !Channels
  , audioSpecSilence :: !Word8
  , audioSpecSamples :: !Word16
  , audioSpecSize :: !Word32
  , audioSpecCallback :: !(CInt -> IO (Vector Word8))
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

data Changeable a
  = Mandate !a
  | Desire !a
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

newtype AudioDriver = AudioDriver Text
  deriving (Eq, Show, Typeable)

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
