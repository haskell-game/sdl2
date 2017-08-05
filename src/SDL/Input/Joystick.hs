{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Input.Joystick
  ( numJoysticks
  , availableJoysticks
  , JoystickDevice(..)

  , openJoystick
  , closeJoystick

  , getJoystickID
  , Joystick
  , JoyButtonState(..)
  , buttonPressed
  , ballDelta
  , axisPosition
  , numAxes
  , numButtons
  , numBalls
  , JoyHatPosition(..)
  , getHat
  , numHats
  , JoyDeviceConnection(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Int
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import GHC.Generics (Generic)
import SDL.Vect
import SDL.Internal.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | A description of joystick that can be opened using 'openJoystick'. To retrieve a list of
-- connected joysticks, use 'availableJoysticks'.
data JoystickDevice = JoystickDevice
  { joystickDeviceName :: Text
  , joystickDeviceId :: CInt
  } deriving (Eq, Generic, Read, Ord, Show, Typeable)

-- | Identifies the state of a joystick button.
data JoyButtonState = JoyButtonPressed | JoyButtonReleased
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber JoyButtonState Word8 where
  fromNumber n = case n of
    Raw.SDL_PRESSED -> JoyButtonPressed
    Raw.SDL_RELEASED -> JoyButtonReleased
    _ -> JoyButtonReleased

-- | Count the number of joysticks attached to the system.
--
-- See @<https://wiki.libsdl.org/SDL_NumJoysticks SDL_NumJoysticks>@ for C documentation.
numJoysticks :: MonadIO m => m (CInt)
numJoysticks = throwIfNeg "SDL.Input.Joystick.availableJoysticks" "SDL_NumJoysticks" Raw.numJoysticks

-- | Enumerate all connected joysticks, retrieving a description of each.
availableJoysticks :: MonadIO m => m (V.Vector JoystickDevice)
availableJoysticks = liftIO $ do
  n <- numJoysticks
  fmap (V.fromList) $
    for [0 .. (n - 1)] $ \i -> do
      cstr <-
        throwIfNull "SDL.Input.Joystick.availableJoysticks" "SDL_JoystickNameForIndex" $
          Raw.joystickNameForIndex i
      name <- Text.decodeUtf8 <$> BS.packCString cstr
      return (JoystickDevice name i)

-- | Open a joystick so that you can start receiving events from interaction with this joystick.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickOpen SDL_JoystickOpen>@ for C documentation.
openJoystick :: (Functor m,MonadIO m)
             => JoystickDevice -- ^ The device to open. Use 'availableJoysticks' to find 'JoystickDevices's
             -> m Joystick
openJoystick (JoystickDevice _ x) =
  fmap Joystick $
  throwIfNull "SDL.Input.Joystick.openJoystick" "SDL_OpenJoystick" $
  Raw.joystickOpen x

-- | Close a joystick previously opened with 'openJoystick'.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickClose SDL_JoystickClose>@ for C documentation.
closeJoystick :: MonadIO m => Joystick -> m ()
closeJoystick (Joystick j) = Raw.joystickClose j

-- | Get the instance ID of an opened joystick. The instance ID is used to identify the joystick
-- in future SDL events.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickInstanceID SDL_JoystickInstanceID>@ for C documentation.
getJoystickID :: MonadIO m => Joystick -> m (Int32)
getJoystickID (Joystick j) =
  throwIfNeg "SDL.Input.Joystick.getJoystickID" "SDL_JoystickInstanceID" $
  Raw.joystickInstanceID j

-- | Determine if a given button is currently held.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickGetButton SDL_JoystickGetButton>@ for C documentation.
buttonPressed :: (Functor m, MonadIO m)
              => Joystick
              -> CInt -- ^ The index of the button. You can use 'numButtons' to determine how many buttons a given joystick has.
              -> m Bool
buttonPressed (Joystick j) buttonIndex = (== 1) <$> Raw.joystickGetButton j buttonIndex

-- | Get the ball axis change since the last poll.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickGetBall SDL_JoystickGetBall>@ for C documentation.
ballDelta :: MonadIO m
          => Joystick
          -> CInt -- ^ The index of the joystick ball. You can use 'numBalls' to determine how many balls a given joystick has.
          -> m (V2 CInt)
ballDelta (Joystick j) ballIndex = liftIO $
  alloca $ \xptr ->
  alloca $ \yptr -> do
    throwIfNeg_ "SDL.Input.Joystick.ballDelta" "SDL_JoystickGetBall" $
      Raw.joystickGetBall j ballIndex xptr yptr

    V2 <$> peek xptr <*> peek yptr

-- | Get the current state of an axis control on a joystick.
--
-- Returns a 16-bit signed integer representing the current position of the axis. The state is a value ranging from -32768 to 32767.
--
-- On most modern joysticks the x-axis is usually represented by axis 0 and the y-axis by axis 1. The value returned by 'axisPosition' is a signed integer (-32768 to 32767) representing the current position of the axis. It may be necessary to impose certain tolerances on these values to account for jitter.
--
-- Some joysticks use axes 2 and 3 for extra buttons.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickGetAxis SDL_JoystickGetAxis>@ for C documentation.
axisPosition :: MonadIO m => Joystick -> CInt -> m Int16
axisPosition (Joystick j) axisIndex = Raw.joystickGetAxis j axisIndex

-- | Get the number of general axis controls on a joystick.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickNumAxes SDL_JoystickNumAxes>@ for C documentation.
numAxes :: (MonadIO m) => Joystick -> m CInt
numAxes (Joystick j) = liftIO $ throwIfNeg "SDL.Input.Joystick.numAxis" "SDL_JoystickNumAxes" (Raw.joystickNumAxes j)

-- | Get the number of buttons on a joystick.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickNumButtons SDL_JoystickNumButtons>@ for C documentation.
numButtons :: (MonadIO m) => Joystick -> m CInt
numButtons (Joystick j) = liftIO $ throwIfNeg "SDL.Input.Joystick.numButtons" "SDL_JoystickNumButtons" (Raw.joystickNumButtons j)

-- | Get the number of trackballs on a joystick.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickNumBalls SDL_JoystickNumBalls>@ for C documentation.
numBalls :: (MonadIO m) => Joystick -> m CInt
numBalls (Joystick j) = liftIO $ throwIfNeg "SDL.Input.Joystick.numBalls" "SDL_JoystickNumBalls" (Raw.joystickNumBalls j)

-- | Identifies the state of the POV hat on a joystick.
data JoyHatPosition
  = HatCentered  -- ^ Centered position
  | HatUp        -- ^ Up position
  | HatRight     -- ^ Right position
  | HatDown      -- ^ Down position
  | HatLeft      -- ^ Left position
  | HatRightUp   -- ^ Right-up position
  | HatRightDown -- ^ Right-down position
  | HatLeftUp    -- ^ Left-up position
  | HatLeftDown  -- ^ Left-down position
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber JoyHatPosition Word8 where
  fromNumber n = case n of
    Raw.SDL_HAT_CENTERED -> HatCentered
    Raw.SDL_HAT_UP -> HatUp
    Raw.SDL_HAT_RIGHT -> HatRight
    Raw.SDL_HAT_DOWN -> HatDown
    Raw.SDL_HAT_LEFT -> HatLeft
    Raw.SDL_HAT_RIGHTUP -> HatRightUp
    Raw.SDL_HAT_RIGHTDOWN -> HatRightDown
    Raw.SDL_HAT_LEFTUP -> HatLeftUp
    Raw.SDL_HAT_LEFTDOWN -> HatLeftDown
    _ -> HatCentered

-- | Get current position of a POV hat on a joystick.
--
-- See @<https://wiki.libsdl.org/SDL_JoystickGetHat SDL_JoystickGetHat>@ for C documentation.
getHat :: (Functor m, MonadIO m)
       => Joystick
       -> CInt -- ^ The index of the POV hat. You can use 'numHats' to determine how many POV hats a given joystick has.
       -> m JoyHatPosition
getHat (Joystick j) hatIndex = fromNumber <$> Raw.joystickGetHat j hatIndex

-- | Get the number of POV hats on a joystick.
--
-- See @<https://wiki.libsdl.org/https://wiki.libsdl.org/SDL_JoystickNumHats SDL_JoystickNumHats>@ for C documentation.
numHats :: (MonadIO m) => Joystick -> m CInt
numHats (Joystick j) = liftIO $ throwIfNeg "SDL.Input.Joystick.numHats" "SDL_JoystickNumHats" (Raw.joystickNumHats j)

-- | Identifies whether a joystick has been connected or disconnected.
data JoyDeviceConnection = JoyDeviceAdded | JoyDeviceRemoved
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber JoyDeviceConnection Word32 where
  fromNumber n = case n of
    Raw.SDL_JOYDEVICEADDED -> JoyDeviceAdded
    Raw.SDL_JOYDEVICEREMOVED -> JoyDeviceRemoved
    _ -> JoyDeviceAdded
