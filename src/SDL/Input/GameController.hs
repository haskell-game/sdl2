{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Input.GameController
  ( ControllerDevice (..)
  , GameController
  , JoystickIndex
  , Raw.JoystickID

  , isGameController
  , mkControllerDevice
  , mkControllerDevice'
  , controllerFromInstanceID
  , availableControllers
  , openController
  , closeController
  , controllerAttached

  , getControllerID

  , controllerMapping
  , addControllerMapping
  , addControllerMappingsFromFile

  , ControllerButton (..)
  , ControllerButtonState (..)
  , controllerButton

  , ControllerAxis (..)
  , controllerAxis
  
  , ControllerDeviceConnection (..)
  ) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Data (Data)
import Data.Int
import Data.Text (Text)
import Data.Typeable
import Data.Word
import Foreign.C (withCString)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Int (Int32)
import SDL.Input.Joystick (numJoysticks)
import SDL.Internal.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types
import SDL.Vect
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified SDL.Raw as Raw
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

type JoystickIndex = CInt

{- | A description of game controller that can be opened using 'openController'.
 To retrieve a list of connected game controllers, use 'availableControllers'.
-}
data ControllerDevice = ControllerDevice
  { gameControllerDeviceName :: Text
  , gameControllerDeviceId :: JoystickIndex
  }
  deriving (Eq, Generic, Read, Ord, Show, Typeable)


{- | Check if the given joystick is supported by the game controller interface.

 See @<https://wiki.libsdl.org/SDL2/SDL_IsGameController SDL_IsGameController>@ for C documentation.
-}
isGameController :: MonadIO m => JoystickIndex -> m Bool
isGameController = Raw.isGameController

{- | Create a 'ControllerDevice' from a 'JoystickIndex'. Returns 'Nothing' if 
     the 'JoystickIndex' does not support the game controller interface.
-}
mkControllerDevice :: MonadIO m => JoystickIndex -> m (Maybe ControllerDevice)
mkControllerDevice i = runMaybeT $ do
  isGC <- isGameController i
  guard isGC
  mkControllerDevice' i

{- | Create a 'ControllerDevice' from a 'JoystickIndex'. Does not check whether
     the 'JoystickIndex' supports the game controller interface.
-}
mkControllerDevice' :: MonadIO m => JoystickIndex -> m ControllerDevice
mkControllerDevice' i = do
  cstr <- liftIO $
    throwIfNull "SDL.Input.GameController.mkControllerDevice'" "SDL_GameControllerNameForIndex" $
      Raw.gameControllerNameForIndex (fromIntegral i)
  name <- liftIO $ Text.decodeUtf8 <$> BS.packCString cstr
  return (ControllerDevice name i)

{- | Get the 'GameController' associated with a 'Raw.JoystickID'.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerFromInstanceID SDL_GameControllerFromInstanceID>@ for C documentation.
-}
controllerFromInstanceID :: MonadIO m => Raw.JoystickID -> m GameController
controllerFromInstanceID i =
  fmap GameController $
    throwIfNull "SDL.Input.GameController.controllerFromInstanceID" "SDL_GameControllerFromInstanceID" $
      Raw.gameControllerFromInstanceID (fromIntegral i)


-- | Enumerate all connected Controllers, retrieving a description of each.
availableControllers :: MonadIO m => m (V.Vector ControllerDevice)
availableControllers = liftIO $ do
  n <- fromIntegral <$> numJoysticks
  V.catMaybes <$> V.generateM n (mkControllerDevice . fromIntegral)

{- | Open a controller so that you can start receiving events from interaction with this controller.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerOpen SDL_GameControllerOpen>@ for C documentation.
-}
openController
  :: (Functor m, MonadIO m)
  => ControllerDevice
  -- ^ The device to open. Use 'availableControllers' to find 'JoystickDevices's
  -> m GameController
openController (ControllerDevice _ i) =
  fmap GameController $
    throwIfNull "SDL.Input.GameController.openController" "SDL_GameControllerOpen" $
      Raw.gameControllerOpen (fromIntegral i)

{- | Close a controller previously opened with 'openController'.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerClose SDL_GameControllerClose>@ for C documentation.
-}
closeController :: MonadIO m => GameController -> m ()
closeController (GameController j) = Raw.gameControllerClose j

{- | Check if a controller has been opened and is currently connected.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerGetAttached SDL_GameControllerGetAttached>@ for C documentation.
-}
controllerAttached :: MonadIO m => GameController -> m Bool
controllerAttached (GameController c) = Raw.gameControllerGetAttached c

{- | Get the instance ID of an opened controller. The instance ID is used to identify the controller
 in future SDL events.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerInstanceID SDL_GameControllerInstanceID>@ for C documentation.
-}
getControllerID :: MonadIO m => GameController -> m Raw.JoystickID
getControllerID (GameController c) =
  throwIfNeg "SDL.Input.GameController.getControllerID" "SDL_JoystickInstanceID" $
    Raw.joystickInstanceID c

{- | Get the current mapping of a Game Controller.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerMapping SDL_GameControllerMapping>@ for C documentation.
-}
controllerMapping :: MonadIO m => GameController -> m Text
controllerMapping (GameController c) = liftIO $ do
  mapping <-
    throwIfNull "SDL.Input.GameController.getControllerMapping" "SDL_GameControllerMapping" $
      Raw.gameControllerMapping c
  Text.decodeUtf8 <$> BS.packCString mapping

{- | Add support for controllers that SDL is unaware of or to cause an existing controller to
 have a different binding.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerAddMapping SDL_GameControllerAddMapping>@ for C documentation.
-}
addControllerMapping :: MonadIO m => BS.ByteString -> m ()
addControllerMapping mapping =
  liftIO $
    throwIfNeg_ "SDL.Input.GameController.addControllerMapping" "SDL_GameControllerAddMapping" $
      let (mappingForeign, _, _) = BSI.toForeignPtr mapping
       in withForeignPtr mappingForeign $ \mappingPtr ->
            Raw.gameControllerAddMapping (castPtr mappingPtr)

{- | Use this function to load a set of Game Controller mappings from a file, filtered by the
 current SDL_GetPlatform(). A community sourced database of controllers is available
 @<https://raw.githubusercontent.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt here>@
 (on GitHub).

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerAddMappingsFromFile SDL_GameControllerAddMappingsFromFile>@ for C documentation.
-}
addControllerMappingsFromFile :: MonadIO m => FilePath -> m ()
addControllerMappingsFromFile mappingFile =
  liftIO $
    throwIfNeg_ "SDL.Input.GameController.addControllerMappingsFromFile" "SDL_GameControllerAddMappingsFromFile" $
      withCString mappingFile Raw.gameControllerAddMappingsFromFile

{- | Get the current state of an axis control on a game controller.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerGetAxis SDL_GameControllerGetAxis>@ for C documentation.
-}
controllerAxis :: MonadIO m => GameController -> ControllerAxis -> m Int16
controllerAxis (GameController c) axis =
  Raw.gameControllerGetAxis c (toNumber axis)

{- | Get the current state of a button on a game controller.

 See @<https://wiki.libsdl.org/SDL2/SDL_GameControllerGetButton SDL_GameControllerGetButton>@ for C documentation.
-}
controllerButton :: MonadIO m => GameController -> ControllerButton -> m ControllerButtonState
controllerButton (GameController c) button =
  fromNumber . fromIntegral <$> Raw.gameControllerGetButton c (toNumber button)

-- | Identifies a gamepad button.
data ControllerButton
  = ControllerButtonInvalid
  | ControllerButtonA
  | ControllerButtonB
  | ControllerButtonX
  | ControllerButtonY
  | ControllerButtonBack
  | ControllerButtonGuide
  | ControllerButtonStart
  | ControllerButtonLeftStick
  | ControllerButtonRightStick
  | ControllerButtonLeftShoulder
  | ControllerButtonRightShoulder
  | ControllerButtonDpadUp
  | ControllerButtonDpadDown
  | ControllerButtonDpadLeft
  | ControllerButtonDpadRight
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButton Int32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLER_BUTTON_A -> ControllerButtonA
    Raw.SDL_CONTROLLER_BUTTON_B -> ControllerButtonB
    Raw.SDL_CONTROLLER_BUTTON_X -> ControllerButtonX
    Raw.SDL_CONTROLLER_BUTTON_Y -> ControllerButtonY
    Raw.SDL_CONTROLLER_BUTTON_BACK -> ControllerButtonBack
    Raw.SDL_CONTROLLER_BUTTON_GUIDE -> ControllerButtonGuide
    Raw.SDL_CONTROLLER_BUTTON_START -> ControllerButtonStart
    Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK -> ControllerButtonLeftStick
    Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK -> ControllerButtonRightStick
    Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER -> ControllerButtonLeftShoulder
    Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> ControllerButtonRightShoulder
    Raw.SDL_CONTROLLER_BUTTON_DPAD_UP -> ControllerButtonDpadUp
    Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN -> ControllerButtonDpadDown
    Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT -> ControllerButtonDpadLeft
    Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT -> ControllerButtonDpadRight
    _ -> ControllerButtonInvalid

instance ToNumber ControllerButton Int32 where
  toNumber c = case c of
    ControllerButtonA -> Raw.SDL_CONTROLLER_BUTTON_A
    ControllerButtonB -> Raw.SDL_CONTROLLER_BUTTON_B
    ControllerButtonX -> Raw.SDL_CONTROLLER_BUTTON_X
    ControllerButtonY -> Raw.SDL_CONTROLLER_BUTTON_Y
    ControllerButtonBack -> Raw.SDL_CONTROLLER_BUTTON_BACK
    ControllerButtonGuide -> Raw.SDL_CONTROLLER_BUTTON_GUIDE
    ControllerButtonStart -> Raw.SDL_CONTROLLER_BUTTON_START
    ControllerButtonLeftStick -> Raw.SDL_CONTROLLER_BUTTON_LEFTSTICK
    ControllerButtonRightStick -> Raw.SDL_CONTROLLER_BUTTON_RIGHTSTICK
    ControllerButtonLeftShoulder -> Raw.SDL_CONTROLLER_BUTTON_LEFTSHOULDER
    ControllerButtonRightShoulder -> Raw.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER
    ControllerButtonDpadUp -> Raw.SDL_CONTROLLER_BUTTON_DPAD_UP
    ControllerButtonDpadDown -> Raw.SDL_CONTROLLER_BUTTON_DPAD_DOWN
    ControllerButtonDpadLeft -> Raw.SDL_CONTROLLER_BUTTON_DPAD_LEFT
    ControllerButtonDpadRight -> Raw.SDL_CONTROLLER_BUTTON_DPAD_RIGHT
    ControllerButtonInvalid -> Raw.SDL_CONTROLLER_BUTTON_INVALID

-- | Identifies the state of a controller button.
data ControllerButtonState
  = ControllerButtonPressed
  | ControllerButtonReleased
  | ControllerButtonInvalidState
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerButtonState Word32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLERBUTTONDOWN -> ControllerButtonPressed
    Raw.SDL_CONTROLLERBUTTONUP -> ControllerButtonReleased
    _ -> ControllerButtonInvalidState

data ControllerAxis
  = ControllerAxisInvalid
  | ControllerAxisLeftX
  | ControllerAxisLeftY
  | ControllerAxisRightX
  | ControllerAxisRightY
  | ControllerAxisTriggerLeft
  | ControllerAxisTriggerRight
  | ControllerAxisMax
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance ToNumber ControllerAxis Int32 where
  toNumber a = case a of
    ControllerAxisLeftX -> Raw.SDL_CONTROLLER_AXIS_LEFTX
    ControllerAxisLeftY -> Raw.SDL_CONTROLLER_AXIS_LEFTY
    ControllerAxisRightX -> Raw.SDL_CONTROLLER_AXIS_RIGHTX
    ControllerAxisRightY -> Raw.SDL_CONTROLLER_AXIS_RIGHTY
    ControllerAxisTriggerLeft -> Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT
    ControllerAxisTriggerRight -> Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT
    ControllerAxisMax -> Raw.SDL_CONTROLLER_AXIS_MAX
    ControllerAxisInvalid -> Raw.SDL_CONTROLLER_AXIS_INVALID

instance FromNumber ControllerAxis Int32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLER_AXIS_LEFTX -> ControllerAxisLeftX
    Raw.SDL_CONTROLLER_AXIS_LEFTY -> ControllerAxisLeftY
    Raw.SDL_CONTROLLER_AXIS_RIGHTX -> ControllerAxisRightX
    Raw.SDL_CONTROLLER_AXIS_RIGHTY -> ControllerAxisRightY
    Raw.SDL_CONTROLLER_AXIS_TRIGGERLEFT -> ControllerAxisTriggerLeft
    Raw.SDL_CONTROLLER_AXIS_TRIGGERRIGHT -> ControllerAxisTriggerRight
    Raw.SDL_CONTROLLER_AXIS_MAX -> ControllerAxisMax
    _ -> ControllerAxisInvalid

-- | Identifies whether the game controller was added, removed, or remapped.
data ControllerDeviceConnection
  = ControllerDeviceAdded
  | ControllerDeviceRemoved
  | ControllerDeviceRemapped
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber ControllerDeviceConnection Word32 where
  fromNumber n = case n of
    Raw.SDL_CONTROLLERDEVICEADDED -> ControllerDeviceAdded
    Raw.SDL_CONTROLLERDEVICEREMOVED -> ControllerDeviceRemoved
    _ -> ControllerDeviceRemapped
