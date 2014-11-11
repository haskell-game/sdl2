module SDL.Raw.Event (
  -- * Event Handling
  addEventWatch,
  delEventWatch,
  eventState,
  filterEvents,
  flushEvent,
  flushEvents,
  getEventFilter,
  getNumTouchDevices,
  getNumTouchFingers,
  getTouchDevice,
  getTouchFinger,
  hasEvent,
  hasEvents,
  loadDollarTemplates,
  peepEvents,
  pollEvent,
  pumpEvents,
  pushEvent,
  quitRequested,
  recordGesture,
  registerEvents,
  saveAllDollarTemplates,
  saveDollarTemplate,
  setEventFilter,
  waitEvent,
  waitEventTimeout,

  -- * Keyboard Support
  getKeyFromName,
  getKeyFromScancode,
  getKeyName,
  getKeyboardFocus,
  getKeyboardState,
  getModState,
  getScancodeFromKey,
  getScancodeFromName,
  getScancodeName,
  hasScreenKeyboardSupport,
  isScreenKeyboardShown,
  isTextInputActive,
  setModState,
  setTextInputRect,
  startTextInput,
  stopTextInput,

  -- * Mouse Support
  createColorCursor,
  createCursor,
  createSystemCursor,
  freeCursor,
  getCursor,
  getDefaultCursor,
  getMouseFocus,
  getMouseState,
  getRelativeMouseMode,
  getRelativeMouseState,
  setCursor,
  setRelativeMouseMode,
  showCursor,
  warpMouseInWindow,

  -- * Joystick Support
  joystickClose,
  joystickEventState,
  joystickGetAttached,
  joystickGetAxis,
  joystickGetBall,
  joystickGetButton,
  joystickGetDeviceGUID,
  joystickGetGUID,
  joystickGetGUIDFromString,
  joystickGetGUIDString,
  joystickGetHat,
  joystickInstanceID,
  joystickName,
  joystickNameForIndex,
  joystickNumAxes,
  joystickNumBalls,
  joystickNumButtons,
  joystickNumHats,
  joystickOpen,
  joystickUpdate,
  numJoysticks,

  -- * Game Controller Support
  gameControllerAddMapping,
  gameControllerAddMappingsFromFile,
  gameControllerAddMappingsFromRW,
  gameControllerClose,
  gameControllerEventState,
  gameControllerGetAttached,
  gameControllerGetAxis,
  gameControllerGetAxisFromString,
  gameControllerGetBindForAxis,
  gameControllerGetBindForButton,
  gameControllerGetButton,
  gameControllerGetButtonFromString,
  gameControllerGetJoystick,
  gameControllerGetStringForAxis,
  gameControllerGetStringForButton,
  gameControllerMapping,
  gameControllerMappingForGUID,
  gameControllerName,
  gameControllerNameForIndex,
  gameControllerOpen,
  gameControllerUpdate,
  isGameController
) where

import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import SDL.Raw.Enum
import SDL.Raw.Filesystem
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_AddEventWatch" addEventWatch' :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_DelEventWatch" delEventWatch' :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_EventState" eventState' :: Word32 -> CInt -> IO Word8
foreign import ccall "SDL.h SDL_FilterEvents" filterEvents' :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_FlushEvent" flushEvent' :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_FlushEvents" flushEvents' :: Word32 -> Word32 -> IO ()
foreign import ccall "SDL.h SDL_GetEventFilter" getEventFilter' :: Ptr EventFilter -> Ptr (Ptr ()) -> IO Bool
foreign import ccall "SDL.h SDL_GetNumTouchDevices" getNumTouchDevices' :: IO CInt
foreign import ccall "SDL.h SDL_GetNumTouchFingers" getNumTouchFingers' :: TouchID -> IO CInt
foreign import ccall "SDL.h SDL_GetTouchDevice" getTouchDevice' :: CInt -> IO TouchID
foreign import ccall "SDL.h SDL_GetTouchFinger" getTouchFinger' :: TouchID -> CInt -> IO (Ptr Finger)
foreign import ccall "SDL.h SDL_HasEvent" hasEvent' :: Word32 -> IO Bool
foreign import ccall "SDL.h SDL_HasEvents" hasEvents' :: Word32 -> Word32 -> IO Bool
foreign import ccall "SDL.h SDL_LoadDollarTemplates" loadDollarTemplates' :: TouchID -> Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_PeepEvents" peepEvents' :: Ptr Event -> CInt -> EventAction -> Word32 -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_PollEvent" pollEvent' :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_PumpEvents" pumpEvents' :: IO ()
foreign import ccall "SDL.h SDL_PushEvent" pushEvent' :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_RecordGesture" recordGesture' :: TouchID -> IO CInt
foreign import ccall "SDL.h SDL_RegisterEvents" registerEvents' :: CInt -> IO Word32
foreign import ccall "SDL.h SDL_SaveAllDollarTemplates" saveAllDollarTemplates' :: Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_SaveDollarTemplate" saveDollarTemplate' :: GestureID -> Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_SetEventFilter" setEventFilter' :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_WaitEvent" waitEvent' :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_WaitEventTimeout" waitEventTimeout' :: Ptr Event -> CInt -> IO CInt

foreign import ccall "SDL.h SDL_GetKeyFromName" getKeyFromName' :: CString -> IO Keycode
foreign import ccall "SDL.h SDL_GetKeyFromScancode" getKeyFromScancode' :: Scancode -> IO Keycode
foreign import ccall "SDL.h SDL_GetKeyName" getKeyName' :: Keycode -> IO CString
foreign import ccall "SDL.h SDL_GetKeyboardFocus" getKeyboardFocus' :: IO Window
foreign import ccall "SDL.h SDL_GetKeyboardState" getKeyboardState' :: Ptr CInt -> IO (Ptr Word8)
foreign import ccall "SDL.h SDL_GetModState" getModState' :: IO Keymod
foreign import ccall "SDL.h SDL_GetScancodeFromKey" getScancodeFromKey' :: Keycode -> IO Scancode
foreign import ccall "SDL.h SDL_GetScancodeFromName" getScancodeFromName' :: CString -> IO Scancode
foreign import ccall "SDL.h SDL_GetScancodeName" getScancodeName' :: Scancode -> IO CString
foreign import ccall "SDL.h SDL_HasScreenKeyboardSupport" hasScreenKeyboardSupport' :: IO Bool
foreign import ccall "SDL.h SDL_IsScreenKeyboardShown" isScreenKeyboardShown' :: Window -> IO Bool
foreign import ccall "SDL.h SDL_IsTextInputActive" isTextInputActive' :: IO Bool
foreign import ccall "SDL.h SDL_SetModState" setModState' :: Keymod -> IO ()
foreign import ccall "SDL.h SDL_SetTextInputRect" setTextInputRect' :: Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_StartTextInput" startTextInput' :: IO ()
foreign import ccall "SDL.h SDL_StopTextInput" stopTextInput' :: IO ()

foreign import ccall "SDL.h SDL_CreateColorCursor" createColorCursor' :: Ptr Surface -> CInt -> CInt -> IO Cursor
foreign import ccall "SDL.h SDL_CreateCursor" createCursor' :: Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO Cursor
foreign import ccall "SDL.h SDL_CreateSystemCursor" createSystemCursor' :: SystemCursor -> IO Cursor
foreign import ccall "SDL.h SDL_FreeCursor" freeCursor' :: Cursor -> IO ()
foreign import ccall "SDL.h SDL_GetCursor" getCursor' :: IO Cursor
foreign import ccall "SDL.h SDL_GetDefaultCursor" getDefaultCursor' :: IO Cursor
foreign import ccall "SDL.h SDL_GetMouseFocus" getMouseFocus' :: IO Window
foreign import ccall "SDL.h SDL_GetMouseState" getMouseState' :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_GetRelativeMouseMode" getRelativeMouseMode' :: IO Bool
foreign import ccall "SDL.h SDL_GetRelativeMouseState" getRelativeMouseState' :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_SetCursor" setCursor' :: Cursor -> IO ()
foreign import ccall "SDL.h SDL_SetRelativeMouseMode" setRelativeMouseMode' :: Bool -> IO CInt
foreign import ccall "SDL.h SDL_ShowCursor" showCursor' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_WarpMouseInWindow" warpMouseInWindow' :: Window -> CInt -> CInt -> IO ()

foreign import ccall "SDL.h SDL_JoystickClose" joystickClose' :: Joystick -> IO ()
foreign import ccall "SDL.h SDL_JoystickEventState" joystickEventState' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_JoystickGetAttached" joystickGetAttached' :: Joystick -> IO Bool
foreign import ccall "SDL.h SDL_JoystickGetAxis" joystickGetAxis' :: Joystick -> CInt -> IO Int16
foreign import ccall "SDL.h SDL_JoystickGetBall" joystickGetBall' :: Joystick -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_JoystickGetButton" joystickGetButton' :: Joystick -> CInt -> IO Word8
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetDeviceGUID" joystickGetDeviceGUID' :: CInt -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUID" joystickGetGUID' :: Joystick -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUIDFromString" joystickGetGUIDFromString' :: CString -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUIDString" joystickGetGUIDString' :: Ptr JoystickGUID -> CString -> CInt -> IO ()
foreign import ccall "SDL.h SDL_JoystickGetHat" joystickGetHat' :: Joystick -> CInt -> IO Word8
foreign import ccall "SDL.h SDL_JoystickInstanceID" joystickInstanceID' :: Joystick -> IO JoystickID
foreign import ccall "SDL.h SDL_JoystickName" joystickName' :: Joystick -> IO CString
foreign import ccall "SDL.h SDL_JoystickNameForIndex" joystickNameForIndex' :: CInt -> IO CString
foreign import ccall "SDL.h SDL_JoystickNumAxes" joystickNumAxes' :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumBalls" joystickNumBalls' :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumButtons" joystickNumButtons' :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumHats" joystickNumHats' :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickOpen" joystickOpen' :: CInt -> IO Joystick
foreign import ccall "SDL.h SDL_JoystickUpdate" joystickUpdate' :: IO ()
foreign import ccall "SDL.h SDL_NumJoysticks" numJoysticks' :: IO CInt

foreign import ccall "SDL.h SDL_GameControllerAddMapping" gameControllerAddMapping' :: CString -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerAddMappingsFromRW" gameControllerAddMappingsFromRW' :: Ptr RWops -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerClose" gameControllerClose' :: GameController -> IO ()
foreign import ccall "SDL.h SDL_GameControllerEventState" gameControllerEventState' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerGetAttached" gameControllerGetAttached' :: GameController -> IO Bool
foreign import ccall "SDL.h SDL_GameControllerGetAxis" gameControllerGetAxis' :: GameController -> GameControllerAxis -> IO Int16
foreign import ccall "SDL.h SDL_GameControllerGetAxisFromString" gameControllerGetAxisFromString' :: CString -> IO GameControllerAxis
foreign import ccall "sdlhelper.h SDLHelper_GameControllerGetBindForAxis" gameControllerGetBindForAxis' :: GameController -> GameControllerAxis -> Ptr GameControllerButtonBind -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_GameControllerGetBindForButton" gameControllerGetBindForButton' :: GameController -> GameControllerButton -> Ptr GameControllerButtonBind -> IO ()
foreign import ccall "SDL.h SDL_GameControllerGetButton" gameControllerGetButton' :: GameController -> GameControllerButton -> IO Word8
foreign import ccall "SDL.h SDL_GameControllerGetButtonFromString" gameControllerGetButtonFromString' :: CString -> IO GameControllerButton
foreign import ccall "SDL.h SDL_GameControllerGetJoystick" gameControllerGetJoystick' :: GameController -> IO Joystick
foreign import ccall "SDL.h SDL_GameControllerGetStringForAxis" gameControllerGetStringForAxis' :: GameControllerAxis -> IO CString
foreign import ccall "SDL.h SDL_GameControllerGetStringForButton" gameControllerGetStringForButton' :: GameControllerButton -> IO CString
foreign import ccall "SDL.h SDL_GameControllerMapping" gameControllerMapping' :: GameController -> IO CString
foreign import ccall "sdlhelper.h SDLHelper_GameControllerMappingForGUID" gameControllerMappingForGUID' :: Ptr JoystickGUID -> IO CString
foreign import ccall "SDL.h SDL_GameControllerName" gameControllerName' :: GameController -> IO CString
foreign import ccall "SDL.h SDL_GameControllerNameForIndex" gameControllerNameForIndex' :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GameControllerOpen" gameControllerOpen' :: CInt -> IO GameController
foreign import ccall "SDL.h SDL_GameControllerUpdate" gameControllerUpdate' :: IO ()
foreign import ccall "SDL.h SDL_IsGameController" isGameController' :: CInt -> IO Bool

addEventWatch :: MonadIO m => EventFilter -> Ptr () -> m ()
addEventWatch v1 v2 = liftIO $ addEventWatch' v1 v2
{-# INLINE addEventWatch #-}

delEventWatch :: MonadIO m => EventFilter -> Ptr () -> m ()
delEventWatch v1 v2 = liftIO $ delEventWatch' v1 v2
{-# INLINE delEventWatch #-}

eventState :: MonadIO m => Word32 -> CInt -> m Word8
eventState v1 v2 = liftIO $ eventState' v1 v2
{-# INLINE eventState #-}

filterEvents :: MonadIO m => EventFilter -> Ptr () -> m ()
filterEvents v1 v2 = liftIO $ filterEvents' v1 v2
{-# INLINE filterEvents #-}

flushEvent :: MonadIO m => Word32 -> m ()
flushEvent v1 = liftIO $ flushEvent' v1
{-# INLINE flushEvent #-}

flushEvents :: MonadIO m => Word32 -> Word32 -> m ()
flushEvents v1 v2 = liftIO $ flushEvents' v1 v2
{-# INLINE flushEvents #-}

getEventFilter :: MonadIO m => Ptr EventFilter -> Ptr (Ptr ()) -> m Bool
getEventFilter v1 v2 = liftIO $ getEventFilter' v1 v2
{-# INLINE getEventFilter #-}

getNumTouchDevices :: MonadIO m => m CInt
getNumTouchDevices = liftIO getNumTouchDevices'
{-# INLINE getNumTouchDevices #-}

getNumTouchFingers :: MonadIO m => TouchID -> m CInt
getNumTouchFingers v1 = liftIO $ getNumTouchFingers' v1
{-# INLINE getNumTouchFingers #-}

getTouchDevice :: MonadIO m => CInt -> m TouchID
getTouchDevice v1 = liftIO $ getTouchDevice' v1
{-# INLINE getTouchDevice #-}

getTouchFinger :: MonadIO m => TouchID -> CInt -> m (Ptr Finger)
getTouchFinger v1 v2 = liftIO $ getTouchFinger' v1 v2
{-# INLINE getTouchFinger #-}

hasEvent :: MonadIO m => Word32 -> m Bool
hasEvent v1 = liftIO $ hasEvent' v1
{-# INLINE hasEvent #-}

hasEvents :: MonadIO m => Word32 -> Word32 -> m Bool
hasEvents v1 v2 = liftIO $ hasEvents' v1 v2
{-# INLINE hasEvents #-}

loadDollarTemplates :: MonadIO m => TouchID -> Ptr RWops -> m CInt
loadDollarTemplates v1 v2 = liftIO $ loadDollarTemplates' v1 v2
{-# INLINE loadDollarTemplates #-}

peepEvents :: MonadIO m => Ptr Event -> CInt -> EventAction -> Word32 -> Word32 -> m CInt
peepEvents v1 v2 v3 v4 v5 = liftIO $ peepEvents' v1 v2 v3 v4 v5
{-# INLINE peepEvents #-}

pollEvent :: MonadIO m => Ptr Event -> m CInt
pollEvent v1 = liftIO $ pollEvent' v1
{-# INLINE pollEvent #-}

pumpEvents :: MonadIO m => m ()
pumpEvents = liftIO pumpEvents'
{-# INLINE pumpEvents #-}

pushEvent :: MonadIO m => Ptr Event -> m CInt
pushEvent v1 = liftIO $ pushEvent' v1
{-# INLINE pushEvent #-}

quitRequested :: MonadIO m => m Bool
quitRequested = liftIO $ do
  pumpEvents
  ev <- peepEvents nullPtr 0 SDL_PEEKEVENT SDL_QUIT SDL_QUIT
  return $ ev > 0
{-# INLINE quitRequested #-}

recordGesture :: MonadIO m => TouchID -> m CInt
recordGesture v1 = liftIO $ recordGesture' v1
{-# INLINE recordGesture #-}

registerEvents :: MonadIO m => CInt -> m Word32
registerEvents v1 = liftIO $ registerEvents' v1
{-# INLINE registerEvents #-}

saveAllDollarTemplates :: MonadIO m => Ptr RWops -> m CInt
saveAllDollarTemplates v1 = liftIO $ saveAllDollarTemplates' v1
{-# INLINE saveAllDollarTemplates #-}

saveDollarTemplate :: MonadIO m => GestureID -> Ptr RWops -> m CInt
saveDollarTemplate v1 v2 = liftIO $ saveDollarTemplate' v1 v2
{-# INLINE saveDollarTemplate #-}

setEventFilter :: MonadIO m => EventFilter -> Ptr () -> m ()
setEventFilter v1 v2 = liftIO $ setEventFilter' v1 v2
{-# INLINE setEventFilter #-}

waitEvent :: MonadIO m => Ptr Event -> m CInt
waitEvent v1 = liftIO $ waitEvent' v1
{-# INLINE waitEvent #-}

waitEventTimeout :: MonadIO m => Ptr Event -> CInt -> m CInt
waitEventTimeout v1 v2 = liftIO $ waitEventTimeout' v1 v2
{-# INLINE waitEventTimeout #-}

getKeyFromName :: MonadIO m => CString -> m Keycode
getKeyFromName v1 = liftIO $ getKeyFromName' v1
{-# INLINE getKeyFromName #-}

getKeyFromScancode :: MonadIO m => Scancode -> m Keycode
getKeyFromScancode v1 = liftIO $ getKeyFromScancode' v1
{-# INLINE getKeyFromScancode #-}

getKeyName :: MonadIO m => Keycode -> m CString
getKeyName v1 = liftIO $ getKeyName' v1
{-# INLINE getKeyName #-}

getKeyboardFocus :: MonadIO m => m Window
getKeyboardFocus = liftIO getKeyboardFocus'
{-# INLINE getKeyboardFocus #-}

getKeyboardState :: MonadIO m => Ptr CInt -> m (Ptr Word8)
getKeyboardState v1 = liftIO $ getKeyboardState' v1
{-# INLINE getKeyboardState #-}

getModState :: MonadIO m => m Keymod
getModState = liftIO getModState'
{-# INLINE getModState #-}

getScancodeFromKey :: MonadIO m => Keycode -> m Scancode
getScancodeFromKey v1 = liftIO $ getScancodeFromKey' v1
{-# INLINE getScancodeFromKey #-}

getScancodeFromName :: MonadIO m => CString -> m Scancode
getScancodeFromName v1 = liftIO $ getScancodeFromName' v1
{-# INLINE getScancodeFromName #-}

getScancodeName :: MonadIO m => Scancode -> m CString
getScancodeName v1 = liftIO $ getScancodeName' v1
{-# INLINE getScancodeName #-}

hasScreenKeyboardSupport :: MonadIO m => m Bool
hasScreenKeyboardSupport = liftIO hasScreenKeyboardSupport'
{-# INLINE hasScreenKeyboardSupport #-}

isScreenKeyboardShown :: MonadIO m => Window -> m Bool
isScreenKeyboardShown v1 = liftIO $ isScreenKeyboardShown' v1
{-# INLINE isScreenKeyboardShown #-}

isTextInputActive :: MonadIO m => m Bool
isTextInputActive = liftIO isTextInputActive'
{-# INLINE isTextInputActive #-}

setModState :: MonadIO m => Keymod -> m ()
setModState v1 = liftIO $ setModState' v1
{-# INLINE setModState #-}

setTextInputRect :: MonadIO m => Ptr Rect -> m ()
setTextInputRect v1 = liftIO $ setTextInputRect' v1
{-# INLINE setTextInputRect #-}

startTextInput :: MonadIO m => m ()
startTextInput = liftIO startTextInput'
{-# INLINE startTextInput #-}

stopTextInput :: MonadIO m => m ()
stopTextInput = liftIO stopTextInput'
{-# INLINE stopTextInput #-}

createColorCursor :: MonadIO m => Ptr Surface -> CInt -> CInt -> m Cursor
createColorCursor v1 v2 v3 = liftIO $ createColorCursor' v1 v2 v3
{-# INLINE createColorCursor #-}

createCursor :: MonadIO m => Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> m Cursor
createCursor v1 v2 v3 v4 v5 v6 = liftIO $ createCursor' v1 v2 v3 v4 v5 v6
{-# INLINE createCursor #-}

createSystemCursor :: MonadIO m => SystemCursor -> m Cursor
createSystemCursor v1 = liftIO $ createSystemCursor' v1
{-# INLINE createSystemCursor #-}

freeCursor :: MonadIO m => Cursor -> m ()
freeCursor v1 = liftIO $ freeCursor' v1
{-# INLINE freeCursor #-}

getCursor :: MonadIO m => m Cursor
getCursor = liftIO getCursor'
{-# INLINE getCursor #-}

getDefaultCursor :: MonadIO m => m Cursor
getDefaultCursor = liftIO getDefaultCursor'
{-# INLINE getDefaultCursor #-}

getMouseFocus :: MonadIO m => m Window
getMouseFocus = liftIO getMouseFocus'
{-# INLINE getMouseFocus #-}

getMouseState :: MonadIO m => Ptr CInt -> Ptr CInt -> m Word32
getMouseState v1 v2 = liftIO $ getMouseState' v1 v2
{-# INLINE getMouseState #-}

getRelativeMouseMode :: MonadIO m => m Bool
getRelativeMouseMode = liftIO getRelativeMouseMode'
{-# INLINE getRelativeMouseMode #-}

getRelativeMouseState :: MonadIO m => Ptr CInt -> Ptr CInt -> m Word32
getRelativeMouseState v1 v2 = liftIO $ getRelativeMouseState' v1 v2
{-# INLINE getRelativeMouseState #-}

setCursor :: MonadIO m => Cursor -> m ()
setCursor v1 = liftIO $ setCursor' v1
{-# INLINE setCursor #-}

setRelativeMouseMode :: MonadIO m => Bool -> m CInt
setRelativeMouseMode v1 = liftIO $ setRelativeMouseMode' v1
{-# INLINE setRelativeMouseMode #-}

showCursor :: MonadIO m => CInt -> m CInt
showCursor v1 = liftIO $ showCursor' v1
{-# INLINE showCursor #-}

warpMouseInWindow :: MonadIO m => Window -> CInt -> CInt -> m ()
warpMouseInWindow v1 v2 v3 = liftIO $ warpMouseInWindow' v1 v2 v3
{-# INLINE warpMouseInWindow #-}

joystickClose :: MonadIO m => Joystick -> m ()
joystickClose v1 = liftIO $ joystickClose' v1
{-# INLINE joystickClose #-}

joystickEventState :: MonadIO m => CInt -> m CInt
joystickEventState v1 = liftIO $ joystickEventState' v1
{-# INLINE joystickEventState #-}

joystickGetAttached :: MonadIO m => Joystick -> m Bool
joystickGetAttached v1 = liftIO $ joystickGetAttached' v1
{-# INLINE joystickGetAttached #-}

joystickGetAxis :: MonadIO m => Joystick -> CInt -> m Int16
joystickGetAxis v1 v2 = liftIO $ joystickGetAxis' v1 v2
{-# INLINE joystickGetAxis #-}

joystickGetBall :: MonadIO m => Joystick -> CInt -> Ptr CInt -> Ptr CInt -> m CInt
joystickGetBall v1 v2 v3 v4 = liftIO $ joystickGetBall' v1 v2 v3 v4
{-# INLINE joystickGetBall #-}

joystickGetButton :: MonadIO m => Joystick -> CInt -> m Word8
joystickGetButton v1 v2 = liftIO $ joystickGetButton' v1 v2
{-# INLINE joystickGetButton #-}

joystickGetDeviceGUID :: MonadIO m => CInt -> m JoystickGUID
joystickGetDeviceGUID device_index = liftIO . alloca $ \ptr -> do
  joystickGetDeviceGUID' device_index ptr
  peek ptr
{-# INLINE joystickGetDeviceGUID #-}

joystickGetGUID :: MonadIO m => Joystick -> m JoystickGUID
joystickGetGUID joystick = liftIO . alloca $ \ptr -> do
  joystickGetGUID' joystick ptr
  peek ptr
{-# INLINE joystickGetGUID #-}

joystickGetGUIDFromString :: MonadIO m => CString -> m JoystickGUID
joystickGetGUIDFromString pchGUID = liftIO . alloca $ \ptr -> do
  joystickGetGUIDFromString' pchGUID ptr
  peek ptr
{-# INLINE joystickGetGUIDFromString #-}

joystickGetGUIDString :: MonadIO m => JoystickGUID -> CString -> CInt -> m ()
joystickGetGUIDString guid pszGUID cbGUID = liftIO . alloca $ \ptr -> do
  poke ptr guid
  joystickGetGUIDString' ptr pszGUID cbGUID
{-# INLINE joystickGetGUIDString #-}

joystickGetHat :: MonadIO m => Joystick -> CInt -> m Word8
joystickGetHat v1 v2 = liftIO $ joystickGetHat' v1 v2
{-# INLINE joystickGetHat #-}

joystickInstanceID :: MonadIO m => Joystick -> m JoystickID
joystickInstanceID v1 = liftIO $ joystickInstanceID' v1
{-# INLINE joystickInstanceID #-}

joystickName :: MonadIO m => Joystick -> m CString
joystickName v1 = liftIO $ joystickName' v1
{-# INLINE joystickName #-}

joystickNameForIndex :: MonadIO m => CInt -> m CString
joystickNameForIndex v1 = liftIO $ joystickNameForIndex' v1
{-# INLINE joystickNameForIndex #-}

joystickNumAxes :: MonadIO m => Joystick -> m CInt
joystickNumAxes v1 = liftIO $ joystickNumAxes' v1
{-# INLINE joystickNumAxes #-}

joystickNumBalls :: MonadIO m => Joystick -> m CInt
joystickNumBalls v1 = liftIO $ joystickNumBalls' v1
{-# INLINE joystickNumBalls #-}

joystickNumButtons :: MonadIO m => Joystick -> m CInt
joystickNumButtons v1 = liftIO $ joystickNumButtons' v1
{-# INLINE joystickNumButtons #-}

joystickNumHats :: MonadIO m => Joystick -> m CInt
joystickNumHats v1 = liftIO $ joystickNumHats' v1
{-# INLINE joystickNumHats #-}

joystickOpen :: MonadIO m => CInt -> m Joystick
joystickOpen v1 = liftIO $ joystickOpen' v1
{-# INLINE joystickOpen #-}

joystickUpdate :: MonadIO m => m ()
joystickUpdate = liftIO joystickUpdate'
{-# INLINE joystickUpdate #-}

numJoysticks :: MonadIO m => m CInt
numJoysticks = liftIO numJoysticks'
{-# INLINE numJoysticks #-}

gameControllerAddMapping :: MonadIO m => CString -> m CInt
gameControllerAddMapping v1 = liftIO $ gameControllerAddMapping' v1
{-# INLINE gameControllerAddMapping #-}

gameControllerAddMappingsFromFile :: MonadIO m => CString -> m CInt
gameControllerAddMappingsFromFile file = liftIO $ do
  rw <- withCString "rb" $ rwFromFile file
  gameControllerAddMappingsFromRW rw 1
{-# INLINE gameControllerAddMappingsFromFile #-}

gameControllerAddMappingsFromRW :: MonadIO m => Ptr RWops -> CInt -> m CInt
gameControllerAddMappingsFromRW v1 v2 = liftIO $ gameControllerAddMappingsFromRW' v1 v2
{-# INLINE gameControllerAddMappingsFromRW #-}

gameControllerClose :: MonadIO m => GameController -> m ()
gameControllerClose v1 = liftIO $ gameControllerClose' v1
{-# INLINE gameControllerClose #-}

gameControllerEventState :: MonadIO m => CInt -> m CInt
gameControllerEventState v1 = liftIO $ gameControllerEventState' v1
{-# INLINE gameControllerEventState #-}

gameControllerGetAttached :: MonadIO m => GameController -> m Bool
gameControllerGetAttached v1 = liftIO $ gameControllerGetAttached' v1
{-# INLINE gameControllerGetAttached #-}

gameControllerGetAxis :: MonadIO m => GameController -> GameControllerAxis -> m Int16
gameControllerGetAxis v1 v2 = liftIO $ gameControllerGetAxis' v1 v2
{-# INLINE gameControllerGetAxis #-}

gameControllerGetAxisFromString :: MonadIO m => CString -> m GameControllerAxis
gameControllerGetAxisFromString v1 = liftIO $ gameControllerGetAxisFromString' v1
{-# INLINE gameControllerGetAxisFromString #-}

gameControllerGetBindForAxis :: MonadIO m => GameController -> GameControllerAxis -> m GameControllerButtonBind
gameControllerGetBindForAxis gamecontroller axis = liftIO . alloca $ \ptr -> do
  gameControllerGetBindForAxis' gamecontroller axis ptr
  peek ptr
{-# INLINE gameControllerGetBindForAxis #-}

gameControllerGetBindForButton :: MonadIO m => GameController -> GameControllerButton -> m GameControllerButtonBind
gameControllerGetBindForButton gamecontroller button = liftIO . alloca $ \ptr -> do
  gameControllerGetBindForButton' gamecontroller button ptr
  peek ptr
{-# INLINE gameControllerGetBindForButton #-}

gameControllerGetButton :: MonadIO m => GameController -> GameControllerButton -> m Word8
gameControllerGetButton v1 v2 = liftIO $ gameControllerGetButton' v1 v2
{-# INLINE gameControllerGetButton #-}

gameControllerGetButtonFromString :: MonadIO m => CString -> m GameControllerButton
gameControllerGetButtonFromString v1 = liftIO $ gameControllerGetButtonFromString' v1
{-# INLINE gameControllerGetButtonFromString #-}

gameControllerGetJoystick :: MonadIO m => GameController -> m Joystick
gameControllerGetJoystick v1 = liftIO $ gameControllerGetJoystick' v1
{-# INLINE gameControllerGetJoystick #-}

gameControllerGetStringForAxis :: MonadIO m => GameControllerAxis -> m CString
gameControllerGetStringForAxis v1 = liftIO $ gameControllerGetStringForAxis' v1
{-# INLINE gameControllerGetStringForAxis #-}

gameControllerGetStringForButton :: MonadIO m => GameControllerButton -> m CString
gameControllerGetStringForButton v1 = liftIO $ gameControllerGetStringForButton' v1
{-# INLINE gameControllerGetStringForButton #-}

gameControllerMapping :: MonadIO m => GameController -> m CString
gameControllerMapping v1 = liftIO $ gameControllerMapping' v1
{-# INLINE gameControllerMapping #-}

gameControllerMappingForGUID :: MonadIO m => JoystickGUID -> m CString
gameControllerMappingForGUID guid = liftIO . alloca $ \ptr -> do
  poke ptr guid
  gameControllerMappingForGUID' ptr
{-# INLINE gameControllerMappingForGUID #-}

gameControllerName :: MonadIO m => GameController -> m CString
gameControllerName v1 = liftIO $ gameControllerName' v1
{-# INLINE gameControllerName #-}

gameControllerNameForIndex :: MonadIO m => CInt -> m CString
gameControllerNameForIndex v1 = liftIO $ gameControllerNameForIndex' v1
{-# INLINE gameControllerNameForIndex #-}

gameControllerOpen :: MonadIO m => CInt -> m GameController
gameControllerOpen v1 = liftIO $ gameControllerOpen' v1
{-# INLINE gameControllerOpen #-}

gameControllerUpdate :: MonadIO m => m ()
gameControllerUpdate = liftIO gameControllerUpdate'
{-# INLINE gameControllerUpdate #-}

isGameController :: MonadIO m => CInt -> m Bool
isGameController v1 = liftIO $ isGameController' v1
{-# INLINE isGameController #-}
