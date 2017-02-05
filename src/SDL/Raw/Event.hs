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
  captureMouse,
  createColorCursor,
  createCursor,
  createSystemCursor,
  freeCursor,
  getCursor,
  getDefaultCursor,
  getGlobalMouseState,
  getMouseFocus,
  getMouseState,
  getRelativeMouseMode,
  getRelativeMouseState,
  setCursor,
  setRelativeMouseMode,
  showCursor,
  warpMouseGlobal,
  warpMouseInWindow,

  -- * Joystick Support
  joystickClose,
  joystickCurrentPowerLevel,
  joystickEventState,
  joystickFromInstanceID,
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
  gameControllerFromInstanceID,
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

foreign import ccall "SDL.h SDL_AddEventWatch" addEventWatchFFI :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_DelEventWatch" delEventWatchFFI :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_EventState" eventStateFFI :: Word32 -> CInt -> IO Word8
foreign import ccall "SDL.h SDL_FilterEvents" filterEventsFFI :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_FlushEvent" flushEventFFI :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_FlushEvents" flushEventsFFI :: Word32 -> Word32 -> IO ()
foreign import ccall "SDL.h SDL_GetEventFilter" getEventFilterFFI :: Ptr EventFilter -> Ptr (Ptr ()) -> IO Bool
foreign import ccall "SDL.h SDL_GetNumTouchDevices" getNumTouchDevicesFFI :: IO CInt
foreign import ccall "SDL.h SDL_GetNumTouchFingers" getNumTouchFingersFFI :: TouchID -> IO CInt
foreign import ccall "SDL.h SDL_GetTouchDevice" getTouchDeviceFFI :: CInt -> IO TouchID
foreign import ccall "SDL.h SDL_GetTouchFinger" getTouchFingerFFI :: TouchID -> CInt -> IO (Ptr Finger)
foreign import ccall "SDL.h SDL_HasEvent" hasEventFFI :: Word32 -> IO Bool
foreign import ccall "SDL.h SDL_HasEvents" hasEventsFFI :: Word32 -> Word32 -> IO Bool
foreign import ccall "SDL.h SDL_LoadDollarTemplates" loadDollarTemplatesFFI :: TouchID -> Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_PeepEvents" peepEventsFFI :: Ptr Event -> CInt -> EventAction -> Word32 -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_PollEvent" pollEventFFI :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_PumpEvents" pumpEventsFFI :: IO ()
foreign import ccall "SDL.h SDL_PushEvent" pushEventFFI :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_RecordGesture" recordGestureFFI :: TouchID -> IO CInt
foreign import ccall "SDL.h SDL_RegisterEvents" registerEventsFFI :: CInt -> IO Word32
foreign import ccall "SDL.h SDL_SaveAllDollarTemplates" saveAllDollarTemplatesFFI :: Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_SaveDollarTemplate" saveDollarTemplateFFI :: GestureID -> Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_SetEventFilter" setEventFilterFFI :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_WaitEvent" waitEventFFI :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_WaitEventTimeout" waitEventTimeoutFFI :: Ptr Event -> CInt -> IO CInt

foreign import ccall "SDL.h SDL_GetKeyFromName" getKeyFromNameFFI :: CString -> IO Keycode
foreign import ccall "SDL.h SDL_GetKeyFromScancode" getKeyFromScancodeFFI :: Scancode -> IO Keycode
foreign import ccall "SDL.h SDL_GetKeyName" getKeyNameFFI :: Keycode -> IO CString
foreign import ccall "SDL.h SDL_GetKeyboardFocus" getKeyboardFocusFFI :: IO Window
foreign import ccall "SDL.h SDL_GetKeyboardState" getKeyboardStateFFI :: Ptr CInt -> IO (Ptr Word8)
foreign import ccall "SDL.h SDL_GetModState" getModStateFFI :: IO Keymod
foreign import ccall "SDL.h SDL_GetScancodeFromKey" getScancodeFromKeyFFI :: Keycode -> IO Scancode
foreign import ccall "SDL.h SDL_GetScancodeFromName" getScancodeFromNameFFI :: CString -> IO Scancode
foreign import ccall "SDL.h SDL_GetScancodeName" getScancodeNameFFI :: Scancode -> IO CString
foreign import ccall "SDL.h SDL_HasScreenKeyboardSupport" hasScreenKeyboardSupportFFI :: IO Bool
foreign import ccall "SDL.h SDL_IsScreenKeyboardShown" isScreenKeyboardShownFFI :: Window -> IO Bool
foreign import ccall "SDL.h SDL_IsTextInputActive" isTextInputActiveFFI :: IO Bool
foreign import ccall "SDL.h SDL_SetModState" setModStateFFI :: Keymod -> IO ()
foreign import ccall "SDL.h SDL_SetTextInputRect" setTextInputRectFFI :: Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_StartTextInput" startTextInputFFI :: IO ()
foreign import ccall "SDL.h SDL_StopTextInput" stopTextInputFFI :: IO ()

foreign import ccall "SDL.h SDL_CaptureMouse" captureMouseFFI :: Bool -> IO CInt
foreign import ccall "SDL.h SDL_CreateColorCursor" createColorCursorFFI :: Ptr Surface -> CInt -> CInt -> IO Cursor
foreign import ccall "SDL.h SDL_CreateCursor" createCursorFFI :: Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO Cursor
foreign import ccall "SDL.h SDL_CreateSystemCursor" createSystemCursorFFI :: SystemCursor -> IO Cursor
foreign import ccall "SDL.h SDL_FreeCursor" freeCursorFFI :: Cursor -> IO ()
foreign import ccall "SDL.h SDL_GetCursor" getCursorFFI :: IO Cursor
foreign import ccall "SDL.h SDL_GetDefaultCursor" getDefaultCursorFFI :: IO Cursor
foreign import ccall "SDL.h SDL_GetGlobalMouseState" getGlobalMouseStateFFI :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_GetMouseFocus" getMouseFocusFFI :: IO Window
foreign import ccall "SDL.h SDL_GetMouseState" getMouseStateFFI :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_GetRelativeMouseMode" getRelativeMouseModeFFI :: IO Bool
foreign import ccall "SDL.h SDL_GetRelativeMouseState" getRelativeMouseStateFFI :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_SetCursor" setCursorFFI :: Cursor -> IO ()
foreign import ccall "SDL.h SDL_SetRelativeMouseMode" setRelativeMouseModeFFI :: Bool -> IO CInt
foreign import ccall "SDL.h SDL_ShowCursor" showCursorFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_WarpMouseGlobal" warpMouseGlobalFFI :: CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_WarpMouseInWindow" warpMouseInWindowFFI :: Window -> CInt -> CInt -> IO ()

foreign import ccall "SDL.h SDL_JoystickClose" joystickCloseFFI :: Joystick -> IO ()
foreign import ccall "SDL.h SDL_JoystickCurrentPowerLevel" joystickCurrentPowerLevelFFI :: Joystick -> IO JoystickPowerLevel
foreign import ccall "SDL.h SDL_JoystickEventState" joystickEventStateFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_JoystickFromInstanceID" joystickFromInstanceIDFFI :: JoystickID -> IO Joystick
foreign import ccall "SDL.h SDL_JoystickGetAttached" joystickGetAttachedFFI :: Joystick -> IO Bool
foreign import ccall "SDL.h SDL_JoystickGetAxis" joystickGetAxisFFI :: Joystick -> CInt -> IO Int16
foreign import ccall "SDL.h SDL_JoystickGetBall" joystickGetBallFFI :: Joystick -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_JoystickGetButton" joystickGetButtonFFI :: Joystick -> CInt -> IO Word8
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetDeviceGUID" joystickGetDeviceGUIDFFI :: CInt -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUID" joystickGetGUIDFFI :: Joystick -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUIDFromString" joystickGetGUIDFromStringFFI :: CString -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUIDString" joystickGetGUIDStringFFI :: Ptr JoystickGUID -> CString -> CInt -> IO ()
foreign import ccall "SDL.h SDL_JoystickGetHat" joystickGetHatFFI :: Joystick -> CInt -> IO Word8
foreign import ccall "SDL.h SDL_JoystickInstanceID" joystickInstanceIDFFI :: Joystick -> IO JoystickID
foreign import ccall "SDL.h SDL_JoystickName" joystickNameFFI :: Joystick -> IO CString
foreign import ccall "SDL.h SDL_JoystickNameForIndex" joystickNameForIndexFFI :: CInt -> IO CString
foreign import ccall "SDL.h SDL_JoystickNumAxes" joystickNumAxesFFI :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumBalls" joystickNumBallsFFI :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumButtons" joystickNumButtonsFFI :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumHats" joystickNumHatsFFI :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickOpen" joystickOpenFFI :: CInt -> IO Joystick
foreign import ccall "SDL.h SDL_JoystickUpdate" joystickUpdateFFI :: IO ()
foreign import ccall "SDL.h SDL_NumJoysticks" numJoysticksFFI :: IO CInt

foreign import ccall "SDL.h SDL_GameControllerAddMapping" gameControllerAddMappingFFI :: CString -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerAddMappingsFromRW" gameControllerAddMappingsFromRWFFI :: Ptr RWops -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerClose" gameControllerCloseFFI :: GameController -> IO ()
foreign import ccall "SDL.h SDL_GameControllerEventState" gameControllerEventStateFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerFromInstanceID" gameControllerFromInstanceIDFFI :: JoystickID -> IO GameController
foreign import ccall "SDL.h SDL_GameControllerGetAttached" gameControllerGetAttachedFFI :: GameController -> IO Bool
foreign import ccall "SDL.h SDL_GameControllerGetAxis" gameControllerGetAxisFFI :: GameController -> GameControllerAxis -> IO Int16
foreign import ccall "SDL.h SDL_GameControllerGetAxisFromString" gameControllerGetAxisFromStringFFI :: CString -> IO GameControllerAxis
foreign import ccall "sdlhelper.h SDLHelper_GameControllerGetBindForAxis" gameControllerGetBindForAxisFFI :: GameController -> GameControllerAxis -> Ptr GameControllerButtonBind -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_GameControllerGetBindForButton" gameControllerGetBindForButtonFFI :: GameController -> GameControllerButton -> Ptr GameControllerButtonBind -> IO ()
foreign import ccall "SDL.h SDL_GameControllerGetButton" gameControllerGetButtonFFI :: GameController -> GameControllerButton -> IO Word8
foreign import ccall "SDL.h SDL_GameControllerGetButtonFromString" gameControllerGetButtonFromStringFFI :: CString -> IO GameControllerButton
foreign import ccall "SDL.h SDL_GameControllerGetJoystick" gameControllerGetJoystickFFI :: GameController -> IO Joystick
foreign import ccall "SDL.h SDL_GameControllerGetStringForAxis" gameControllerGetStringForAxisFFI :: GameControllerAxis -> IO CString
foreign import ccall "SDL.h SDL_GameControllerGetStringForButton" gameControllerGetStringForButtonFFI :: GameControllerButton -> IO CString
foreign import ccall "SDL.h SDL_GameControllerMapping" gameControllerMappingFFI :: GameController -> IO CString
foreign import ccall "sdlhelper.h SDLHelper_GameControllerMappingForGUID" gameControllerMappingForGUIDFFI :: Ptr JoystickGUID -> IO CString
foreign import ccall "SDL.h SDL_GameControllerName" gameControllerNameFFI :: GameController -> IO CString
foreign import ccall "SDL.h SDL_GameControllerNameForIndex" gameControllerNameForIndexFFI :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GameControllerOpen" gameControllerOpenFFI :: CInt -> IO GameController
foreign import ccall "SDL.h SDL_GameControllerUpdate" gameControllerUpdateFFI :: IO ()
foreign import ccall "SDL.h SDL_IsGameController" isGameControllerFFI :: CInt -> IO Bool

addEventWatch :: MonadIO m => EventFilter -> Ptr () -> m ()
addEventWatch v1 v2 = liftIO $ addEventWatchFFI v1 v2
{-# INLINE addEventWatch #-}

delEventWatch :: MonadIO m => EventFilter -> Ptr () -> m ()
delEventWatch v1 v2 = liftIO $ delEventWatchFFI v1 v2
{-# INLINE delEventWatch #-}

eventState :: MonadIO m => Word32 -> CInt -> m Word8
eventState v1 v2 = liftIO $ eventStateFFI v1 v2
{-# INLINE eventState #-}

filterEvents :: MonadIO m => EventFilter -> Ptr () -> m ()
filterEvents v1 v2 = liftIO $ filterEventsFFI v1 v2
{-# INLINE filterEvents #-}

flushEvent :: MonadIO m => Word32 -> m ()
flushEvent v1 = liftIO $ flushEventFFI v1
{-# INLINE flushEvent #-}

flushEvents :: MonadIO m => Word32 -> Word32 -> m ()
flushEvents v1 v2 = liftIO $ flushEventsFFI v1 v2
{-# INLINE flushEvents #-}

getEventFilter :: MonadIO m => Ptr EventFilter -> Ptr (Ptr ()) -> m Bool
getEventFilter v1 v2 = liftIO $ getEventFilterFFI v1 v2
{-# INLINE getEventFilter #-}

getNumTouchDevices :: MonadIO m => m CInt
getNumTouchDevices = liftIO getNumTouchDevicesFFI
{-# INLINE getNumTouchDevices #-}

getNumTouchFingers :: MonadIO m => TouchID -> m CInt
getNumTouchFingers v1 = liftIO $ getNumTouchFingersFFI v1
{-# INLINE getNumTouchFingers #-}

getTouchDevice :: MonadIO m => CInt -> m TouchID
getTouchDevice v1 = liftIO $ getTouchDeviceFFI v1
{-# INLINE getTouchDevice #-}

getTouchFinger :: MonadIO m => TouchID -> CInt -> m (Ptr Finger)
getTouchFinger v1 v2 = liftIO $ getTouchFingerFFI v1 v2
{-# INLINE getTouchFinger #-}

hasEvent :: MonadIO m => Word32 -> m Bool
hasEvent v1 = liftIO $ hasEventFFI v1
{-# INLINE hasEvent #-}

hasEvents :: MonadIO m => Word32 -> Word32 -> m Bool
hasEvents v1 v2 = liftIO $ hasEventsFFI v1 v2
{-# INLINE hasEvents #-}

loadDollarTemplates :: MonadIO m => TouchID -> Ptr RWops -> m CInt
loadDollarTemplates v1 v2 = liftIO $ loadDollarTemplatesFFI v1 v2
{-# INLINE loadDollarTemplates #-}

peepEvents :: MonadIO m => Ptr Event -> CInt -> EventAction -> Word32 -> Word32 -> m CInt
peepEvents v1 v2 v3 v4 v5 = liftIO $ peepEventsFFI v1 v2 v3 v4 v5
{-# INLINE peepEvents #-}

pollEvent :: MonadIO m => Ptr Event -> m CInt
pollEvent v1 = liftIO $ pollEventFFI v1
{-# INLINE pollEvent #-}

pumpEvents :: MonadIO m => m ()
pumpEvents = liftIO pumpEventsFFI
{-# INLINE pumpEvents #-}

pushEvent :: MonadIO m => Ptr Event -> m CInt
pushEvent v1 = liftIO $ pushEventFFI v1
{-# INLINE pushEvent #-}

quitRequested :: MonadIO m => m Bool
quitRequested = liftIO $ do
  pumpEvents
  ev <- peepEvents nullPtr 0 SDL_PEEKEVENT SDL_QUIT SDL_QUIT
  return $ ev > 0
{-# INLINE quitRequested #-}

recordGesture :: MonadIO m => TouchID -> m CInt
recordGesture v1 = liftIO $ recordGestureFFI v1
{-# INLINE recordGesture #-}

registerEvents :: MonadIO m => CInt -> m Word32
registerEvents v1 = liftIO $ registerEventsFFI v1
{-# INLINE registerEvents #-}

saveAllDollarTemplates :: MonadIO m => Ptr RWops -> m CInt
saveAllDollarTemplates v1 = liftIO $ saveAllDollarTemplatesFFI v1
{-# INLINE saveAllDollarTemplates #-}

saveDollarTemplate :: MonadIO m => GestureID -> Ptr RWops -> m CInt
saveDollarTemplate v1 v2 = liftIO $ saveDollarTemplateFFI v1 v2
{-# INLINE saveDollarTemplate #-}

setEventFilter :: MonadIO m => EventFilter -> Ptr () -> m ()
setEventFilter v1 v2 = liftIO $ setEventFilterFFI v1 v2
{-# INLINE setEventFilter #-}

waitEvent :: MonadIO m => Ptr Event -> m CInt
waitEvent v1 = liftIO $ waitEventFFI v1
{-# INLINE waitEvent #-}

waitEventTimeout :: MonadIO m => Ptr Event -> CInt -> m CInt
waitEventTimeout v1 v2 = liftIO $ waitEventTimeoutFFI v1 v2
{-# INLINE waitEventTimeout #-}

getKeyFromName :: MonadIO m => CString -> m Keycode
getKeyFromName v1 = liftIO $ getKeyFromNameFFI v1
{-# INLINE getKeyFromName #-}

getKeyFromScancode :: MonadIO m => Scancode -> m Keycode
getKeyFromScancode v1 = liftIO $ getKeyFromScancodeFFI v1
{-# INLINE getKeyFromScancode #-}

getKeyName :: MonadIO m => Keycode -> m CString
getKeyName v1 = liftIO $ getKeyNameFFI v1
{-# INLINE getKeyName #-}

getKeyboardFocus :: MonadIO m => m Window
getKeyboardFocus = liftIO getKeyboardFocusFFI
{-# INLINE getKeyboardFocus #-}

getKeyboardState :: MonadIO m => Ptr CInt -> m (Ptr Word8)
getKeyboardState v1 = liftIO $ getKeyboardStateFFI v1
{-# INLINE getKeyboardState #-}

getModState :: MonadIO m => m Keymod
getModState = liftIO getModStateFFI
{-# INLINE getModState #-}

getScancodeFromKey :: MonadIO m => Keycode -> m Scancode
getScancodeFromKey v1 = liftIO $ getScancodeFromKeyFFI v1
{-# INLINE getScancodeFromKey #-}

getScancodeFromName :: MonadIO m => CString -> m Scancode
getScancodeFromName v1 = liftIO $ getScancodeFromNameFFI v1
{-# INLINE getScancodeFromName #-}

getScancodeName :: MonadIO m => Scancode -> m CString
getScancodeName v1 = liftIO $ getScancodeNameFFI v1
{-# INLINE getScancodeName #-}

hasScreenKeyboardSupport :: MonadIO m => m Bool
hasScreenKeyboardSupport = liftIO hasScreenKeyboardSupportFFI
{-# INLINE hasScreenKeyboardSupport #-}

isScreenKeyboardShown :: MonadIO m => Window -> m Bool
isScreenKeyboardShown v1 = liftIO $ isScreenKeyboardShownFFI v1
{-# INLINE isScreenKeyboardShown #-}

isTextInputActive :: MonadIO m => m Bool
isTextInputActive = liftIO isTextInputActiveFFI
{-# INLINE isTextInputActive #-}

setModState :: MonadIO m => Keymod -> m ()
setModState v1 = liftIO $ setModStateFFI v1
{-# INLINE setModState #-}

setTextInputRect :: MonadIO m => Ptr Rect -> m ()
setTextInputRect v1 = liftIO $ setTextInputRectFFI v1
{-# INLINE setTextInputRect #-}

startTextInput :: MonadIO m => m ()
startTextInput = liftIO startTextInputFFI
{-# INLINE startTextInput #-}

stopTextInput :: MonadIO m => m ()
stopTextInput = liftIO stopTextInputFFI
{-# INLINE stopTextInput #-}

captureMouse :: MonadIO m => Bool -> m CInt
captureMouse v1 = liftIO $ captureMouseFFI v1
{-# INLINE captureMouse #-}

createColorCursor :: MonadIO m => Ptr Surface -> CInt -> CInt -> m Cursor
createColorCursor v1 v2 v3 = liftIO $ createColorCursorFFI v1 v2 v3
{-# INLINE createColorCursor #-}

createCursor :: MonadIO m => Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> m Cursor
createCursor v1 v2 v3 v4 v5 v6 = liftIO $ createCursorFFI v1 v2 v3 v4 v5 v6
{-# INLINE createCursor #-}

createSystemCursor :: MonadIO m => SystemCursor -> m Cursor
createSystemCursor v1 = liftIO $ createSystemCursorFFI v1
{-# INLINE createSystemCursor #-}

freeCursor :: MonadIO m => Cursor -> m ()
freeCursor v1 = liftIO $ freeCursorFFI v1
{-# INLINE freeCursor #-}

getCursor :: MonadIO m => m Cursor
getCursor = liftIO getCursorFFI
{-# INLINE getCursor #-}

getDefaultCursor :: MonadIO m => m Cursor
getDefaultCursor = liftIO getDefaultCursorFFI
{-# INLINE getDefaultCursor #-}

getGlobalMouseState :: MonadIO m => Ptr CInt -> Ptr CInt -> m Word32
getGlobalMouseState v1 v2 = liftIO $ getGlobalMouseStateFFI v1 v2
{-# INLINE getGlobalMouseState #-}

getMouseFocus :: MonadIO m => m Window
getMouseFocus = liftIO getMouseFocusFFI
{-# INLINE getMouseFocus #-}

getMouseState :: MonadIO m => Ptr CInt -> Ptr CInt -> m Word32
getMouseState v1 v2 = liftIO $ getMouseStateFFI v1 v2
{-# INLINE getMouseState #-}

getRelativeMouseMode :: MonadIO m => m Bool
getRelativeMouseMode = liftIO getRelativeMouseModeFFI
{-# INLINE getRelativeMouseMode #-}

getRelativeMouseState :: MonadIO m => Ptr CInt -> Ptr CInt -> m Word32
getRelativeMouseState v1 v2 = liftIO $ getRelativeMouseStateFFI v1 v2
{-# INLINE getRelativeMouseState #-}

setCursor :: MonadIO m => Cursor -> m ()
setCursor v1 = liftIO $ setCursorFFI v1
{-# INLINE setCursor #-}

setRelativeMouseMode :: MonadIO m => Bool -> m CInt
setRelativeMouseMode v1 = liftIO $ setRelativeMouseModeFFI v1
{-# INLINE setRelativeMouseMode #-}

showCursor :: MonadIO m => CInt -> m CInt
showCursor v1 = liftIO $ showCursorFFI v1
{-# INLINE showCursor #-}

warpMouseGlobal :: MonadIO m => CInt -> CInt -> m CInt
warpMouseGlobal v1 v2 = liftIO $ warpMouseGlobalFFI v1 v2
{-# INLINE warpMouseGlobal #-}

warpMouseInWindow :: MonadIO m => Window -> CInt -> CInt -> m ()
warpMouseInWindow v1 v2 v3 = liftIO $ warpMouseInWindowFFI v1 v2 v3
{-# INLINE warpMouseInWindow #-}

joystickClose :: MonadIO m => Joystick -> m ()
joystickClose v1 = liftIO $ joystickCloseFFI v1
{-# INLINE joystickClose #-}

joystickCurrentPowerLevel :: MonadIO m => Joystick -> m JoystickPowerLevel
joystickCurrentPowerLevel v1 = liftIO $ joystickCurrentPowerLevelFFI v1
{-# INLINE joystickCurrentPowerLevel #-}

joystickEventState :: MonadIO m => CInt -> m CInt
joystickEventState v1 = liftIO $ joystickEventStateFFI v1
{-# INLINE joystickEventState #-}

joystickFromInstanceID :: MonadIO m => JoystickID -> m Joystick
joystickFromInstanceID v1 = liftIO $ joystickFromInstanceIDFFI v1
{-# INLINE joystickFromInstanceID #-}

joystickGetAttached :: MonadIO m => Joystick -> m Bool
joystickGetAttached v1 = liftIO $ joystickGetAttachedFFI v1
{-# INLINE joystickGetAttached #-}

joystickGetAxis :: MonadIO m => Joystick -> CInt -> m Int16
joystickGetAxis v1 v2 = liftIO $ joystickGetAxisFFI v1 v2
{-# INLINE joystickGetAxis #-}

joystickGetBall :: MonadIO m => Joystick -> CInt -> Ptr CInt -> Ptr CInt -> m CInt
joystickGetBall v1 v2 v3 v4 = liftIO $ joystickGetBallFFI v1 v2 v3 v4
{-# INLINE joystickGetBall #-}

joystickGetButton :: MonadIO m => Joystick -> CInt -> m Word8
joystickGetButton v1 v2 = liftIO $ joystickGetButtonFFI v1 v2
{-# INLINE joystickGetButton #-}

joystickGetDeviceGUID :: MonadIO m => CInt -> m JoystickGUID
joystickGetDeviceGUID device_index = liftIO . alloca $ \ptr -> do
  joystickGetDeviceGUIDFFI device_index ptr
  peek ptr
{-# INLINE joystickGetDeviceGUID #-}

joystickGetGUID :: MonadIO m => Joystick -> m JoystickGUID
joystickGetGUID joystick = liftIO . alloca $ \ptr -> do
  joystickGetGUIDFFI joystick ptr
  peek ptr
{-# INLINE joystickGetGUID #-}

joystickGetGUIDFromString :: MonadIO m => CString -> m JoystickGUID
joystickGetGUIDFromString pchGUID = liftIO . alloca $ \ptr -> do
  joystickGetGUIDFromStringFFI pchGUID ptr
  peek ptr
{-# INLINE joystickGetGUIDFromString #-}

joystickGetGUIDString :: MonadIO m => JoystickGUID -> CString -> CInt -> m ()
joystickGetGUIDString guid pszGUID cbGUID = liftIO . alloca $ \ptr -> do
  poke ptr guid
  joystickGetGUIDStringFFI ptr pszGUID cbGUID
{-# INLINE joystickGetGUIDString #-}

joystickGetHat :: MonadIO m => Joystick -> CInt -> m Word8
joystickGetHat v1 v2 = liftIO $ joystickGetHatFFI v1 v2
{-# INLINE joystickGetHat #-}

joystickInstanceID :: MonadIO m => Joystick -> m JoystickID
joystickInstanceID v1 = liftIO $ joystickInstanceIDFFI v1
{-# INLINE joystickInstanceID #-}

joystickName :: MonadIO m => Joystick -> m CString
joystickName v1 = liftIO $ joystickNameFFI v1
{-# INLINE joystickName #-}

joystickNameForIndex :: MonadIO m => CInt -> m CString
joystickNameForIndex v1 = liftIO $ joystickNameForIndexFFI v1
{-# INLINE joystickNameForIndex #-}

joystickNumAxes :: MonadIO m => Joystick -> m CInt
joystickNumAxes v1 = liftIO $ joystickNumAxesFFI v1
{-# INLINE joystickNumAxes #-}

joystickNumBalls :: MonadIO m => Joystick -> m CInt
joystickNumBalls v1 = liftIO $ joystickNumBallsFFI v1
{-# INLINE joystickNumBalls #-}

joystickNumButtons :: MonadIO m => Joystick -> m CInt
joystickNumButtons v1 = liftIO $ joystickNumButtonsFFI v1
{-# INLINE joystickNumButtons #-}

joystickNumHats :: MonadIO m => Joystick -> m CInt
joystickNumHats v1 = liftIO $ joystickNumHatsFFI v1
{-# INLINE joystickNumHats #-}

joystickOpen :: MonadIO m => CInt -> m Joystick
joystickOpen v1 = liftIO $ joystickOpenFFI v1
{-# INLINE joystickOpen #-}

joystickUpdate :: MonadIO m => m ()
joystickUpdate = liftIO joystickUpdateFFI
{-# INLINE joystickUpdate #-}

numJoysticks :: MonadIO m => m CInt
numJoysticks = liftIO numJoysticksFFI
{-# INLINE numJoysticks #-}

gameControllerAddMapping :: MonadIO m => CString -> m CInt
gameControllerAddMapping v1 = liftIO $ gameControllerAddMappingFFI v1
{-# INLINE gameControllerAddMapping #-}

gameControllerAddMappingsFromFile :: MonadIO m => CString -> m CInt
gameControllerAddMappingsFromFile file = liftIO $ do
  rw <- withCString "rb" $ rwFromFile file
  gameControllerAddMappingsFromRW rw 1
{-# INLINE gameControllerAddMappingsFromFile #-}

gameControllerAddMappingsFromRW :: MonadIO m => Ptr RWops -> CInt -> m CInt
gameControllerAddMappingsFromRW v1 v2 = liftIO $ gameControllerAddMappingsFromRWFFI v1 v2
{-# INLINE gameControllerAddMappingsFromRW #-}

gameControllerClose :: MonadIO m => GameController -> m ()
gameControllerClose v1 = liftIO $ gameControllerCloseFFI v1
{-# INLINE gameControllerClose #-}

gameControllerEventState :: MonadIO m => CInt -> m CInt
gameControllerEventState v1 = liftIO $ gameControllerEventStateFFI v1
{-# INLINE gameControllerEventState #-}

gameControllerFromInstanceID :: MonadIO m => JoystickID -> m GameController
gameControllerFromInstanceID v1 = liftIO $ gameControllerFromInstanceIDFFI v1
{-# INLINE gameControllerFromInstanceID #-}

gameControllerGetAttached :: MonadIO m => GameController -> m Bool
gameControllerGetAttached v1 = liftIO $ gameControllerGetAttachedFFI v1
{-# INLINE gameControllerGetAttached #-}

gameControllerGetAxis :: MonadIO m => GameController -> GameControllerAxis -> m Int16
gameControllerGetAxis v1 v2 = liftIO $ gameControllerGetAxisFFI v1 v2
{-# INLINE gameControllerGetAxis #-}

gameControllerGetAxisFromString :: MonadIO m => CString -> m GameControllerAxis
gameControllerGetAxisFromString v1 = liftIO $ gameControllerGetAxisFromStringFFI v1
{-# INLINE gameControllerGetAxisFromString #-}

gameControllerGetBindForAxis :: MonadIO m => GameController -> GameControllerAxis -> m GameControllerButtonBind
gameControllerGetBindForAxis gamecontroller axis = liftIO . alloca $ \ptr -> do
  gameControllerGetBindForAxisFFI gamecontroller axis ptr
  peek ptr
{-# INLINE gameControllerGetBindForAxis #-}

gameControllerGetBindForButton :: MonadIO m => GameController -> GameControllerButton -> m GameControllerButtonBind
gameControllerGetBindForButton gamecontroller button = liftIO . alloca $ \ptr -> do
  gameControllerGetBindForButtonFFI gamecontroller button ptr
  peek ptr
{-# INLINE gameControllerGetBindForButton #-}

gameControllerGetButton :: MonadIO m => GameController -> GameControllerButton -> m Word8
gameControllerGetButton v1 v2 = liftIO $ gameControllerGetButtonFFI v1 v2
{-# INLINE gameControllerGetButton #-}

gameControllerGetButtonFromString :: MonadIO m => CString -> m GameControllerButton
gameControllerGetButtonFromString v1 = liftIO $ gameControllerGetButtonFromStringFFI v1
{-# INLINE gameControllerGetButtonFromString #-}

gameControllerGetJoystick :: MonadIO m => GameController -> m Joystick
gameControllerGetJoystick v1 = liftIO $ gameControllerGetJoystickFFI v1
{-# INLINE gameControllerGetJoystick #-}

gameControllerGetStringForAxis :: MonadIO m => GameControllerAxis -> m CString
gameControllerGetStringForAxis v1 = liftIO $ gameControllerGetStringForAxisFFI v1
{-# INLINE gameControllerGetStringForAxis #-}

gameControllerGetStringForButton :: MonadIO m => GameControllerButton -> m CString
gameControllerGetStringForButton v1 = liftIO $ gameControllerGetStringForButtonFFI v1
{-# INLINE gameControllerGetStringForButton #-}

gameControllerMapping :: MonadIO m => GameController -> m CString
gameControllerMapping v1 = liftIO $ gameControllerMappingFFI v1
{-# INLINE gameControllerMapping #-}

gameControllerMappingForGUID :: MonadIO m => JoystickGUID -> m CString
gameControllerMappingForGUID guid = liftIO . alloca $ \ptr -> do
  poke ptr guid
  gameControllerMappingForGUIDFFI ptr
{-# INLINE gameControllerMappingForGUID #-}

gameControllerName :: MonadIO m => GameController -> m CString
gameControllerName v1 = liftIO $ gameControllerNameFFI v1
{-# INLINE gameControllerName #-}

gameControllerNameForIndex :: MonadIO m => CInt -> m CString
gameControllerNameForIndex v1 = liftIO $ gameControllerNameForIndexFFI v1
{-# INLINE gameControllerNameForIndex #-}

gameControllerOpen :: MonadIO m => CInt -> m GameController
gameControllerOpen v1 = liftIO $ gameControllerOpenFFI v1
{-# INLINE gameControllerOpen #-}

gameControllerUpdate :: MonadIO m => m ()
gameControllerUpdate = liftIO gameControllerUpdateFFI
{-# INLINE gameControllerUpdate #-}

isGameController :: MonadIO m => CInt -> m Bool
isGameController v1 = liftIO $ isGameControllerFFI v1
{-# INLINE isGameController #-}
