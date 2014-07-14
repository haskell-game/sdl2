module Graphics.UI.SDL.Event (
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

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Filesystem
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_AddEventWatch" addEventWatch :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_DelEventWatch" delEventWatch :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_EventState" eventState :: Word32 -> CInt -> IO Word8
foreign import ccall "SDL.h SDL_FilterEvents" filterEvents :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_FlushEvent" flushEvent :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_FlushEvents" flushEvents :: Word32 -> Word32 -> IO ()
foreign import ccall "SDL.h SDL_GetEventFilter" getEventFilter :: Ptr EventFilter -> Ptr (Ptr ()) -> IO Bool
foreign import ccall "SDL.h SDL_GetNumTouchDevices" getNumTouchDevices :: IO CInt
foreign import ccall "SDL.h SDL_GetNumTouchFingers" getNumTouchFingers :: TouchID -> IO CInt
foreign import ccall "SDL.h SDL_GetTouchDevice" getTouchDevice :: CInt -> IO TouchID
foreign import ccall "SDL.h SDL_GetTouchFinger" getTouchFinger :: TouchID -> CInt -> IO (Ptr Finger)
foreign import ccall "SDL.h SDL_HasEvent" hasEvent :: Word32 -> IO Bool
foreign import ccall "SDL.h SDL_HasEvents" hasEvents :: Word32 -> Word32 -> IO Bool
foreign import ccall "SDL.h SDL_LoadDollarTemplates" loadDollarTemplates :: TouchID -> Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_PeepEvents" peepEvents :: Ptr Event -> CInt -> EventAction -> Word32 -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_PollEvent" pollEvent :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_PumpEvents" pumpEvents :: IO ()
foreign import ccall "SDL.h SDL_PushEvent" pushEvent :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_RecordGesture" recordGesture :: TouchID -> IO CInt
foreign import ccall "SDL.h SDL_RegisterEvents" registerEvents :: CInt -> IO Word32
foreign import ccall "SDL.h SDL_SaveAllDollarTemplates" saveAllDollarTemplates :: Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_SaveDollarTemplate" saveDollarTemplate :: GestureID -> Ptr RWops -> IO CInt
foreign import ccall "SDL.h SDL_SetEventFilter" setEventFilter :: EventFilter -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_WaitEvent" waitEvent :: Ptr Event -> IO CInt
foreign import ccall "SDL.h SDL_WaitEventTimeout" waitEventTimeout :: Ptr Event -> CInt -> IO CInt

foreign import ccall "SDL.h SDL_GetKeyFromName" getKeyFromName :: CString -> IO Keycode
foreign import ccall "SDL.h SDL_GetKeyFromScancode" getKeyFromScancode :: Scancode -> IO Keycode
foreign import ccall "SDL.h SDL_GetKeyName" getKeyName :: Keycode -> IO CString
foreign import ccall "SDL.h SDL_GetKeyboardFocus" getKeyboardFocus :: IO Window
foreign import ccall "SDL.h SDL_GetKeyboardState" getKeyboardState :: Ptr CInt -> IO (Ptr Word8)
foreign import ccall "SDL.h SDL_GetModState" getModState :: IO Keymod
foreign import ccall "SDL.h SDL_GetScancodeFromKey" getScancodeFromKey :: Keycode -> IO Scancode
foreign import ccall "SDL.h SDL_GetScancodeFromName" getScancodeFromName :: CString -> IO Scancode
foreign import ccall "SDL.h SDL_GetScancodeName" getScancodeName :: Scancode -> IO CString
foreign import ccall "SDL.h SDL_HasScreenKeyboardSupport" hasScreenKeyboardSupport :: IO Bool
foreign import ccall "SDL.h SDL_IsScreenKeyboardShown" isScreenKeyboardShown :: Window -> IO Bool
foreign import ccall "SDL.h SDL_IsTextInputActive" isTextInputActive :: IO Bool
foreign import ccall "SDL.h SDL_SetModState" setModState :: Keymod -> IO ()
foreign import ccall "SDL.h SDL_SetTextInputRect" setTextInputRect :: Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_StartTextInput" startTextInput :: IO ()
foreign import ccall "SDL.h SDL_StopTextInput" stopTextInput :: IO ()

foreign import ccall "SDL.h SDL_CreateColorCursor" createColorCursor :: Ptr Surface -> CInt -> CInt -> IO Cursor
foreign import ccall "SDL.h SDL_CreateCursor" createCursor :: Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO Cursor
foreign import ccall "SDL.h SDL_CreateSystemCursor" createSystemCursor :: SystemCursor -> IO Cursor
foreign import ccall "SDL.h SDL_FreeCursor" freeCursor :: Cursor -> IO ()
foreign import ccall "SDL.h SDL_GetCursor" getCursor :: IO Cursor
foreign import ccall "SDL.h SDL_GetDefaultCursor" getDefaultCursor :: IO Cursor
foreign import ccall "SDL.h SDL_GetMouseFocus" getMouseFocus :: IO Window
foreign import ccall "SDL.h SDL_GetMouseState" getMouseState :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_GetRelativeMouseMode" getRelativeMouseMode :: IO Bool
foreign import ccall "SDL.h SDL_GetRelativeMouseState" getRelativeMouseState :: Ptr CInt -> Ptr CInt -> IO Word32
foreign import ccall "SDL.h SDL_SetCursor" setCursor :: Cursor -> IO ()
foreign import ccall "SDL.h SDL_SetRelativeMouseMode" setRelativeMouseMode :: Bool -> IO CInt
foreign import ccall "SDL.h SDL_ShowCursor" showCursor :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_WarpMouseInWindow" warpMouseInWindow :: Window -> CInt -> CInt -> IO ()

foreign import ccall "SDL.h SDL_JoystickClose" joystickClose :: Joystick -> IO ()
foreign import ccall "SDL.h SDL_JoystickEventState" joystickEventState :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_JoystickGetAttached" joystickGetAttached :: Joystick -> IO Bool
foreign import ccall "SDL.h SDL_JoystickGetAxis" joystickGetAxis :: Joystick -> CInt -> IO Int16
foreign import ccall "SDL.h SDL_JoystickGetBall" joystickGetBall :: Joystick -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_JoystickGetButton" joystickGetButton :: Joystick -> CInt -> IO Word8
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetDeviceGUID" joystickGetDeviceGUID' :: CInt -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUID" joystickGetGUID' :: Joystick -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUIDFromString" joystickGetGUIDFromString' :: CString -> Ptr JoystickGUID -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_JoystickGetGUIDString" joystickGetGUIDString' :: Ptr JoystickGUID -> CString -> CInt -> IO ()
foreign import ccall "SDL.h SDL_JoystickGetHat" joystickGetHat :: Joystick -> CInt -> IO Word8
foreign import ccall "SDL.h SDL_JoystickInstanceID" joystickInstanceID :: Joystick -> IO JoystickID
foreign import ccall "SDL.h SDL_JoystickName" joystickName :: Joystick -> IO CString
foreign import ccall "SDL.h SDL_JoystickNameForIndex" joystickNameForIndex :: CInt -> IO CString
foreign import ccall "SDL.h SDL_JoystickNumAxes" joystickNumAxes :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumBalls" joystickNumBalls :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumButtons" joystickNumButtons :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickNumHats" joystickNumHats :: Joystick -> IO CInt
foreign import ccall "SDL.h SDL_JoystickOpen" joystickOpen :: CInt -> IO Joystick
foreign import ccall "SDL.h SDL_JoystickUpdate" joystickUpdate :: IO ()
foreign import ccall "SDL.h SDL_NumJoysticks" numJoysticks :: IO CInt

foreign import ccall "SDL.h SDL_GameControllerAddMapping" gameControllerAddMapping :: CString -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerAddMappingsFromRW" gameControllerAddMappingsFromRW :: Ptr RWops -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerClose" gameControllerClose :: GameController -> IO ()
foreign import ccall "SDL.h SDL_GameControllerEventState" gameControllerEventState :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GameControllerGetAttached" gameControllerGetAttached :: GameController -> IO Bool
foreign import ccall "SDL.h SDL_GameControllerGetAxis" gameControllerGetAxis :: GameController -> GameControllerAxis -> IO Int16
foreign import ccall "SDL.h SDL_GameControllerGetAxisFromString" gameControllerGetAxisFromString :: CString -> IO GameControllerAxis
foreign import ccall "sdlhelper.h SDLHelper_GameControllerGetBindForAxis" gameControllerGetBindForAxis' :: GameController -> GameControllerAxis -> Ptr GameControllerButtonBind -> IO ()
foreign import ccall "sdlhelper.h SDLHelper_GameControllerGetBindForButton" gameControllerGetBindForButton' :: GameController -> GameControllerButton -> Ptr GameControllerButtonBind -> IO ()
foreign import ccall "SDL.h SDL_GameControllerGetButton" gameControllerGetButton :: GameController -> GameControllerButton -> IO Word8
foreign import ccall "SDL.h SDL_GameControllerGetButtonFromString" gameControllerGetButtonFromString :: CString -> IO GameControllerButton
foreign import ccall "SDL.h SDL_GameControllerGetJoystick" gameControllerGetJoystick :: GameController -> IO Joystick
foreign import ccall "SDL.h SDL_GameControllerGetStringForAxis" gameControllerGetStringForAxis :: GameControllerAxis -> IO CString
foreign import ccall "SDL.h SDL_GameControllerGetStringForButton" gameControllerGetStringForButton :: GameControllerButton -> IO CString
foreign import ccall "SDL.h SDL_GameControllerMapping" gameControllerMapping :: GameController -> IO CString
foreign import ccall "sdlhelper.h SDLHelper_GameControllerMappingForGUID" gameControllerMappingForGUID' :: Ptr JoystickGUID -> IO CString
foreign import ccall "SDL.h SDL_GameControllerName" gameControllerName :: GameController -> IO CString
foreign import ccall "SDL.h SDL_GameControllerNameForIndex" gameControllerNameForIndex :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GameControllerOpen" gameControllerOpen :: CInt -> IO GameController
foreign import ccall "SDL.h SDL_GameControllerUpdate" gameControllerUpdate :: IO ()
foreign import ccall "SDL.h SDL_IsGameController" isGameController :: CInt -> IO Bool

quitRequested :: IO Bool
quitRequested = do
	pumpEvents
	ev <- peepEvents nullPtr 0 eventActionPeekEvent eventTypeQuit eventTypeQuit
	return $ ev > 0

joystickGetDeviceGUID :: CInt -> IO JoystickGUID
joystickGetDeviceGUID device_index = alloca $ \ptr -> do
	joystickGetDeviceGUID' device_index ptr
	peek ptr

joystickGetGUID :: Joystick -> IO JoystickGUID
joystickGetGUID joystick = alloca $ \ptr -> do
	joystickGetGUID' joystick ptr
	peek ptr

joystickGetGUIDFromString :: CString -> IO JoystickGUID
joystickGetGUIDFromString pchGUID = alloca $ \ptr -> do
	joystickGetGUIDFromString' pchGUID ptr
	peek ptr

joystickGetGUIDString :: JoystickGUID -> CString -> CInt -> IO ()
joystickGetGUIDString guid pszGUID cbGUID = alloca $ \ptr -> do
	poke ptr guid
	joystickGetGUIDString' ptr pszGUID cbGUID

gameControllerAddMappingsFromFile :: CString -> IO CInt
gameControllerAddMappingsFromFile file = do
	rw <- withCString "rb" $ rwFromFile file
	gameControllerAddMappingsFromRW rw 1

gameControllerGetBindForAxis :: GameController -> GameControllerAxis -> IO GameControllerButtonBind
gameControllerGetBindForAxis gamecontroller axis = alloca $ \ptr -> do
	gameControllerGetBindForAxis' gamecontroller axis ptr
	peek ptr

gameControllerGetBindForButton :: GameController -> GameControllerButton -> IO GameControllerButtonBind
gameControllerGetBindForButton gamecontroller button = alloca $ \ptr -> do
	gameControllerGetBindForButton' gamecontroller button ptr
	peek ptr

gameControllerMappingForGUID :: JoystickGUID -> IO CString
gameControllerMappingForGUID guid = alloca $ \ptr -> do
	poke ptr guid
	gameControllerMappingForGUID' ptr
