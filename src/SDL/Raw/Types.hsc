{-# LANGUAGE DeriveDataTypeable #-}
module SDL.Raw.Types (
  -- * Type Aliases
  -- ** Function Types
  VkGetInstanceProcAddrFunc,

  AudioCallback,
  EventFilter,
  HintCallback,
  LogOutputFunction,
  ThreadFunction,
  TimerCallback,

  mkAudioCallback,
  mkEventFilter,
  mkHintCallback,
  mkLogOutputFunction,
  mkThreadFunction,
  mkTimerCallback,

  -- ** Common Types
  AudioDeviceID,
  AudioFormat,
  Cond,
  Cursor,
  FingerID,
  GameController,
  GestureID,
  GLContext,
  Haptic,
  Joystick,
  JoystickID,
  Mutex,
  Renderer,
  Sem,
  SpinLock,
  SysWMinfo,
  SysWMmsg,
  Texture,
  Thread,
  ThreadID,
  TimerID,
  TLSID,
  TouchID,
  VkInstance,
  VkSurfaceKHR,
  Window,

  -- * Data Structures
  Atomic(..),
  AudioCVT(..),
  AudioSpec(..),
  Color(..),
  DisplayMode(..),
  Event(..),
  Finger(..),
  GameControllerButtonBind(..),
  HapticDirection(..),
  HapticEffect(..),
  JoystickGUID(..),
  Keysym(..),
  MessageBoxButtonData(..),
  MessageBoxColor(..),
  MessageBoxColorScheme(..),
  MessageBoxData(..),
  Palette(..),
  PixelFormat(..),
  Point(..),
  FPoint(..),
  Rect(..),
  FRect(..),
  RendererInfo(..),
  RWops(..),
  Surface(..),
  Version(..)
) where

#include "SDL.h"

import Data.Int
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import SDL.Raw.Enum

type VkGetInstanceProcAddrFunc = VkInstance -> CString -> IO (FunPtr ())

type AudioCallback = FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ())
type EventFilter = FunPtr (Ptr () -> Ptr Event -> IO CInt)
type HintCallback = FunPtr (Ptr () -> CString -> CString -> CString -> IO ())
type LogOutputFunction = FunPtr (Ptr () -> CInt -> LogPriority -> CString -> IO ())
type ThreadFunction = FunPtr (Ptr () -> IO CInt)
type TimerCallback = FunPtr (Word32 -> Ptr () -> IO Word32)

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import ccall "wrapper"
  mkAudioCallback :: (Ptr () -> Ptr Word8 -> CInt -> IO ()) -> IO AudioCallback

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import ccall "wrapper"
  mkEventFilter :: (Ptr () -> Ptr Event -> IO CInt) -> IO EventFilter

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import ccall "wrapper"
  mkHintCallback :: (Ptr () -> CString -> CString -> CString -> IO ()) -> IO HintCallback

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import ccall "wrapper"
  mkLogOutputFunction :: (Ptr () -> CInt -> LogPriority -> CString -> IO ()) -> IO LogOutputFunction

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import ccall "wrapper"
  mkThreadFunction :: (Ptr () -> IO CInt) -> IO ThreadFunction

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import ccall "wrapper"
  mkTimerCallback :: (Word32 -> Ptr () -> IO Word32) -> IO TimerCallback

type AudioDeviceID = Word32
type Cond = Ptr ()
type Cursor = Ptr ()
type FingerID = Int64
type GameController = Ptr ()
type GestureID = Int64
type GLContext = Ptr ()
type Haptic = Ptr ()
type Joystick = Ptr ()
type JoystickID = Int32
type Mutex = Ptr ()
type Renderer = Ptr ()
type Sem = Ptr ()
type SpinLock = CInt
type SysWMinfo = Ptr ()
type SysWMmsg = Ptr ()
type Texture = Ptr ()
type Thread = Ptr ()
type ThreadID = CULong
type TimerID = CInt
type TLSID = CUInt
type TouchID = Int64
type VkInstance = Ptr ()
type VkSurfaceKHR = Word64
type Window = Ptr ()

data Atomic = Atomic
  { atomicValue :: !CInt
  } deriving (Eq, Show, Typeable)

instance Storable Atomic where
  sizeOf _ = (#size SDL_atomic_t)
  alignment = sizeOf
  peek ptr = do
    value <- (#peek SDL_atomic_t, value) ptr
    return $! Atomic value
  poke ptr (Atomic value) = do
    (#poke SDL_atomic_t, value) ptr value

data AudioCVT = AudioCVT
  { audioCVTNeeded :: !CInt
  , audioCVTSrcFormat :: !AudioFormat
  , audioCVTDstFormat :: !AudioFormat
  , audioCVTRateIncr :: !CDouble
  , audioCVTBuf :: !(Ptr Word8)
  , audioCVTLen :: !CInt
  , audioCVTLenCvt :: !CInt
  , audioCVTLenMult :: !CInt
  , audioCVTLenRatio :: !CDouble
  } deriving (Eq, Show, Typeable)

instance Storable AudioCVT where
  sizeOf _ = (#size SDL_AudioCVT)
  alignment = sizeOf
  peek ptr = do
    needed <- (#peek SDL_AudioCVT, needed) ptr
    src_format <- (#peek SDL_AudioCVT, src_format) ptr
    dst_format <- (#peek SDL_AudioCVT, dst_format) ptr
    rate_incr <- (#peek SDL_AudioCVT, rate_incr) ptr
    buf <- (#peek SDL_AudioCVT, buf) ptr
    len <- (#peek SDL_AudioCVT, len) ptr
    len_cvt <- (#peek SDL_AudioCVT, len_cvt) ptr
    len_mult <- (#peek SDL_AudioCVT, len_mult) ptr
    len_ratio <- (#peek SDL_AudioCVT, len_ratio) ptr
    return $! AudioCVT needed src_format dst_format rate_incr buf len len_cvt len_mult len_ratio
  poke ptr (AudioCVT needed src_format dst_format rate_incr buf len len_cvt len_mult len_ratio) = do
    (#poke SDL_AudioCVT, needed) ptr needed
    (#poke SDL_AudioCVT, src_format) ptr src_format
    (#poke SDL_AudioCVT, dst_format) ptr dst_format
    (#poke SDL_AudioCVT, rate_incr) ptr rate_incr
    (#poke SDL_AudioCVT, buf) ptr buf
    (#poke SDL_AudioCVT, len) ptr len
    (#poke SDL_AudioCVT, len_cvt) ptr len_cvt
    (#poke SDL_AudioCVT, len_mult) ptr len_mult
    (#poke SDL_AudioCVT, len_ratio) ptr len_ratio

data AudioSpec = AudioSpec
  { audioSpecFreq :: !CInt
  , audioSpecFormat :: !AudioFormat
  , audioSpecChannels :: !Word8
  , audioSpecSilence :: !Word8
  , audioSpecSamples :: !Word16
  , audioSpecSize :: !Word32
  , audioSpecCallback :: !AudioCallback
  , audioSpecUserdata :: !(Ptr ())
  } deriving (Eq, Show, Typeable)

instance Storable AudioSpec where
  sizeOf _ = (#size SDL_AudioSpec)
  alignment = sizeOf
  peek ptr = do
    freq <- (#peek SDL_AudioSpec, freq) ptr
    format <- (#peek SDL_AudioSpec, format) ptr
    channels <- (#peek SDL_AudioSpec, channels) ptr
    silence <- (#peek SDL_AudioSpec, silence) ptr
    samples <- (#peek SDL_AudioSpec, samples) ptr
    size <- (#peek SDL_AudioSpec, size) ptr
    callback <- (#peek SDL_AudioSpec, callback) ptr
    userdata <- (#peek SDL_AudioSpec, userdata) ptr
    return $! AudioSpec freq format channels silence samples size callback userdata
  poke ptr (AudioSpec freq format channels silence samples size callback userdata) = do
    (#poke SDL_AudioSpec, freq) ptr freq
    (#poke SDL_AudioSpec, format) ptr format
    (#poke SDL_AudioSpec, channels) ptr channels
    (#poke SDL_AudioSpec, silence) ptr silence
    (#poke SDL_AudioSpec, samples) ptr samples
    (#poke SDL_AudioSpec, size) ptr size
    (#poke SDL_AudioSpec, callback) ptr callback
    (#poke SDL_AudioSpec, userdata) ptr userdata

data Color = Color
  { colorR :: !Word8
  , colorG :: !Word8
  , colorB :: !Word8
  , colorA :: !Word8
  } deriving (Eq, Show, Typeable)

instance Storable Color where
  sizeOf _ = (#size SDL_Color)
  alignment = sizeOf
  peek ptr = do
    r <- (#peek SDL_Color, r) ptr
    g <- (#peek SDL_Color, g) ptr
    b <- (#peek SDL_Color, b) ptr
    a <- (#peek SDL_Color, a) ptr
    return $! Color r g b a
  poke ptr (Color r g b a) = do
    (#poke SDL_Color, r) ptr r
    (#poke SDL_Color, g) ptr g
    (#poke SDL_Color, b) ptr b
    (#poke SDL_Color, a) ptr a

data DisplayMode = DisplayMode
  { displayModeFormat :: !Word32
  , displayModeW :: !CInt
  , displayModeH :: !CInt
  , displayModeRefreshRate :: !CInt
  , displayModeDriverData :: !(Ptr ())
  } deriving (Eq, Show, Typeable)

instance Storable DisplayMode where
  sizeOf _ = (#size SDL_DisplayMode)
  alignment = sizeOf
  peek ptr = do
    format <- (#peek SDL_DisplayMode, format) ptr
    w <- (#peek SDL_DisplayMode, w) ptr
    h <- (#peek SDL_DisplayMode, h) ptr
    refresh_rate <- (#peek SDL_DisplayMode, refresh_rate) ptr
    driverdata <- (#peek SDL_DisplayMode, driverdata) ptr
    return $! DisplayMode format w h refresh_rate driverdata
  poke ptr (DisplayMode format w h refresh_rate driverdata) = do
    (#poke SDL_DisplayMode, format) ptr format
    (#poke SDL_DisplayMode, w) ptr w
    (#poke SDL_DisplayMode, h) ptr h
    (#poke SDL_DisplayMode, refresh_rate) ptr refresh_rate
    (#poke SDL_DisplayMode, driverdata) ptr driverdata

data Event
  = WindowEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , windowEventWindowID :: !Word32
    , windowEventEvent :: !Word8
    , windowEventData1 :: !Int32
    , windowEventData2 :: !Int32
    }
  | KeyboardEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , keyboardEventWindowID :: !Word32
    , keyboardEventState :: !Word8
    , keyboardEventRepeat :: !Word8
    , keyboardEventKeysym :: !Keysym
    }
  | TextEditingEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , textEditingEventWindowID :: !Word32
    , textEditingEventText :: ![CChar]
    , textEditingEventStart :: !Int32
    , textEditingEventLength :: !Int32
    }
  | TextInputEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , textInputEventWindowID :: !Word32
    , textInputEventText :: ![CChar]
    }
  | KeymapChangedEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    }
  | MouseMotionEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , mouseMotionEventWindowID :: !Word32
    , mouseMotionEventWhich :: !Word32
    , mouseMotionEventState :: !Word32
    , mouseMotionEventX :: !Int32
    , mouseMotionEventY :: !Int32
    , mouseMotionEventXRel :: !Int32
    , mouseMotionEventYRel :: !Int32
    }
  | MouseButtonEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , mouseButtonEventWindowID :: !Word32
    , mouseButtonEventWhich :: !Word32
    , mouseButtonEventButton :: !Word8
    , mouseButtonEventState :: !Word8
    , mouseButtonEventClicks :: !Word8
    , mouseButtonEventX :: !Int32
    , mouseButtonEventY :: !Int32
    }
  | MouseWheelEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , mouseWheelEventWindowID :: !Word32
    , mouseWheelEventWhich :: !Word32
    , mouseWheelEventX :: !Int32
    , mouseWheelEventY :: !Int32
    , mouseWheelEventDirection :: !Word32
    }
  | JoyAxisEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , joyAxisEventWhich :: !JoystickID
    , joyAxisEventAxis :: !Word8
    , joyAxisEventValue :: !Int16
    }
  | JoyBallEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , joyBallEventWhich :: !JoystickID
    , joyBallEventBall :: !Word8
    , joyBallEventXRel :: !Int16
    , joyBallEventYRel :: !Int16
    }
  | JoyHatEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , joyHatEventWhich :: !JoystickID
    , joyHatEventHat :: !Word8
    , joyHatEventValue :: !Word8
    }
  | JoyButtonEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , joyButtonEventWhich :: !JoystickID
    , joyButtonEventButton :: !Word8
    , joyButtonEventState :: !Word8
    }
  | JoyDeviceEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , joyDeviceEventWhich :: !Int32
    }
  | ControllerAxisEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , controllerAxisEventWhich :: !JoystickID
    , controllerAxisEventAxis :: !Word8
    , controllerAxisEventValue :: !Int16
    }
  | ControllerButtonEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , controllerButtonEventWhich :: !JoystickID
    , controllerButtonEventButton :: !Word8
    , controllerButtonEventState :: !Word8
    }
  | ControllerDeviceEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , controllerDeviceEventWhich :: !Int32
    }
  | AudioDeviceEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , audioDeviceEventWhich :: !Word32
    , audioDeviceEventIsCapture :: !Word8
    }
  | QuitEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    }
  | UserEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , userEventWindowID :: !Word32
    , userEventCode :: !Int32
    , userEventData1 :: !(Ptr ())
    , userEventData2 :: !(Ptr ())
    }
  | SysWMEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , sysWMEventMsg :: !SysWMmsg
    }
  | TouchFingerEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , touchFingerEventTouchID :: !TouchID
    , touchFingerEventFingerID :: !FingerID
    , touchFingerEventX :: !CFloat
    , touchFingerEventY :: !CFloat
    , touchFingerEventDX :: !CFloat
    , touchFingerEventDY :: !CFloat
    , touchFingerEventPressure :: !CFloat
    }
  | MultiGestureEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , multiGestureEventTouchID :: !TouchID
    , multiGestureEventDTheta :: !CFloat
    , multiGestureEventDDist :: !CFloat
    , multiGestureEventX :: !CFloat
    , multiGestureEventY :: !CFloat
    , multiGestureEventNumFingers :: !Word16
    }
  | DollarGestureEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , dollarGestureEventTouchID :: !TouchID
    , dollarGestureEventGestureID :: !GestureID
    , dollarGestureEventNumFingers :: !Word32
    , dollarGestureEventError :: !CFloat
    , dollarGestureEventX :: !CFloat
    , dollarGestureEventY :: !CFloat
    }
  | DropEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    , dropEventFile :: !CString
    }
  | ClipboardUpdateEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    }
  | UnknownEvent
    { eventType :: !Word32
    , eventTimestamp :: !Word32
    }
  deriving (Eq, Show, Typeable)

instance Storable Event where
  sizeOf _ = (#size SDL_Event)
  alignment = sizeOf
  peek ptr = do
    typ <- (#peek SDL_Event, common.type) ptr
    timestamp <- (#peek SDL_Event, common.timestamp) ptr
    case typ of
      (#const SDL_QUIT) ->
        return $! QuitEvent typ timestamp
      (#const SDL_WINDOWEVENT) -> do
        wid <- (#peek SDL_Event, window.windowID) ptr
        event <- (#peek SDL_Event, window.event) ptr
        data1 <- (#peek SDL_Event, window.data1) ptr
        data2 <- (#peek SDL_Event, window.data2) ptr
        return $! WindowEvent typ timestamp wid event data1 data2
      (#const SDL_SYSWMEVENT) -> do
        msg <- (#peek SDL_Event, syswm.msg) ptr
        return $! SysWMEvent typ timestamp msg
      (#const SDL_KEYDOWN) -> key $ KeyboardEvent typ timestamp
      (#const SDL_KEYUP) -> key $ KeyboardEvent typ timestamp
      (#const SDL_TEXTEDITING) -> do
        wid <- (#peek SDL_Event, edit.windowID) ptr
        text <- peekArray (#const SDL_TEXTEDITINGEVENT_TEXT_SIZE) $ (#ptr SDL_Event, edit.text) ptr
        start <- (#peek SDL_Event, edit.start) ptr
        len <- (#peek SDL_Event, edit.length) ptr
        let upToNull = takeWhile (/= 0) text
        return $! TextEditingEvent typ timestamp wid upToNull start len
      (#const SDL_TEXTINPUT) -> do
        wid <- (#peek SDL_Event, text.windowID) ptr
        text <- peekArray (#const SDL_TEXTINPUTEVENT_TEXT_SIZE) $ (#ptr SDL_Event, text.text) ptr
        let upToNull = takeWhile (/= 0) text
        return $! TextInputEvent typ timestamp wid upToNull
      (#const SDL_KEYMAPCHANGED) ->
        return $! KeymapChangedEvent typ timestamp
      (#const SDL_MOUSEMOTION) -> do
        wid <- (#peek SDL_Event, motion.windowID) ptr
        which <- (#peek SDL_Event, motion.which) ptr
        state <- (#peek SDL_Event, motion.state) ptr
        x <- (#peek SDL_Event, motion.x) ptr
        y <- (#peek SDL_Event, motion.y) ptr
        xrel <- (#peek SDL_Event, motion.xrel) ptr
        yrel <- (#peek SDL_Event, motion.yrel) ptr
        return $! MouseMotionEvent typ timestamp wid which state x y xrel yrel
      (#const SDL_MOUSEBUTTONDOWN) -> mouse $ MouseButtonEvent typ timestamp
      (#const SDL_MOUSEBUTTONUP) -> mouse $ MouseButtonEvent typ timestamp
      (#const SDL_MOUSEWHEEL) -> do
        wid <- (#peek SDL_Event, wheel.windowID) ptr
        which <- (#peek SDL_Event, wheel.which) ptr
        x <- (#peek SDL_Event, wheel.x) ptr
        y <- (#peek SDL_Event, wheel.y) ptr
        direction <- (#peek SDL_Event, wheel.direction) ptr
        return $! MouseWheelEvent typ timestamp wid which x y direction
      (#const SDL_JOYAXISMOTION) -> do
        which <- (#peek SDL_Event, jaxis.which) ptr
        axis <- (#peek SDL_Event, jaxis.axis) ptr
        value <- (#peek SDL_Event, jaxis.value) ptr
        return $! JoyAxisEvent typ timestamp which axis value
      (#const SDL_JOYBALLMOTION) -> do
        which <- (#peek SDL_Event, jball.which) ptr
        ball <- (#peek SDL_Event, jball.ball) ptr
        xrel <- (#peek SDL_Event, jball.xrel) ptr
        yrel <- (#peek SDL_Event, jball.yrel) ptr
        return $! JoyBallEvent typ timestamp which ball xrel yrel
      (#const SDL_JOYHATMOTION) -> do
        which <- (#peek SDL_Event, jhat.which) ptr
        hat <- (#peek SDL_Event, jhat.hat) ptr
        value <- (#peek SDL_Event, jhat.value) ptr
        return $! JoyHatEvent typ timestamp which hat value
      (#const SDL_JOYBUTTONDOWN) -> joybutton $ JoyButtonEvent typ timestamp
      (#const SDL_JOYBUTTONUP) -> joybutton $ JoyButtonEvent typ timestamp
      (#const SDL_JOYDEVICEADDED) -> joydevice $ JoyDeviceEvent typ timestamp
      (#const SDL_JOYDEVICEREMOVED) -> joydevice $ JoyDeviceEvent typ timestamp
      (#const SDL_CONTROLLERAXISMOTION) -> do
        which <- (#peek SDL_Event, caxis.which) ptr
        axis <- (#peek SDL_Event, caxis.axis) ptr
        value <- (#peek SDL_Event, caxis.value) ptr
        return $! ControllerAxisEvent typ timestamp which axis value
      (#const SDL_CONTROLLERBUTTONDOWN) -> controllerbutton $ ControllerButtonEvent typ timestamp
      (#const SDL_CONTROLLERBUTTONUP) -> controllerbutton $ ControllerButtonEvent typ timestamp
      (#const SDL_CONTROLLERDEVICEADDED) -> controllerdevice $ ControllerDeviceEvent typ timestamp
      (#const SDL_CONTROLLERDEVICEREMOVED) -> controllerdevice $ ControllerDeviceEvent typ timestamp
      (#const SDL_CONTROLLERDEVICEREMAPPED) -> controllerdevice $ ControllerDeviceEvent typ timestamp
      (#const SDL_AUDIODEVICEADDED) -> audiodevice $ AudioDeviceEvent typ timestamp
      (#const SDL_AUDIODEVICEREMOVED) -> audiodevice $ AudioDeviceEvent typ timestamp
      (#const SDL_FINGERDOWN) -> finger $ TouchFingerEvent typ timestamp
      (#const SDL_FINGERUP) -> finger $ TouchFingerEvent typ timestamp
      (#const SDL_FINGERMOTION) -> finger $ TouchFingerEvent typ timestamp
      (#const SDL_DOLLARGESTURE) -> dollargesture $ DollarGestureEvent typ timestamp
      (#const SDL_DOLLARRECORD) -> dollargesture $ DollarGestureEvent typ timestamp
      (#const SDL_MULTIGESTURE) -> do
        touchId <- (#peek SDL_Event, mgesture.touchId) ptr
        dTheta <- (#peek SDL_Event, mgesture.dTheta) ptr
        dDist <- (#peek SDL_Event, mgesture.dDist) ptr
        x <- (#peek SDL_Event, mgesture.x) ptr
        y <- (#peek SDL_Event, mgesture.y) ptr
        numFingers <- (#peek SDL_Event, mgesture.numFingers) ptr
        return $! MultiGestureEvent typ timestamp touchId dTheta dDist x y numFingers
      (#const SDL_CLIPBOARDUPDATE) ->
        return $! ClipboardUpdateEvent typ timestamp
      (#const SDL_DROPFILE) -> do
        file <- (#peek SDL_Event, drop.file) ptr
        return $! DropEvent typ timestamp file
      x | x >= (#const SDL_USEREVENT) -> do
        wid <- (#peek SDL_Event, user.windowID) ptr
        code <- (#peek SDL_Event, user.code) ptr
        data1 <- (#peek SDL_Event, user.data1) ptr
        data2 <- (#peek SDL_Event, user.data2) ptr
        return $! UserEvent typ timestamp wid code data1 data2
      _ -> return $! UnknownEvent typ timestamp
    where
    key f = do
      wid <- (#peek SDL_Event, key.windowID) ptr
      state <- (#peek SDL_Event, key.state) ptr
      repeat' <- (#peek SDL_Event, key.repeat) ptr
      keysym <- (#peek SDL_Event, key.keysym) ptr
      return $! f wid state repeat' keysym

    mouse f = do
      wid <- (#peek SDL_Event, button.windowID) ptr
      which <- (#peek SDL_Event, button.which) ptr
      button <- (#peek SDL_Event, button.button) ptr
      state <- (#peek SDL_Event, button.state) ptr
      clicks <- (#peek SDL_Event, button.clicks) ptr
      x <- (#peek SDL_Event, button.x) ptr
      y <- (#peek SDL_Event, button.y) ptr
      return $! f wid which button state clicks x y

    joybutton f = do
      which <- (#peek SDL_Event, jbutton.which) ptr
      button <- (#peek SDL_Event, jbutton.button) ptr
      state <- (#peek SDL_Event, jbutton.state) ptr
      return $! f which button state

    joydevice f = do
      which <- (#peek SDL_Event, jdevice.which) ptr
      return $! f which

    controllerbutton f = do
      which <- (#peek SDL_Event, cbutton.which) ptr
      button <- (#peek SDL_Event, cbutton.button) ptr
      state <- (#peek SDL_Event, cbutton.state) ptr
      return $! f which button state

    controllerdevice f = do
      which <- (#peek SDL_Event, cdevice.which) ptr
      return $! f which

    audiodevice f = do
      which <- (#peek SDL_Event, adevice.which) ptr
      iscapture <- (#peek SDL_Event, adevice.iscapture) ptr
      return $! f which iscapture

    finger f = do
      touchId <- (#peek SDL_Event, tfinger.touchId) ptr
      fingerId <- (#peek SDL_Event, tfinger.fingerId) ptr
      x <- (#peek SDL_Event, tfinger.x) ptr
      y <- (#peek SDL_Event, tfinger.y) ptr
      dx <- (#peek SDL_Event, tfinger.dx) ptr
      dy <- (#peek SDL_Event, tfinger.dy) ptr
      pressure <- (#peek SDL_Event, tfinger.pressure) ptr
      return $! f touchId fingerId x y dx dy pressure

    dollargesture f = do
      touchId <- (#peek SDL_Event, dgesture.touchId) ptr
      gestureId <- (#peek SDL_Event, dgesture.gestureId) ptr
      numFingers <- (#peek SDL_Event, dgesture.numFingers) ptr
      err <- (#peek SDL_Event, dgesture.error) ptr
      x <- (#peek SDL_Event, dgesture.x) ptr
      y <- (#peek SDL_Event, dgesture.y) ptr
      return $! f touchId gestureId numFingers err x y
  poke ptr ev = case ev of
    WindowEvent typ timestamp wid event data1 data2 -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, window.windowID) ptr wid
      (#poke SDL_Event, window.event) ptr event
      (#poke SDL_Event, window.data1) ptr data1
      (#poke SDL_Event, window.data2) ptr data2
    KeyboardEvent typ timestamp wid state repeat' keysym -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, key.windowID) ptr wid
      (#poke SDL_Event, key.state) ptr state
      (#poke SDL_Event, key.repeat) ptr repeat'
      (#poke SDL_Event, key.keysym) ptr keysym
    TextEditingEvent typ timestamp wid text start len -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, edit.windowID) ptr wid
      pokeArray ((#ptr SDL_Event, edit.text) ptr) text
      (#poke SDL_Event, edit.start) ptr start
      (#poke SDL_Event, edit.length) ptr len
    TextInputEvent typ timestamp wid text -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, text.windowID) ptr wid
      pokeArray ((#ptr SDL_Event, text.text) ptr) text
    KeymapChangedEvent typ timestamp -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
    MouseMotionEvent typ timestamp wid which state x y xrel yrel -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, motion.windowID) ptr wid
      (#poke SDL_Event, motion.which) ptr which
      (#poke SDL_Event, motion.state) ptr state
      (#poke SDL_Event, motion.x) ptr x
      (#poke SDL_Event, motion.y) ptr y
      (#poke SDL_Event, motion.xrel) ptr xrel
      (#poke SDL_Event, motion.yrel) ptr yrel
    MouseButtonEvent typ timestamp wid which button state clicks x y -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, button.windowID) ptr wid
      (#poke SDL_Event, button.which) ptr which
      (#poke SDL_Event, button.button) ptr button
      (#poke SDL_Event, button.state) ptr state
      (#poke SDL_Event, button.clicks) ptr clicks
      (#poke SDL_Event, button.x) ptr x
      (#poke SDL_Event, button.y) ptr y
    MouseWheelEvent typ timestamp wid which x y direction -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, wheel.windowID) ptr wid
      (#poke SDL_Event, wheel.which) ptr which
      (#poke SDL_Event, wheel.x) ptr x
      (#poke SDL_Event, wheel.y) ptr y
      (#poke SDL_Event, wheel.direction) ptr direction
    JoyAxisEvent typ timestamp which axis value -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, jaxis.which) ptr which
      (#poke SDL_Event, jaxis.axis) ptr axis
      (#poke SDL_Event, jaxis.value) ptr value
    JoyBallEvent typ timestamp which ball xrel yrel -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, jball.which) ptr which
      (#poke SDL_Event, jball.ball) ptr ball
      (#poke SDL_Event, jball.xrel) ptr xrel
      (#poke SDL_Event, jball.yrel) ptr yrel
    JoyHatEvent typ timestamp which hat value -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, jhat.which) ptr which
      (#poke SDL_Event, jhat.hat) ptr hat
      (#poke SDL_Event, jhat.value) ptr value
    JoyButtonEvent typ timestamp which button state -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, jbutton.which) ptr which
      (#poke SDL_Event, jbutton.button) ptr button
      (#poke SDL_Event, jbutton.state) ptr state
    JoyDeviceEvent typ timestamp which -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, jdevice.which) ptr which
    ControllerAxisEvent typ timestamp which axis value -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, caxis.which) ptr which
      (#poke SDL_Event, caxis.axis) ptr axis
      (#poke SDL_Event, caxis.value) ptr value
    ControllerButtonEvent typ timestamp which button state -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, cbutton.which) ptr which
      (#poke SDL_Event, cbutton.button) ptr button
      (#poke SDL_Event, cbutton.state) ptr state
    ControllerDeviceEvent typ timestamp which -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, cdevice.which) ptr which
    AudioDeviceEvent typ timestamp which iscapture -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, adevice.which) ptr which
      (#poke SDL_Event, adevice.iscapture) ptr iscapture
    QuitEvent typ timestamp -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
    UserEvent typ timestamp wid code data1 data2 -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, user.windowID) ptr wid
      (#poke SDL_Event, user.code) ptr code
      (#poke SDL_Event, user.data1) ptr data1
      (#poke SDL_Event, user.data2) ptr data2
    SysWMEvent typ timestamp msg -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, syswm.msg) ptr msg
    TouchFingerEvent typ timestamp touchid fingerid x y dx dy pressure -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, tfinger.touchId) ptr touchid
      (#poke SDL_Event, tfinger.fingerId) ptr fingerid
      (#poke SDL_Event, tfinger.x) ptr x
      (#poke SDL_Event, tfinger.y) ptr y
      (#poke SDL_Event, tfinger.dx) ptr dx
      (#poke SDL_Event, tfinger.dy) ptr dy
      (#poke SDL_Event, tfinger.pressure) ptr pressure
    MultiGestureEvent typ timestamp touchid dtheta ddist x y numfingers -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, mgesture.touchId) ptr touchid
      (#poke SDL_Event, mgesture.dTheta) ptr dtheta
      (#poke SDL_Event, mgesture.dDist) ptr ddist
      (#poke SDL_Event, mgesture.x) ptr x
      (#poke SDL_Event, mgesture.y) ptr y
      (#poke SDL_Event, mgesture.numFingers) ptr numfingers
    DollarGestureEvent typ timestamp touchid gestureid numfingers err x y -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, dgesture.touchId) ptr touchid
      (#poke SDL_Event, dgesture.gestureId) ptr gestureid
      (#poke SDL_Event, dgesture.numFingers) ptr numfingers
      (#poke SDL_Event, dgesture.error) ptr err
      (#poke SDL_Event, dgesture.x) ptr x
      (#poke SDL_Event, dgesture.y) ptr y
    ClipboardUpdateEvent typ timestamp -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
    DropEvent typ timestamp file -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp
      (#poke SDL_Event, drop.file) ptr file
    UnknownEvent typ timestamp -> do
      (#poke SDL_Event, common.type) ptr typ
      (#poke SDL_Event, common.timestamp) ptr timestamp

data Finger = Finger
  { fingerID :: !FingerID
  , fingerX :: !CFloat
  , fingerY :: !CFloat
  , fingerPressure :: !CFloat
  } deriving (Eq, Show, Typeable)

instance Storable Finger where
  sizeOf _ = (#size SDL_Finger)
  alignment = sizeOf
  peek ptr = do
    fingerId <- (#peek SDL_Finger, id) ptr
    x <- (#peek SDL_Finger, x) ptr
    y <- (#peek SDL_Finger, y) ptr
    pressure <- (#peek SDL_Finger, pressure) ptr
    return $! Finger fingerId x y pressure
  poke ptr (Finger fingerId x y pressure) = do
    (#poke SDL_Finger, id) ptr fingerId
    (#poke SDL_Finger, x) ptr x
    (#poke SDL_Finger, y) ptr y
    (#poke SDL_Finger, pressure) ptr pressure

data GameControllerButtonBind
  = GameControllerButtonBindNone
  | GameControllerButtonBindButton
    { gameControllerButtonBindButton :: !CInt
    }
  | GameControllerButtonBindAxis
    { gameControllerButtonBindAxis :: !CInt
    }
  | GameControllerButtonBindHat
    { gameControllerButtonBindHat :: !CInt
    , gameControllerButtonBindHatMask :: !CInt
    }
  deriving (Eq, Show, Typeable)

instance Storable GameControllerButtonBind where
  sizeOf _ = (#size SDL_GameControllerButtonBind)
  alignment = sizeOf
  peek ptr = do
    bind_type <- (#peek SDL_GameControllerButtonBind, bindType) ptr
    case bind_type :: (#type SDL_GameControllerBindType) of
      (#const SDL_CONTROLLER_BINDTYPE_NONE) -> do
        return $! GameControllerButtonBindNone
      (#const SDL_CONTROLLER_BINDTYPE_BUTTON) -> do
        button <- (#peek SDL_GameControllerButtonBind, value.button) ptr
        return $! GameControllerButtonBindButton button
      (#const SDL_CONTROLLER_BINDTYPE_AXIS) -> do
        axis <- (#peek SDL_GameControllerButtonBind, value.axis) ptr
        return $! GameControllerButtonBindAxis axis
      (#const SDL_CONTROLLER_BINDTYPE_HAT) -> do
        hat <- (#peek SDL_GameControllerButtonBind, value.hat.hat) ptr
        hat_mask <- (#peek SDL_GameControllerButtonBind, value.hat.hat_mask) ptr
        return $! GameControllerButtonBindHat hat hat_mask
      _ -> error $ "Unknown type " ++ show bind_type ++ " for SDL_GameControllerButtonBind"
  poke ptr bind = case bind of
    GameControllerButtonBindNone -> do
      (#poke SDL_GameControllerButtonBind, bindType) ptr ((#const SDL_CONTROLLER_BINDTYPE_NONE) :: (#type SDL_GameControllerBindType))
    GameControllerButtonBindButton button -> do
      (#poke SDL_GameControllerButtonBind, bindType) ptr ((#const SDL_CONTROLLER_BINDTYPE_BUTTON) :: (#type SDL_GameControllerBindType))
      (#poke SDL_GameControllerButtonBind, value.button) ptr button
    GameControllerButtonBindAxis axis -> do
      (#poke SDL_GameControllerButtonBind, bindType) ptr ((#const SDL_CONTROLLER_BINDTYPE_AXIS) :: (#type SDL_GameControllerBindType))
      (#poke SDL_GameControllerButtonBind, value.axis) ptr axis
    GameControllerButtonBindHat hat hat_mask -> do
      (#poke SDL_GameControllerButtonBind, bindType) ptr ((#const SDL_CONTROLLER_BINDTYPE_HAT) :: (#type SDL_GameControllerBindType))
      (#poke SDL_GameControllerButtonBind, value.hat.hat) ptr hat
      (#poke SDL_GameControllerButtonBind, value.hat.hat_mask) ptr hat_mask

data HapticDirection = HapticDirection
  { hapticDirectionType :: !Word8
  , hapticDirectionX :: !Int32
  , hapticDirectionY :: !Int32
  , hapticDirectionZ :: !Int32
  } deriving (Eq, Show, Typeable)

instance Storable HapticDirection where
  sizeOf _ = (#size SDL_HapticDirection)
  alignment = sizeOf
  peek ptr = do
    typ <- (#peek SDL_HapticDirection, type) ptr
    x <- (#peek SDL_HapticDirection, dir[0]) ptr
    y <- (#peek SDL_HapticDirection, dir[1]) ptr
    z <- (#peek SDL_HapticDirection, dir[2]) ptr
    return $! HapticDirection typ x y z
  poke ptr (HapticDirection typ x y z) = do
    (#poke SDL_HapticDirection, type) ptr typ
    (#poke SDL_HapticDirection, dir[0]) ptr x
    (#poke SDL_HapticDirection, dir[1]) ptr y
    (#poke SDL_HapticDirection, dir[2]) ptr z

data HapticEffect
  = HapticConstant
    { hapticEffectType :: !Word16
    , hapticConstantDirection :: !HapticDirection
    , hapticConstantLength :: !Word32
    , hapticConstantDelay :: !Word16
    , hapticConstantButton :: !Word16
    , hapticConstantInterval :: !Word16
    , hapticConstantLevel :: !Int16
    , hapticConstantAttackLength :: !Word16
    , hapticConstantAttackLevel :: !Word16
    , hapticConstantFadeLength :: !Word16
    , hapticConstantFadeLevel :: !Word16
    }
  | HapticPeriodic
    { hapticEffectType :: !Word16
    , hapticPeriodicDirection :: !HapticDirection
    , hapticPeriodicLength :: !Word32
    , hapticPeriodicDelay :: !Word16
    , hapticPeriodicButton :: !Word16
    , hapticPeriodicInterval :: !Word16
    , hapticPeriodicPeriod :: !Word16
    , hapticPeriodicMagnitude :: !Int16
    , hapticPeriodicOffset :: !Int16
    , hapticPeriodicPhase :: !Word16
    , hapticPeriodicAttackLength :: !Word16
    , hapticPeriodicAttackLevel :: !Word16
    , hapticPeriodicFadeLength :: !Word16
    , hapticPeriodicFadeLevel :: !Word16
    }
  | HapticCondition
    { hapticEffectType :: !Word16
    , hapticConditionLength :: !Word32
    , hapticConditionDelay :: !Word16
    , hapticConditionButton :: !Word16
    , hapticConditionInterval :: !Word16
    , hapticConditionRightSat :: ![Word16]
    , hapticConditionLeftSat :: ![Word16]
    , hapticConditionRightCoeff :: ![Int16]
    , hapticConditionLeftCoeff :: ![Int16]
    , hapticConditionDeadband :: ![Word16]
    , hapticConditionCenter :: ![Int16]
    }
  | HapticRamp
    { hapticEffectType :: !Word16
    , hapticRampDirection :: !HapticDirection
    , hapticRampLength :: !Word32
    , hapticRampDelay :: !Word16
    , hapticRampButton :: !Word16
    , hapticRampInterval :: !Word16
    , hapticRampStart :: !Int16
    , hapticRampEnd :: !Int16
    , hapticRampAttackLength :: !Word16
    , hapticRampAttackLevel :: !Word16
    , hapticRampFadeLength :: !Word16
    , hapticRampFadeLevel :: !Word16
    }
  | HapticLeftRight
    { hapticEffectType :: !Word16
    , hapticLeftRightLength :: !Word32
    , hapticLeftRightLargeMagnitude :: !Word16
    , hapticLeftRightSmallMagnitude :: !Word16
    }
  | HapticCustom
    { hapticEffectType :: !Word16
    , hapticCustomDirection :: !HapticDirection
    , hapticCustomLength :: !Word32
    , hapticCustomDelay :: !Word16
    , hapticCustomButton :: !Word16
    , hapticCustomInterval :: !Word16
    , hapticCustomChannels :: !Word8
    , hapticCustomPeriod :: !Word16
    , hapticCustomSamples :: !Word16
    , hapticCustomData :: !(Ptr Word16)
    , hapticCustomAttackLength :: !Word16
    , hapticCustomAttackLevel :: !Word16
    , hapticCustomFadeLength :: !Word16
    , hapticCustomFadeLevel :: !Word16
    }
  deriving (Eq, Show, Typeable)

instance Storable HapticEffect where
  sizeOf _ = (#size SDL_HapticEffect)
  alignment = sizeOf
  peek ptr = do
    typ <- (#peek SDL_HapticEffect, type) ptr
    case typ of
      (#const SDL_HAPTIC_CONSTANT) -> do
        direction <- (#peek SDL_HapticEffect, constant.direction) ptr
        len <- (#peek SDL_HapticEffect, constant.length) ptr
        delay <- (#peek SDL_HapticEffect, constant.delay) ptr
        button <- (#peek SDL_HapticEffect, constant.button) ptr
        interval <- (#peek SDL_HapticEffect, constant.interval) ptr
        level <- (#peek SDL_HapticEffect, constant.level) ptr
        attack_length <- (#peek SDL_HapticEffect, constant.attack_length) ptr
        attack_level <- (#peek SDL_HapticEffect, constant.attack_level) ptr
        fade_length <- (#peek SDL_HapticEffect, constant.fade_length) ptr
        fade_level <- (#peek SDL_HapticEffect, constant.fade_level) ptr
        return $! HapticConstant typ direction len delay button interval level attack_length attack_level fade_length fade_level

      (#const SDL_HAPTIC_SINE) -> hapticperiodic $ HapticPeriodic typ
      (#const SDL_HAPTIC_TRIANGLE) -> hapticperiodic $ HapticPeriodic typ
      (#const SDL_HAPTIC_SAWTOOTHUP) -> hapticperiodic $ HapticPeriodic typ
      (#const SDL_HAPTIC_SAWTOOTHDOWN) -> hapticperiodic $ HapticPeriodic typ

      (#const SDL_HAPTIC_RAMP) -> do
        direction <- (#peek SDL_HapticEffect, ramp.direction) ptr
        len <- (#peek SDL_HapticEffect, ramp.length) ptr
        delay <- (#peek SDL_HapticEffect, ramp.delay) ptr
        button <- (#peek SDL_HapticEffect, ramp.button) ptr
        interval <- (#peek SDL_HapticEffect, ramp.interval) ptr
        start <- (#peek SDL_HapticEffect, ramp.start) ptr
        end <- (#peek SDL_HapticEffect, ramp.end) ptr
        attack_length <- (#peek SDL_HapticEffect, ramp.attack_length) ptr
        attack_level <- (#peek SDL_HapticEffect, ramp.attack_level) ptr
        fade_length <- (#peek SDL_HapticEffect, ramp.fade_length) ptr
        fade_level <- (#peek SDL_HapticEffect, ramp.fade_level) ptr
        return $! HapticRamp typ direction len delay button interval start end attack_length attack_level fade_length fade_level

      (#const SDL_HAPTIC_SPRING) -> hapticcondition $ HapticCondition typ
      (#const SDL_HAPTIC_DAMPER) -> hapticcondition $ HapticCondition typ
      (#const SDL_HAPTIC_INERTIA) -> hapticcondition $ HapticCondition typ
      (#const SDL_HAPTIC_FRICTION) -> hapticcondition $ HapticCondition typ

      (#const SDL_HAPTIC_LEFTRIGHT) -> do
        len <- (#peek SDL_HapticEffect, leftright.length) ptr
        large_magnitude <- (#peek SDL_HapticEffect, leftright.large_magnitude) ptr
        small_magnitude <- (#peek SDL_HapticEffect, leftright.small_magnitude) ptr
        return $! HapticLeftRight typ len large_magnitude small_magnitude

      (#const SDL_HAPTIC_CUSTOM) -> do
        direction <- (#peek SDL_HapticEffect, custom.direction) ptr
        len <- (#peek SDL_HapticEffect, custom.length) ptr
        delay <- (#peek SDL_HapticEffect, custom.delay) ptr
        button <- (#peek SDL_HapticEffect, custom.button) ptr
        interval <- (#peek SDL_HapticEffect, custom.interval) ptr
        channels <- (#peek SDL_HapticEffect, custom.channels) ptr
        period <- (#peek SDL_HapticEffect, custom.period) ptr
        samples <- (#peek SDL_HapticEffect, custom.samples) ptr
        datum <- (#peek SDL_HapticEffect, custom.data) ptr
        attack_length <- (#peek SDL_HapticEffect, custom.attack_length) ptr
        attack_level <- (#peek SDL_HapticEffect, custom.attack_level) ptr
        fade_length <- (#peek SDL_HapticEffect, custom.fade_length) ptr
        fade_level <- (#peek SDL_HapticEffect, custom.fade_level) ptr
        return $! HapticCustom typ direction len delay button interval channels period samples datum attack_length attack_level fade_length fade_level
      _ -> error $ "Unknown type " ++ show typ ++ " for SDL_HapticEffect"
    where
    hapticperiodic f = do
      direction <- (#peek SDL_HapticEffect, periodic.direction) ptr
      len <- (#peek SDL_HapticEffect, periodic.length) ptr
      delay <- (#peek SDL_HapticEffect, periodic.delay) ptr
      button <- (#peek SDL_HapticEffect, periodic.button) ptr
      interval <- (#peek SDL_HapticEffect, periodic.interval) ptr
      period <- (#peek SDL_HapticEffect, periodic.period) ptr
      magnitude <- (#peek SDL_HapticEffect, periodic.magnitude) ptr
      offset <- (#peek SDL_HapticEffect, periodic.offset) ptr
      phase <- (#peek SDL_HapticEffect, periodic.phase) ptr
      attack_length <- (#peek SDL_HapticEffect, periodic.attack_length) ptr
      attack_level <- (#peek SDL_HapticEffect, periodic.attack_level) ptr
      fade_length <- (#peek SDL_HapticEffect, periodic.fade_length) ptr
      fade_level <- (#peek SDL_HapticEffect, periodic.fade_level) ptr
      return $! f direction len delay button interval period magnitude offset phase attack_length attack_level fade_length fade_level

    hapticcondition f = do
      len <- (#peek SDL_HapticEffect, condition.length) ptr
      delay <- (#peek SDL_HapticEffect, condition.delay) ptr
      button <- (#peek SDL_HapticEffect, condition.button) ptr
      interval <- (#peek SDL_HapticEffect, condition.interval) ptr
      right_sat <- peekArray 3 $ (#ptr SDL_HapticEffect, condition.right_sat) ptr
      left_sat <- peekArray 3 $ (#ptr SDL_HapticEffect, condition.left_sat) ptr
      right_coeff <- peekArray 3 $ (#ptr SDL_HapticEffect, condition.right_coeff) ptr
      left_coeff <- peekArray 3 $ (#ptr SDL_HapticEffect, condition.left_coeff) ptr
      deadband <- peekArray 3 $ (#ptr SDL_HapticEffect, condition.deadband) ptr
      center <- peekArray 3 $ (#ptr SDL_HapticEffect, condition.center) ptr
      return $! f len delay button interval right_sat left_sat right_coeff left_coeff deadband center
  poke ptr event = case event of
    HapticConstant typ direction len delay button interval level attack_length attack_level fade_length fade_level -> do
      (#poke SDL_HapticEffect, type) ptr typ
      (#poke SDL_HapticEffect, constant.direction) ptr direction
      (#poke SDL_HapticEffect, constant.length) ptr len
      (#poke SDL_HapticEffect, constant.delay) ptr delay
      (#poke SDL_HapticEffect, constant.button) ptr button
      (#poke SDL_HapticEffect, constant.interval) ptr interval
      (#poke SDL_HapticEffect, constant.level) ptr level
      (#poke SDL_HapticEffect, constant.attack_length) ptr attack_length
      (#poke SDL_HapticEffect, constant.attack_level) ptr attack_level
      (#poke SDL_HapticEffect, constant.fade_length) ptr fade_length
      (#poke SDL_HapticEffect, constant.fade_level) ptr fade_level
    HapticPeriodic typ direction len delay button interval period magnitude offset phase attack_length attack_level fade_length fade_level -> do
      (#poke SDL_HapticEffect, type) ptr typ
      (#poke SDL_HapticEffect, periodic.direction) ptr direction
      (#poke SDL_HapticEffect, periodic.length) ptr len
      (#poke SDL_HapticEffect, periodic.delay) ptr delay
      (#poke SDL_HapticEffect, periodic.button) ptr button
      (#poke SDL_HapticEffect, periodic.interval) ptr interval
      (#poke SDL_HapticEffect, periodic.period) ptr period
      (#poke SDL_HapticEffect, periodic.magnitude) ptr magnitude
      (#poke SDL_HapticEffect, periodic.offset) ptr offset
      (#poke SDL_HapticEffect, periodic.phase) ptr phase
      (#poke SDL_HapticEffect, periodic.attack_length) ptr attack_length
      (#poke SDL_HapticEffect, periodic.attack_level) ptr attack_level
      (#poke SDL_HapticEffect, periodic.fade_length) ptr fade_length
      (#poke SDL_HapticEffect, periodic.fade_level) ptr fade_level
    HapticCondition typ len delay button interval right_sat left_sat right_coeff left_coeff deadband center -> do
      (#poke SDL_HapticEffect, type) ptr typ
      (#poke SDL_HapticEffect, condition.length) ptr len
      (#poke SDL_HapticEffect, condition.delay) ptr delay
      (#poke SDL_HapticEffect, condition.button) ptr button
      (#poke SDL_HapticEffect, condition.interval) ptr interval
      pokeArray ((#ptr SDL_HapticEffect, condition.right_sat) ptr) right_sat
      pokeArray ((#ptr SDL_HapticEffect, condition.left_sat) ptr) left_sat
      pokeArray ((#ptr SDL_HapticEffect, condition.right_coeff) ptr) right_coeff
      pokeArray ((#ptr SDL_HapticEffect, condition.left_coeff) ptr) left_coeff
      pokeArray ((#ptr SDL_HapticEffect, condition.deadband) ptr) deadband
      pokeArray ((#ptr SDL_HapticEffect, condition.center) ptr) center
    HapticRamp typ direction len delay button interval start end attack_length attack_level fade_length fade_level -> do
      (#poke SDL_HapticEffect, type) ptr typ
      (#poke SDL_HapticEffect, ramp.direction) ptr direction
      (#poke SDL_HapticEffect, ramp.length) ptr len
      (#poke SDL_HapticEffect, ramp.delay) ptr delay
      (#poke SDL_HapticEffect, ramp.button) ptr button
      (#poke SDL_HapticEffect, ramp.interval) ptr interval
      (#poke SDL_HapticEffect, ramp.start) ptr start
      (#poke SDL_HapticEffect, ramp.end) ptr end
      (#poke SDL_HapticEffect, ramp.attack_length) ptr attack_length
      (#poke SDL_HapticEffect, ramp.attack_level) ptr attack_level
      (#poke SDL_HapticEffect, ramp.fade_length) ptr fade_length
      (#poke SDL_HapticEffect, ramp.fade_level) ptr fade_level
    HapticLeftRight typ len large_magnitude small_magnitude -> do
      (#poke SDL_HapticEffect, type) ptr typ
      (#poke SDL_HapticEffect, leftright.length) ptr len
      (#poke SDL_HapticEffect, leftright.large_magnitude) ptr large_magnitude
      (#poke SDL_HapticEffect, leftright.small_magnitude) ptr small_magnitude
    HapticCustom typ direction len delay button interval channels period samples datum attack_length attack_level fade_length fade_level -> do
      (#poke SDL_HapticEffect, type) ptr typ
      (#poke SDL_HapticEffect, custom.direction) ptr direction
      (#poke SDL_HapticEffect, custom.length) ptr len
      (#poke SDL_HapticEffect, custom.delay) ptr delay
      (#poke SDL_HapticEffect, custom.button) ptr button
      (#poke SDL_HapticEffect, custom.interval) ptr interval
      (#poke SDL_HapticEffect, custom.channels) ptr channels
      (#poke SDL_HapticEffect, custom.period) ptr period
      (#poke SDL_HapticEffect, custom.samples) ptr samples
      (#poke SDL_HapticEffect, custom.data) ptr datum
      (#poke SDL_HapticEffect, custom.attack_length) ptr attack_length
      (#poke SDL_HapticEffect, custom.attack_level) ptr attack_level
      (#poke SDL_HapticEffect, custom.fade_length) ptr fade_length
      (#poke SDL_HapticEffect, custom.fade_level) ptr fade_level

data JoystickGUID = JoystickGUID
  { joystickGUID :: ![Word8]
  } deriving (Eq, Show, Typeable)

instance Storable JoystickGUID where
  sizeOf _ = (#size SDL_JoystickGUID)
  alignment = sizeOf
  peek ptr = do
    guid <- peekArray 16 $ (#ptr SDL_JoystickGUID, data) ptr
    return $! JoystickGUID guid
  poke ptr (JoystickGUID guid) =
    pokeArray ((#ptr SDL_JoystickGUID, data) ptr) guid

data Keysym = Keysym
  { keysymScancode :: !Scancode
  , keysymKeycode :: !Keycode
  , keysymMod :: !Word16
  } deriving (Eq, Show, Typeable)

instance Storable Keysym where
  sizeOf _ = (#size SDL_Keysym)
  alignment = sizeOf
  peek ptr = do
    scancode <- (#peek SDL_Keysym, scancode) ptr
    sym <- (#peek SDL_Keysym, sym) ptr
    mod' <- (#peek SDL_Keysym, mod) ptr
    return $! Keysym scancode sym mod'
  poke ptr (Keysym scancode sym mod') = do
    (#poke SDL_Keysym, scancode) ptr scancode
    (#poke SDL_Keysym, sym) ptr sym
    (#poke SDL_Keysym, mod) ptr mod'

data MessageBoxButtonData = MessageBoxButtonData
  { messageBoxButtonDataFlags :: !Word32
  , messageBoxButtonButtonID :: !CInt
  , messageBoxButtonText :: !CString
  } deriving (Eq, Show, Typeable)

instance Storable MessageBoxButtonData where
  sizeOf _ = (#size SDL_MessageBoxButtonData)
  alignment = sizeOf
  peek ptr = do
    flags <- (#peek SDL_MessageBoxButtonData, flags) ptr
    buttonid <- (#peek SDL_MessageBoxButtonData, buttonid) ptr
    text <- (#peek SDL_MessageBoxButtonData, text) ptr
    return $! MessageBoxButtonData flags buttonid text
  poke ptr (MessageBoxButtonData flags buttonid text) = do
    (#poke SDL_MessageBoxButtonData, flags) ptr flags
    (#poke SDL_MessageBoxButtonData, buttonid) ptr buttonid
    (#poke SDL_MessageBoxButtonData, text) ptr text

data MessageBoxColor = MessageBoxColor
  { messageBoxColorR :: !Word8
  , messageBoxColorG :: !Word8
  , messageBoxColorB :: !Word8
  } deriving (Eq, Show, Typeable)

instance Storable MessageBoxColor where
  sizeOf _ = (#size SDL_MessageBoxColor)
  alignment = sizeOf
  peek ptr = do
    r <- (#peek SDL_MessageBoxColor, r) ptr
    g <- (#peek SDL_MessageBoxColor, g) ptr
    b <- (#peek SDL_MessageBoxColor, b) ptr
    return $! MessageBoxColor r g b
  poke ptr (MessageBoxColor r g b) = do
    (#poke SDL_MessageBoxColor, r) ptr r
    (#poke SDL_MessageBoxColor, g) ptr g
    (#poke SDL_MessageBoxColor, b) ptr b

data MessageBoxColorScheme = MessageBoxColorScheme
  { messageBoxColorSchemeColorBackground :: !MessageBoxColor
  , messageBoxColorSchemeColorText :: !MessageBoxColor
  , messageBoxColorSchemeColorButtonBorder :: !MessageBoxColor
  , messageBoxColorSchemeColorButtonBackground :: !MessageBoxColor
  , messageBoxColorSchemeColorButtonSelected :: !MessageBoxColor
  } deriving (Eq, Show, Typeable)

instance Storable MessageBoxColorScheme where
  sizeOf _ = (#size SDL_MessageBoxColorScheme)
  alignment = sizeOf
  peek ptr = do
    background <- (#peek SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BACKGROUND]) ptr
    text <- (#peek SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_TEXT]) ptr
    button_border <- (#peek SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BUTTON_BORDER]) ptr
    button_background <- (#peek SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND]) ptr
    button_selected <- (#peek SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED]) ptr
    return $! MessageBoxColorScheme background text button_border button_background button_selected
  poke ptr (MessageBoxColorScheme background text button_border button_background button_selected) = do
    (#poke SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BACKGROUND]) ptr background
    (#poke SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_TEXT]) ptr text
    (#poke SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BUTTON_BORDER]) ptr button_border
    (#poke SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND]) ptr button_background
    (#poke SDL_MessageBoxColorScheme, colors[SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED]) ptr button_selected

data MessageBoxData = MessageBoxData
  { messageBoxDataFlags :: !Word32
  , messageBoxDataWindow :: !Window
  , messageBoxDataTitle :: !CString
  , messageBoxDataMessage :: !CString
  , messageBoxDataNumButtons :: !CInt
  , messageBoxDataButtons :: !(Ptr MessageBoxButtonData)
  , messageBoxDataColorScheme :: !(Ptr MessageBoxColorScheme)
  } deriving (Eq, Show, Typeable)

instance Storable MessageBoxData where
  sizeOf _ = (#size SDL_MessageBoxData)
  alignment = sizeOf
  peek ptr = do
    flags <- (#peek SDL_MessageBoxData, flags) ptr
    window <- (#peek SDL_MessageBoxData, window) ptr
    title <- (#peek SDL_MessageBoxData, title) ptr
    message <- (#peek SDL_MessageBoxData, message) ptr
    numbuttons <- (#peek SDL_MessageBoxData, numbuttons) ptr
    buttons <- (#peek SDL_MessageBoxData, buttons) ptr
    color_scheme <- (#peek SDL_MessageBoxData, colorScheme) ptr
    return $! MessageBoxData flags window title message numbuttons buttons color_scheme
  poke ptr (MessageBoxData flags window title message numbuttons buttons color_scheme) = do
    (#poke SDL_MessageBoxData, flags) ptr flags
    (#poke SDL_MessageBoxData, window) ptr window
    (#poke SDL_MessageBoxData, title) ptr title
    (#poke SDL_MessageBoxData, message) ptr message
    (#poke SDL_MessageBoxData, numbuttons) ptr numbuttons
    (#poke SDL_MessageBoxData, buttons) ptr buttons
    (#poke SDL_MessageBoxData, colorScheme) ptr color_scheme

data Palette = Palette
  { paletteNColors :: !CInt
  , paletteColors :: !(Ptr Color)
  } deriving (Eq, Show, Typeable)

instance Storable Palette where
  sizeOf _ = (#size SDL_Palette)
  alignment = sizeOf
  peek ptr = do
    ncolors <- (#peek SDL_Palette, ncolors) ptr
    colors <- (#peek SDL_Palette, colors) ptr
    return $! Palette ncolors colors
  poke ptr (Palette ncolors colors) = do
    (#poke SDL_Palette, ncolors) ptr ncolors
    (#poke SDL_Palette, colors) ptr colors

data PixelFormat = PixelFormat
  { pixelFormatFormat :: !Word32
  , pixelFormatPalette :: !(Ptr Palette)
  , pixelFormatBitsPerPixel :: !Word8
  , pixelFormatBytesPerPixel :: !Word8
  , pixelFormatRMask :: !Word32
  , pixelFormatGMask :: !Word32
  , pixelFormatBMask :: !Word32
  , pixelFormatAMask :: !Word32
  } deriving (Eq, Show, Typeable)

instance Storable PixelFormat where
  sizeOf _ = (#size SDL_PixelFormat)
  alignment = sizeOf
  peek ptr = do
    format <- (#peek SDL_PixelFormat, format) ptr
    palette <- (#peek SDL_PixelFormat, palette) ptr
    bits_per_pixel <- (#peek SDL_PixelFormat, BitsPerPixel) ptr
    bytes_per_pixel <- (#peek SDL_PixelFormat, BytesPerPixel) ptr
    rmask <- (#peek SDL_PixelFormat, Rmask) ptr
    gmask <- (#peek SDL_PixelFormat, Gmask) ptr
    bmask <- (#peek SDL_PixelFormat, Bmask) ptr
    amask <- (#peek SDL_PixelFormat, Amask) ptr
    return $! PixelFormat format palette bits_per_pixel bytes_per_pixel rmask gmask bmask amask
  poke ptr (PixelFormat format palette bits_per_pixel bytes_per_pixel rmask gmask bmask amask) = do
    (#poke SDL_PixelFormat, format) ptr format
    (#poke SDL_PixelFormat, palette) ptr palette
    (#poke SDL_PixelFormat, BitsPerPixel) ptr bits_per_pixel
    (#poke SDL_PixelFormat, BytesPerPixel) ptr bytes_per_pixel
    (#poke SDL_PixelFormat, Rmask) ptr rmask
    (#poke SDL_PixelFormat, Gmask) ptr gmask
    (#poke SDL_PixelFormat, Bmask) ptr bmask
    (#poke SDL_PixelFormat, Amask) ptr amask

data Point = Point
  { pointX :: !CInt
  , pointY :: !CInt
  } deriving (Eq, Show, Typeable)

instance Storable Point where
  sizeOf _ = (#size SDL_Point)
  alignment = sizeOf
  peek ptr = do
    x <- (#peek SDL_Point, x) ptr
    y <- (#peek SDL_Point, y) ptr
    return $! Point x y
  poke ptr (Point x y) = do
    (#poke SDL_Point, x) ptr x
    (#poke SDL_Point, y) ptr y

data FPoint = FPoint
  { fPointX :: !CFloat
  , fPointY :: !CFloat
  } deriving (Eq, Show, Typeable)

instance Storable FPoint where
  sizeOf _ = (#size SDL_FPoint)
  alignment = sizeOf
  peek ptr = do
    x <- (#peek SDL_FPoint, x) ptr
    y <- (#peek SDL_FPoint, y) ptr
    return $! FPoint x y
  poke ptr (FPoint x y) = do
    (#poke SDL_FPoint, x) ptr x
    (#poke SDL_FPoint, y) ptr y

data Rect = Rect
  { rectX :: !CInt
  , rectY :: !CInt
  , rectW :: !CInt
  , rectH :: !CInt
  } deriving (Eq, Show, Typeable)

instance Storable Rect where
  sizeOf _ = (#size SDL_Rect)
  alignment = sizeOf
  peek ptr = do
    x <- (#peek SDL_Rect, x) ptr
    y <- (#peek SDL_Rect, y) ptr
    w <- (#peek SDL_Rect, w) ptr
    h <- (#peek SDL_Rect, h) ptr
    return $! Rect x y w h
  poke ptr (Rect x y w h) = do
    (#poke SDL_Rect, x) ptr x
    (#poke SDL_Rect, y) ptr y
    (#poke SDL_Rect, w) ptr w
    (#poke SDL_Rect, h) ptr h

data FRect = FRect
  { fRectX :: !CFloat
  , fRectY :: !CFloat
  , fRectW :: !CFloat
  , fRectH :: !CFloat
  } deriving (Eq, Show, Typeable)

instance Storable FRect where
  sizeOf _ = (#size SDL_FRect)
  alignment = sizeOf
  peek ptr = do
    x <- (#peek SDL_FRect, x) ptr
    y <- (#peek SDL_FRect, y) ptr
    w <- (#peek SDL_FRect, w) ptr
    h <- (#peek SDL_FRect, h) ptr
    return $! FRect x y w h
  poke ptr (FRect x y w h) = do
    (#poke SDL_FRect, x) ptr x
    (#poke SDL_FRect, y) ptr y
    (#poke SDL_FRect, w) ptr w
    (#poke SDL_FRect, h) ptr h

data RendererInfo = RendererInfo
  { rendererInfoName :: !CString
  , rendererInfoFlags :: !Word32
  , rendererInfoNumTextureFormats :: !Word32
  , rendererInfoTextureFormats :: ![Word32]
  , rendererInfoMaxTextureWidth :: !CInt
  , rendererInfoMaxTextureHeight :: !CInt
  } deriving (Eq, Show, Typeable)

instance Storable RendererInfo where
  sizeOf _ = (#size SDL_RendererInfo)
  alignment = sizeOf
  peek ptr = do
    name <- (#peek SDL_RendererInfo, name) ptr
    flags <- (#peek SDL_RendererInfo, flags) ptr
    num_texture_formats <- (#peek SDL_RendererInfo, num_texture_formats) ptr
    texture_formats <- peekArray 16 $ (#ptr SDL_RendererInfo, texture_formats) ptr
    max_texture_width <- (#peek SDL_RendererInfo, max_texture_width) ptr
    max_texture_height <- (#peek SDL_RendererInfo, max_texture_height) ptr
    return $! RendererInfo name flags num_texture_formats texture_formats max_texture_width max_texture_height
  poke ptr (RendererInfo name flags num_texture_formats texture_formats max_texture_width max_texture_height) = do
    (#poke SDL_RendererInfo, name) ptr name
    (#poke SDL_RendererInfo, flags) ptr flags
    (#poke SDL_RendererInfo, num_texture_formats) ptr num_texture_formats
    pokeArray ((#ptr SDL_RendererInfo, texture_formats) ptr) texture_formats
    (#poke SDL_RendererInfo, max_texture_width) ptr max_texture_width
    (#poke SDL_RendererInfo, max_texture_height) ptr max_texture_height

data RWops = RWops
  { rwopsSize :: !(FunPtr (Ptr RWops -> IO Int64))
  , rwopsSeek :: !(FunPtr (Ptr RWops -> Int64 -> CInt -> IO Int64))
  , rwopsRead :: !(FunPtr (Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize))
  , rwopsWrite :: !(FunPtr (Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize))
  , rwopsClose :: !(FunPtr (Ptr RWops -> IO CInt))
  , rwopsType :: !Word32
  } deriving (Eq, Show, Typeable)

instance Storable RWops where
  sizeOf _ = (#size SDL_RWops)
  alignment = sizeOf
  peek ptr = do
    size <- (#peek SDL_RWops, size) ptr
    seek <- (#peek SDL_RWops, seek) ptr
    read' <- (#peek SDL_RWops, read) ptr
    write <- (#peek SDL_RWops, write) ptr
    close <- (#peek SDL_RWops, close) ptr
    typ <- (#peek SDL_RWops, type) ptr
    return $! RWops size seek read' write close typ
  poke ptr (RWops size seek read' write close typ) = do
    (#poke SDL_RWops, size) ptr size
    (#poke SDL_RWops, seek) ptr seek
    (#poke SDL_RWops, read) ptr read'
    (#poke SDL_RWops, write) ptr write
    (#poke SDL_RWops, close) ptr close
    (#poke SDL_RWops, type) ptr typ

data Surface = Surface
  { surfaceFormat :: !(Ptr PixelFormat)
  , surfaceW :: !CInt
  , surfaceH :: !CInt
  , surfacePixels :: !(Ptr ())
  , surfaceUserdata :: !(Ptr ())
  , surfaceClipRect :: !Rect
  , surfaceRefcount :: !CInt
  } deriving (Eq, Show, Typeable)

instance Storable Surface where
  sizeOf _ = (#size SDL_Surface)
  alignment = sizeOf
  peek ptr = do
    format <- (#peek SDL_Surface, format) ptr
    w <- (#peek SDL_Surface, w) ptr
    h <- (#peek SDL_Surface, h) ptr
    pixels <- (#peek SDL_Surface, pixels) ptr
    userdata <- (#peek SDL_Surface, userdata) ptr
    cliprect <- (#peek SDL_Surface, clip_rect) ptr
    refcount <- (#peek SDL_Surface, refcount) ptr
    return $! Surface format w h pixels userdata cliprect refcount
  poke ptr (Surface format w h pixels userdata cliprect refcount) = do
    (#poke SDL_Surface, format) ptr format
    (#poke SDL_Surface, w) ptr w
    (#poke SDL_Surface, h) ptr h
    (#poke SDL_Surface, pixels) ptr pixels
    (#poke SDL_Surface, userdata) ptr userdata
    (#poke SDL_Surface, clip_rect) ptr cliprect
    (#poke SDL_Surface, refcount) ptr refcount

data Version = Version
  { versionMajor :: !Word8
  , versionMinor :: !Word8
  , versionPatch :: !Word8
  } deriving (Eq, Show, Typeable)

instance Storable Version where
  sizeOf _ = (#size SDL_version)
  alignment = sizeOf
  peek ptr = do
    major <- (#peek SDL_version, major) ptr
    minor <- (#peek SDL_version, minor) ptr
    patch <- (#peek SDL_version, patch) ptr
    return $! Version major minor patch
  poke ptr (Version major minor patch) = do
    (#poke SDL_version, major) ptr major
    (#poke SDL_version, minor) ptr minor
    (#poke SDL_version, patch) ptr patch
