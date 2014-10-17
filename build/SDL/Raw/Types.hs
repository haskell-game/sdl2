{-# LINE 1 "src/SDL/Raw/Types.hsc" #-}
module SDL.Raw.Types (
{-# LINE 2 "src/SDL/Raw/Types.hsc" #-}
	-- * Type Aliases
	AudioCallback,
	AudioDeviceID,
	AudioFormat,
	Cond,
	Cursor,
	EventFilter,
	FingerID,
	GameController,
	GestureID,
	GLContext,
	Haptic,
	HintCallback,
	Joystick,
	JoystickID,
	Keycode,
	LogOutputFunction,
	Mutex,
	Renderer,
	Sem,
	SpinLock,
	SysWMinfo,
	SysWMmsg,
	Texture,
	Thread,
	ThreadFunction,
	ThreadID,
	TimerCallback,
	TimerID,
	TLSID,
	TouchID,
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
	Rect(..),
	RendererInfo(..),
	RWops(..),
	Surface(..),
	Version(..)
) where


{-# LINE 63 "src/SDL/Raw/Types.hsc" #-}

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import SDL.Raw.Enum

type AudioCallback = FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ())
type AudioDeviceID = Word32
type AudioFormat = Word16
type Cond = Ptr ()
type Cursor = Ptr ()
type EventFilter = FunPtr (Ptr () -> Ptr Event -> IO CInt)
type FingerID = Int64
type GameController = Ptr ()
type GestureID = Int64
type GLContext = Ptr ()
type Haptic = Ptr ()
type HintCallback = FunPtr (Ptr () -> CString -> CString -> CString -> IO ())
type Joystick = Ptr ()
type JoystickID = Int32
type Keycode = Int32
type LogOutputFunction = FunPtr (Ptr () -> CInt -> LogPriority -> CString -> IO ())
type Mutex = Ptr ()
type Renderer = Ptr ()
type Sem = Ptr ()
type SpinLock = CInt
type SysWMinfo = Ptr ()
type SysWMmsg = Ptr ()
type Texture = Ptr ()
type Thread = Ptr ()
type ThreadFunction = FunPtr (Ptr () -> IO CInt)
type ThreadID = CULong
type TimerCallback = FunPtr (Word32 -> Ptr () -> IO Word32)
type TimerID = CInt
type TLSID = CUInt
type TouchID = Int64
type Window = Ptr ()

data Atomic = Atomic {
              atomicValue :: CInt
            } deriving (Eq, Show)

instance Storable Atomic where
	sizeOf _ = ((4))
{-# LINE 111 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		value <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 114 "src/SDL/Raw/Types.hsc" #-}
		return $! Atomic value
	poke ptr (Atomic value) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr value
{-# LINE 117 "src/SDL/Raw/Types.hsc" #-}

data AudioCVT = AudioCVT {
                audioCVTNeeded :: CInt
              , audioCVTSrcFormat :: AudioFormat
              , audioCVTDstFormat :: AudioFormat
              , audioCVTRateIncr :: CDouble
              , audioCVTBuf :: Ptr Word8
              , audioCVTLen :: CInt
              , audioCVTLenCvt :: CInt
              , audioCVTLenMult :: CInt
              , audioCVTLenRatio :: CDouble
              } deriving (Eq, Show)

instance Storable AudioCVT where
	sizeOf _ = ((128))
{-# LINE 132 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		needed <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 135 "src/SDL/Raw/Types.hsc" #-}
		src_format <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 136 "src/SDL/Raw/Types.hsc" #-}
		dst_format <- ((\hsc_ptr -> peekByteOff hsc_ptr 6)) ptr
{-# LINE 137 "src/SDL/Raw/Types.hsc" #-}
		rate_incr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 138 "src/SDL/Raw/Types.hsc" #-}
		buf <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 139 "src/SDL/Raw/Types.hsc" #-}
		len <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 140 "src/SDL/Raw/Types.hsc" #-}
		len_cvt <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 141 "src/SDL/Raw/Types.hsc" #-}
		len_mult <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 142 "src/SDL/Raw/Types.hsc" #-}
		len_ratio <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 143 "src/SDL/Raw/Types.hsc" #-}
		return $! AudioCVT needed src_format dst_format rate_incr buf len len_cvt len_mult len_ratio
	poke ptr (AudioCVT needed src_format dst_format rate_incr buf len len_cvt len_mult len_ratio) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr needed
{-# LINE 146 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr src_format
{-# LINE 147 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 6)) ptr dst_format
{-# LINE 148 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr rate_incr
{-# LINE 149 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr buf
{-# LINE 150 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr len
{-# LINE 151 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr len_cvt
{-# LINE 152 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr len_mult
{-# LINE 153 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr len_ratio
{-# LINE 154 "src/SDL/Raw/Types.hsc" #-}

data AudioSpec = AudioSpec {
                 audioSpecFreq :: CInt
               , audioSpecFormat :: AudioFormat
               , audioSpecChannels :: Word8
               , audioSpecSilence :: Word8
               , audioSpecSamples :: Word16
               , audioSpecSize :: Word32
               , audioSpecCallback :: AudioCallback
               , audioSpecUserdata :: Ptr ()
               } deriving (Eq, Show)

instance Storable AudioSpec where
	sizeOf _ = ((32))
{-# LINE 168 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		freq <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 171 "src/SDL/Raw/Types.hsc" #-}
		format <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 172 "src/SDL/Raw/Types.hsc" #-}
		channels <- ((\hsc_ptr -> peekByteOff hsc_ptr 6)) ptr
{-# LINE 173 "src/SDL/Raw/Types.hsc" #-}
		silence <- ((\hsc_ptr -> peekByteOff hsc_ptr 7)) ptr
{-# LINE 174 "src/SDL/Raw/Types.hsc" #-}
		samples <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 175 "src/SDL/Raw/Types.hsc" #-}
		size <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 176 "src/SDL/Raw/Types.hsc" #-}
		callback <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 177 "src/SDL/Raw/Types.hsc" #-}
		userdata <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 178 "src/SDL/Raw/Types.hsc" #-}
		return $! AudioSpec freq format channels silence samples size callback userdata
	poke ptr (AudioSpec freq format channels silence samples size callback userdata) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr freq
{-# LINE 181 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr format
{-# LINE 182 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 6)) ptr channels
{-# LINE 183 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 7)) ptr silence
{-# LINE 184 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr samples
{-# LINE 185 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr size
{-# LINE 186 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr callback
{-# LINE 187 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr userdata
{-# LINE 188 "src/SDL/Raw/Types.hsc" #-}

data Color = Color {
             colorR :: Word8
           , colorG :: Word8
           , colorB :: Word8
           , colorA :: Word8
           } deriving (Eq, Show)

instance Storable Color where
	sizeOf _ = ((4))
{-# LINE 198 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		r <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 201 "src/SDL/Raw/Types.hsc" #-}
		g <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) ptr
{-# LINE 202 "src/SDL/Raw/Types.hsc" #-}
		b <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
{-# LINE 203 "src/SDL/Raw/Types.hsc" #-}
		a <- ((\hsc_ptr -> peekByteOff hsc_ptr 3)) ptr
{-# LINE 204 "src/SDL/Raw/Types.hsc" #-}
		return $! Color r g b a
	poke ptr (Color r g b a) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr r
{-# LINE 207 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 1)) ptr g
{-# LINE 208 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 2)) ptr b
{-# LINE 209 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 3)) ptr a
{-# LINE 210 "src/SDL/Raw/Types.hsc" #-}

data DisplayMode = DisplayMode {
                   displayModeFormat :: Word32
                 , displayModeW :: CInt
                 , displayModeH :: CInt
                 , displayModeRefreshRate :: CInt
                 , displayModeDriverData :: Ptr ()
                 } deriving (Eq, Show)

instance Storable DisplayMode where
	sizeOf _ = ((24))
{-# LINE 221 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		format <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 224 "src/SDL/Raw/Types.hsc" #-}
		w <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 225 "src/SDL/Raw/Types.hsc" #-}
		h <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 226 "src/SDL/Raw/Types.hsc" #-}
		refresh_rate <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 227 "src/SDL/Raw/Types.hsc" #-}
		driverdata <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 228 "src/SDL/Raw/Types.hsc" #-}
		return $! DisplayMode format w h refresh_rate driverdata
	poke ptr (DisplayMode format w h refresh_rate driverdata) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr format
{-# LINE 231 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr w
{-# LINE 232 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr h
{-# LINE 233 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr refresh_rate
{-# LINE 234 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr driverdata
{-# LINE 235 "src/SDL/Raw/Types.hsc" #-}

data Event = WindowEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , windowEventWindowID :: Word32
           , windowEventEvent :: Word8
           , windowEventData1 :: Int32
           , windowEventData2 :: Int32
           }
           | KeyboardEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , keyboardEventWindowID :: Word32
           , keyboardEventState :: Word8
           , keyboardEventRepeat :: Word8
           , keyboardEventKeysym :: Keysym
           }
           | TextEditingEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , textEditingEventWindowID :: Word32
           , textEditingEventText :: [CChar]
           , textEditingEventStart :: Int32
           , textEditingEventLength :: Int32
           }
           | TextInputEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , textInputEventWindowID :: Word32
           , textInputEventText :: [CChar]
           }
           | MouseMotionEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , mouseMotionEventWindowID :: Word32
           , mouseMotionEventWhich :: Word32
           , mouseMotionEventState :: Word32
           , mouseMotionEventX :: Int32
           , mouseMotionEventY :: Int32
           , mouseMotionEventXRel :: Int32
           , mouseMotionEventYRel :: Int32
           }
           | MouseButtonEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , mouseButtonEventWindowID :: Word32
           , mouseButtonEventWhich :: Word32
           , mouseButtonEventButton :: Word8
           , mouseButtonEventState :: Word8
           , mouseButtonEventClicks :: Word8
           , mouseButtonEventX :: Int32
           , mouseButtonEventY :: Int32
           }
           | MouseWheelEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , mouseWheelEventWindowID :: Word32
           , mouseWheelEventWhich :: Word32
           , mouseWheelEventX :: Int32
           , mouseWheelEventY :: Int32
           }
           | JoyAxisEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , joyAxisEventWhich :: JoystickID
           , joyAxisEventAxis :: Word8
           , joyAxisEventValue :: Int16
           }
           | JoyBallEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , joyBallEventWhich :: JoystickID
           , joyBallEventBall :: Word8
           , joyBallEventXRel :: Int16
           , joyBallEventYRel :: Int16
           }
           | JoyHatEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , joyHatEventWhich :: JoystickID
           , joyHatEventHat :: Word8
           , joyHatEventValue :: Word8
           }
           | JoyButtonEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , joyButtonEventWhich :: JoystickID
           , joyButtonEventButton :: Word8
           , joyButtonEventState :: Word8
           }
           | JoyDeviceEvent{
             eventType :: Word32
           , eventTimestamp :: Word32
           , joyDeviceEventWhich :: Int32
           }
           | ControllerAxisEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , controllerAxisEventWhich :: JoystickID
           , controllerAxisEventAxis :: Word8
           , controllerAxisEventValue :: Int16
           }
           | ControllerButtonEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , controllerButtonEventWhich :: JoystickID
           , controllerButtonEventButton :: Word8
           , controllerButtonEventState :: Word8
           }
           | ControllerDeviceEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , controllerDeviceEventWhich :: Int32
           }
           | QuitEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           }
           | UserEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , userEventWindowID :: Word32
           , userEventCode :: Int32
           , userEventData1 :: Ptr ()
           , userEventData2 :: Ptr ()
           }
           | SysWMEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , sysWMEventMsg :: SysWMmsg
           }
           | TouchFingerEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , touchFingerEventTouchID :: TouchID
           , touchFingerEventFingerID :: FingerID
           , touchFingerEventX :: CFloat
           , touchFingerEventY :: CFloat
           , touchFingerEventDX :: CFloat
           , touchFingerEventDY :: CFloat
           , touchFingerEventPressure :: CFloat
           }
           | MultiGestureEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , multiGestureEventTouchID :: TouchID
           , multiGestureEventDTheta :: CFloat
           , multiGestureEventDDist :: CFloat
           , multiGestureEventX :: CFloat
           , multiGestureEventY :: CFloat
           , multiGestureEventNumFingers :: Word16
           }
           | DollarGestureEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , dollarGestureEventTouchID :: TouchID
           , dollarGestureEventGestureID :: GestureID
           , dollarGestureEventNumFingers :: Word32
           , dollarGestureEventError :: CFloat
           , dollarGestureEventX :: CFloat
           , dollarGestureEventY :: CFloat
           }
           | DropEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           , dropEventFile :: CString
           }
           | ClipboardUpdateEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           }
           | UnknownEvent {
             eventType :: Word32
           , eventTimestamp :: Word32
           }
           deriving (Eq, Show)

instance Storable Event where
	sizeOf _ = ((56))
{-# LINE 414 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		typ <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 417 "src/SDL/Raw/Types.hsc" #-}
		timestamp <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 418 "src/SDL/Raw/Types.hsc" #-}
		case typ of
			(256) ->
{-# LINE 420 "src/SDL/Raw/Types.hsc" #-}
				return $! QuitEvent typ timestamp
			(512) -> do
{-# LINE 422 "src/SDL/Raw/Types.hsc" #-}
				wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 423 "src/SDL/Raw/Types.hsc" #-}
				event <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 424 "src/SDL/Raw/Types.hsc" #-}
				data1 <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 425 "src/SDL/Raw/Types.hsc" #-}
				data2 <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 426 "src/SDL/Raw/Types.hsc" #-}
				return $! WindowEvent typ timestamp wid event data1 data2
			(513) -> do
{-# LINE 428 "src/SDL/Raw/Types.hsc" #-}
				msg <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 429 "src/SDL/Raw/Types.hsc" #-}
				return $! SysWMEvent typ timestamp msg
			(768) -> key $ KeyboardEvent typ timestamp
{-# LINE 431 "src/SDL/Raw/Types.hsc" #-}
			(769) -> key $ KeyboardEvent typ timestamp
{-# LINE 432 "src/SDL/Raw/Types.hsc" #-}
			(770) -> do
{-# LINE 433 "src/SDL/Raw/Types.hsc" #-}
				wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 434 "src/SDL/Raw/Types.hsc" #-}
				text <- peekArray (32) $ ((\hsc_ptr -> hsc_ptr `plusPtr` 12)) ptr
{-# LINE 435 "src/SDL/Raw/Types.hsc" #-}
				start <- ((\hsc_ptr -> peekByteOff hsc_ptr 44)) ptr
{-# LINE 436 "src/SDL/Raw/Types.hsc" #-}
				len <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 437 "src/SDL/Raw/Types.hsc" #-}
				return $! TextEditingEvent typ timestamp wid text start len
			(771) -> do
{-# LINE 439 "src/SDL/Raw/Types.hsc" #-}
				wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 440 "src/SDL/Raw/Types.hsc" #-}
				text <- peekArray (32) $ ((\hsc_ptr -> hsc_ptr `plusPtr` 12)) ptr
{-# LINE 441 "src/SDL/Raw/Types.hsc" #-}
				return $! TextInputEvent typ timestamp wid text
			(1024) -> do
{-# LINE 443 "src/SDL/Raw/Types.hsc" #-}
				wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 444 "src/SDL/Raw/Types.hsc" #-}
				which <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 445 "src/SDL/Raw/Types.hsc" #-}
				state <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 446 "src/SDL/Raw/Types.hsc" #-}
				x <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 447 "src/SDL/Raw/Types.hsc" #-}
				y <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 448 "src/SDL/Raw/Types.hsc" #-}
				xrel <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 449 "src/SDL/Raw/Types.hsc" #-}
				yrel <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 450 "src/SDL/Raw/Types.hsc" #-}
				return $! MouseMotionEvent typ timestamp wid which state x y xrel yrel
			(1025) -> mouse $ MouseButtonEvent typ timestamp
{-# LINE 452 "src/SDL/Raw/Types.hsc" #-}
			(1026) -> mouse $ MouseButtonEvent typ timestamp
{-# LINE 453 "src/SDL/Raw/Types.hsc" #-}
			(1027) -> do
{-# LINE 454 "src/SDL/Raw/Types.hsc" #-}
				wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 455 "src/SDL/Raw/Types.hsc" #-}
				which <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 456 "src/SDL/Raw/Types.hsc" #-}
				x <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 457 "src/SDL/Raw/Types.hsc" #-}
				y <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 458 "src/SDL/Raw/Types.hsc" #-}
				return $! MouseWheelEvent typ timestamp wid which x y
			(1536) -> do
{-# LINE 460 "src/SDL/Raw/Types.hsc" #-}
				which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 461 "src/SDL/Raw/Types.hsc" #-}
				axis <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 462 "src/SDL/Raw/Types.hsc" #-}
				value <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 463 "src/SDL/Raw/Types.hsc" #-}
				return $! JoyAxisEvent typ timestamp which axis value
			(1537) -> do
{-# LINE 465 "src/SDL/Raw/Types.hsc" #-}
				which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 466 "src/SDL/Raw/Types.hsc" #-}
				ball <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 467 "src/SDL/Raw/Types.hsc" #-}
				xrel <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 468 "src/SDL/Raw/Types.hsc" #-}
				yrel <- ((\hsc_ptr -> peekByteOff hsc_ptr 18)) ptr
{-# LINE 469 "src/SDL/Raw/Types.hsc" #-}
				return $! JoyBallEvent typ timestamp which ball xrel yrel
			(1538) -> do
{-# LINE 471 "src/SDL/Raw/Types.hsc" #-}
				which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 472 "src/SDL/Raw/Types.hsc" #-}
				hat <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 473 "src/SDL/Raw/Types.hsc" #-}
				value <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 474 "src/SDL/Raw/Types.hsc" #-}
				return $! JoyHatEvent typ timestamp which hat value
			(1539) -> joybutton $ JoyButtonEvent typ timestamp
{-# LINE 476 "src/SDL/Raw/Types.hsc" #-}
			(1540) -> joybutton $ JoyButtonEvent typ timestamp
{-# LINE 477 "src/SDL/Raw/Types.hsc" #-}
			(1541) -> joydevice $ JoyDeviceEvent typ timestamp
{-# LINE 478 "src/SDL/Raw/Types.hsc" #-}
			(1542) -> joydevice $ JoyDeviceEvent typ timestamp
{-# LINE 479 "src/SDL/Raw/Types.hsc" #-}
			(1616) -> do
{-# LINE 480 "src/SDL/Raw/Types.hsc" #-}
				which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 481 "src/SDL/Raw/Types.hsc" #-}
				axis <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 482 "src/SDL/Raw/Types.hsc" #-}
				value <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 483 "src/SDL/Raw/Types.hsc" #-}
				return $! ControllerButtonEvent typ timestamp which axis value
			(1617) -> controllerbutton $ ControllerButtonEvent typ timestamp
{-# LINE 485 "src/SDL/Raw/Types.hsc" #-}
			(1618) -> controllerbutton $ ControllerButtonEvent typ timestamp
{-# LINE 486 "src/SDL/Raw/Types.hsc" #-}
			(1619) -> controllerdevice $ ControllerDeviceEvent typ timestamp
{-# LINE 487 "src/SDL/Raw/Types.hsc" #-}
			(1620) -> controllerdevice $ ControllerDeviceEvent typ timestamp
{-# LINE 488 "src/SDL/Raw/Types.hsc" #-}
			(1621) -> controllerdevice $ ControllerDeviceEvent typ timestamp
{-# LINE 489 "src/SDL/Raw/Types.hsc" #-}
			(1792) -> finger $ TouchFingerEvent typ timestamp
{-# LINE 490 "src/SDL/Raw/Types.hsc" #-}
			(1793) -> finger $ TouchFingerEvent typ timestamp
{-# LINE 491 "src/SDL/Raw/Types.hsc" #-}
			(1794) -> finger $ TouchFingerEvent typ timestamp
{-# LINE 492 "src/SDL/Raw/Types.hsc" #-}
			(2048) -> dollargesture $ DollarGestureEvent typ timestamp
{-# LINE 493 "src/SDL/Raw/Types.hsc" #-}
			(2049) -> dollargesture $ DollarGestureEvent typ timestamp
{-# LINE 494 "src/SDL/Raw/Types.hsc" #-}
			(2050) -> do
{-# LINE 495 "src/SDL/Raw/Types.hsc" #-}
				touchId <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 496 "src/SDL/Raw/Types.hsc" #-}
				dTheta <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 497 "src/SDL/Raw/Types.hsc" #-}
				dDist <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 498 "src/SDL/Raw/Types.hsc" #-}
				x <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 499 "src/SDL/Raw/Types.hsc" #-}
				y <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 500 "src/SDL/Raw/Types.hsc" #-}
				numFingers <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 501 "src/SDL/Raw/Types.hsc" #-}
				return $! MultiGestureEvent typ timestamp touchId dTheta dDist x y numFingers
			(2304) ->
{-# LINE 503 "src/SDL/Raw/Types.hsc" #-}
				return $! ClipboardUpdateEvent typ timestamp
			(4096) -> do
{-# LINE 505 "src/SDL/Raw/Types.hsc" #-}
				file <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 506 "src/SDL/Raw/Types.hsc" #-}
				return $! DropEvent typ timestamp file
			x | x >= (32768) -> do
{-# LINE 508 "src/SDL/Raw/Types.hsc" #-}
				wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 509 "src/SDL/Raw/Types.hsc" #-}
				code <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 510 "src/SDL/Raw/Types.hsc" #-}
				data1 <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 511 "src/SDL/Raw/Types.hsc" #-}
				data2 <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 512 "src/SDL/Raw/Types.hsc" #-}
				return $! UserEvent typ timestamp wid code data1 data2
			_ -> return $! UnknownEvent typ timestamp
		where
		key f = do
			wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 517 "src/SDL/Raw/Types.hsc" #-}
			state <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 518 "src/SDL/Raw/Types.hsc" #-}
			repeat' <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 519 "src/SDL/Raw/Types.hsc" #-}
			keysym <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 520 "src/SDL/Raw/Types.hsc" #-}
			return $! f wid state repeat' keysym

		mouse f = do
			wid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 524 "src/SDL/Raw/Types.hsc" #-}
			which <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 525 "src/SDL/Raw/Types.hsc" #-}
			button <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 526 "src/SDL/Raw/Types.hsc" #-}
			state <- ((\hsc_ptr -> peekByteOff hsc_ptr 17)) ptr
{-# LINE 527 "src/SDL/Raw/Types.hsc" #-}
			clicks <- ((\hsc_ptr -> peekByteOff hsc_ptr 18)) ptr
{-# LINE 528 "src/SDL/Raw/Types.hsc" #-}
			x <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 529 "src/SDL/Raw/Types.hsc" #-}
			y <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 530 "src/SDL/Raw/Types.hsc" #-}
			return $! f wid which button state clicks x y

		joybutton f = do
			which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 534 "src/SDL/Raw/Types.hsc" #-}
			button <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 535 "src/SDL/Raw/Types.hsc" #-}
			state <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 536 "src/SDL/Raw/Types.hsc" #-}
			return $! f which button state

		joydevice f = do
			which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 540 "src/SDL/Raw/Types.hsc" #-}
			return $! f which

		controllerbutton f = do
			which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 544 "src/SDL/Raw/Types.hsc" #-}
			button <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 545 "src/SDL/Raw/Types.hsc" #-}
			state <- ((\hsc_ptr -> peekByteOff hsc_ptr 13)) ptr
{-# LINE 546 "src/SDL/Raw/Types.hsc" #-}
			return $! f which button state

		controllerdevice f = do
			which <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 550 "src/SDL/Raw/Types.hsc" #-}
			return $! f which

		finger f = do
			touchId <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 554 "src/SDL/Raw/Types.hsc" #-}
			fingerId <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 555 "src/SDL/Raw/Types.hsc" #-}
			x <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 556 "src/SDL/Raw/Types.hsc" #-}
			y <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 557 "src/SDL/Raw/Types.hsc" #-}
			dx <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 558 "src/SDL/Raw/Types.hsc" #-}
			dy <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 559 "src/SDL/Raw/Types.hsc" #-}
			pressure <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 560 "src/SDL/Raw/Types.hsc" #-}
			return $! f touchId fingerId x y dx dy pressure

		dollargesture f = do
			touchId <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 564 "src/SDL/Raw/Types.hsc" #-}
			gestureId <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 565 "src/SDL/Raw/Types.hsc" #-}
			numFingers <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 566 "src/SDL/Raw/Types.hsc" #-}
			err <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 567 "src/SDL/Raw/Types.hsc" #-}
			x <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 568 "src/SDL/Raw/Types.hsc" #-}
			y <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 569 "src/SDL/Raw/Types.hsc" #-}
			return $! f touchId gestureId numFingers err x y
	poke ptr ev = case ev of
		WindowEvent typ timestamp wid event data1 data2 -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 573 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 574 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 575 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr event
{-# LINE 576 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr data1
{-# LINE 577 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr data2
{-# LINE 578 "src/SDL/Raw/Types.hsc" #-}
		KeyboardEvent typ timestamp wid state repeat' keysym -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 580 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 581 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 582 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr state
{-# LINE 583 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 13)) ptr repeat'
{-# LINE 584 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr keysym
{-# LINE 585 "src/SDL/Raw/Types.hsc" #-}
		TextEditingEvent typ timestamp wid text start len -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 587 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 588 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 589 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 12)) ptr) text
{-# LINE 590 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 44)) ptr start
{-# LINE 591 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 48)) ptr len
{-# LINE 592 "src/SDL/Raw/Types.hsc" #-}
		TextInputEvent typ timestamp wid text -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 594 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 595 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 596 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 12)) ptr) text
{-# LINE 597 "src/SDL/Raw/Types.hsc" #-}
		MouseMotionEvent typ timestamp wid which state x y xrel yrel -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 599 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 600 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 601 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr which
{-# LINE 602 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr state
{-# LINE 603 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr x
{-# LINE 604 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr y
{-# LINE 605 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr xrel
{-# LINE 606 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr yrel
{-# LINE 607 "src/SDL/Raw/Types.hsc" #-}
		MouseButtonEvent typ timestamp wid which button state clicks x y -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 609 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 610 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 611 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr which
{-# LINE 612 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr button
{-# LINE 613 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 17)) ptr state
{-# LINE 614 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 18)) ptr clicks
{-# LINE 615 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr x
{-# LINE 616 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr y
{-# LINE 617 "src/SDL/Raw/Types.hsc" #-}
		MouseWheelEvent typ timestamp wid which x y -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 619 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 620 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 621 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr which
{-# LINE 622 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr x
{-# LINE 623 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr y
{-# LINE 624 "src/SDL/Raw/Types.hsc" #-}
		JoyAxisEvent typ timestamp which axis value -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 626 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 627 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 628 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr axis
{-# LINE 629 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr value
{-# LINE 630 "src/SDL/Raw/Types.hsc" #-}
		JoyBallEvent typ timestamp which ball xrel yrel -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 632 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 633 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 634 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr ball
{-# LINE 635 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr xrel
{-# LINE 636 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 18)) ptr yrel
{-# LINE 637 "src/SDL/Raw/Types.hsc" #-}
		JoyHatEvent typ timestamp which hat value -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 639 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 640 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 641 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr hat
{-# LINE 642 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 13)) ptr value
{-# LINE 643 "src/SDL/Raw/Types.hsc" #-}
		JoyButtonEvent typ timestamp which button state -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 645 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 646 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 647 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr button
{-# LINE 648 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 13)) ptr state
{-# LINE 649 "src/SDL/Raw/Types.hsc" #-}
		JoyDeviceEvent typ timestamp which -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 651 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 652 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 653 "src/SDL/Raw/Types.hsc" #-}
		ControllerAxisEvent typ timestamp which axis value -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 655 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 656 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 657 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr axis
{-# LINE 658 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr value
{-# LINE 659 "src/SDL/Raw/Types.hsc" #-}
		ControllerButtonEvent typ timestamp which button state -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 661 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 662 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 663 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr button
{-# LINE 664 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 13)) ptr state
{-# LINE 665 "src/SDL/Raw/Types.hsc" #-}
		ControllerDeviceEvent typ timestamp which -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 667 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 668 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr which
{-# LINE 669 "src/SDL/Raw/Types.hsc" #-}
		QuitEvent typ timestamp -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 671 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 672 "src/SDL/Raw/Types.hsc" #-}
		UserEvent typ timestamp wid code data1 data2 -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 674 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 675 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr wid
{-# LINE 676 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr code
{-# LINE 677 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr data1
{-# LINE 678 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr data2
{-# LINE 679 "src/SDL/Raw/Types.hsc" #-}
		SysWMEvent typ timestamp msg -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 681 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 682 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr msg
{-# LINE 683 "src/SDL/Raw/Types.hsc" #-}
		TouchFingerEvent typ timestamp touchid fingerid x y dx dy pressure -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 685 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 686 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr touchid
{-# LINE 687 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr fingerid
{-# LINE 688 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr x
{-# LINE 689 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr y
{-# LINE 690 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr dx
{-# LINE 691 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr dy
{-# LINE 692 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr pressure
{-# LINE 693 "src/SDL/Raw/Types.hsc" #-}
		MultiGestureEvent typ timestamp touchid dtheta ddist x y numfingers -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 695 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 696 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr touchid
{-# LINE 697 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr dtheta
{-# LINE 698 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr ddist
{-# LINE 699 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr x
{-# LINE 700 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr y
{-# LINE 701 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr numfingers
{-# LINE 702 "src/SDL/Raw/Types.hsc" #-}
		DollarGestureEvent typ timestamp touchid gestureid numfingers err x y -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 704 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 705 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr touchid
{-# LINE 706 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr gestureid
{-# LINE 707 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr numfingers
{-# LINE 708 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr err
{-# LINE 709 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr x
{-# LINE 710 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr y
{-# LINE 711 "src/SDL/Raw/Types.hsc" #-}
		ClipboardUpdateEvent typ timestamp -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 713 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 714 "src/SDL/Raw/Types.hsc" #-}
		DropEvent typ timestamp file -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 716 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 717 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr file
{-# LINE 718 "src/SDL/Raw/Types.hsc" #-}
		UnknownEvent typ timestamp -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 720 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr timestamp
{-# LINE 721 "src/SDL/Raw/Types.hsc" #-}

data Finger = Finger {
              fingerID :: FingerID
            , fingerX :: CFloat
            , fingerY :: CFloat
            , fingerPressure :: CFloat
            } deriving (Eq, Show)

instance Storable Finger where
	sizeOf _ = ((24))
{-# LINE 731 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		fingerId <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 734 "src/SDL/Raw/Types.hsc" #-}
		x <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 735 "src/SDL/Raw/Types.hsc" #-}
		y <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 736 "src/SDL/Raw/Types.hsc" #-}
		pressure <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 737 "src/SDL/Raw/Types.hsc" #-}
		return $! Finger fingerId x y pressure
	poke ptr (Finger fingerId x y pressure) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr fingerId
{-# LINE 740 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr x
{-# LINE 741 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr y
{-# LINE 742 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr pressure
{-# LINE 743 "src/SDL/Raw/Types.hsc" #-}

data GameControllerButtonBind = GameControllerButtonBindNone
                              | GameControllerButtonBindButton {
                                gameControllerButtonBindButton :: CInt
                              }
                              | GameControllerButtonBindAxis {
                                gameControllerButtonBindAxis :: CInt
                              }
                              | GameControllerButtonBindHat {
                                gameControllerButtonBindHat :: CInt
                              , gameControllerButtonBindHatMask :: CInt
                              } deriving (Eq, Show)

instance Storable GameControllerButtonBind where
	sizeOf _ = ((12))
{-# LINE 758 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		bind_type <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 761 "src/SDL/Raw/Types.hsc" #-}
		case bind_type :: (Word32) of
{-# LINE 762 "src/SDL/Raw/Types.hsc" #-}
			(0) -> do
{-# LINE 763 "src/SDL/Raw/Types.hsc" #-}
				return $! GameControllerButtonBindNone
			(1) -> do
{-# LINE 765 "src/SDL/Raw/Types.hsc" #-}
				button <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 766 "src/SDL/Raw/Types.hsc" #-}
				return $! GameControllerButtonBindButton button
			(2) -> do
{-# LINE 768 "src/SDL/Raw/Types.hsc" #-}
				axis <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 769 "src/SDL/Raw/Types.hsc" #-}
				return $! GameControllerButtonBindAxis axis
			(3) -> do
{-# LINE 771 "src/SDL/Raw/Types.hsc" #-}
				hat <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 772 "src/SDL/Raw/Types.hsc" #-}
				hat_mask <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 773 "src/SDL/Raw/Types.hsc" #-}
				return $! GameControllerButtonBindHat hat hat_mask
			_ -> error $ "Unknown type " ++ show bind_type ++ " for SDL_GameControllerButtonBind"
	poke ptr bind = case bind of
		GameControllerButtonBindNone -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr ((0) :: (Word32))
{-# LINE 778 "src/SDL/Raw/Types.hsc" #-}
		GameControllerButtonBindButton button -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr ((1) :: (Word32))
{-# LINE 780 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr button
{-# LINE 781 "src/SDL/Raw/Types.hsc" #-}
		GameControllerButtonBindAxis axis -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr ((2) :: (Word32))
{-# LINE 783 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr axis
{-# LINE 784 "src/SDL/Raw/Types.hsc" #-}
		GameControllerButtonBindHat hat hat_mask -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr ((3) :: (Word32))
{-# LINE 786 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr hat
{-# LINE 787 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr hat_mask
{-# LINE 788 "src/SDL/Raw/Types.hsc" #-}

data HapticDirection = HapticDirection {
                       hapticDirectionType :: Word8
                     , hapticDirectionX :: Int32
                     , hapticDirectionY :: Int32
                     , hapticDirectionZ :: Int32
                     } deriving (Eq, Show)

instance Storable HapticDirection where
	sizeOf _ = ((16))
{-# LINE 798 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		typ <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 801 "src/SDL/Raw/Types.hsc" #-}
		x <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 802 "src/SDL/Raw/Types.hsc" #-}
		y <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 803 "src/SDL/Raw/Types.hsc" #-}
		z <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 804 "src/SDL/Raw/Types.hsc" #-}
		return $! HapticDirection typ x y z
	poke ptr (HapticDirection typ x y z) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 807 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr x
{-# LINE 808 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr y
{-# LINE 809 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr z
{-# LINE 810 "src/SDL/Raw/Types.hsc" #-}

data HapticEffect = HapticConstant {
                    hapticEffectType :: Word16
                  , hapticConstantDirection :: HapticDirection
                  , hapticConstantLength :: Word32
                  , hapticConstantDelay :: Word16
                  , hapticConstantButton :: Word16
                  , hapticConstantInterval :: Word16
                  , hapticConstantLevel :: Int16
                  , hapticConstantAttackLength :: Word16
                  , hapticConstantAttackLevel :: Word16
                  , hapticConstantFadeLength :: Word16
                  , hapticConstantFadeLevel :: Word16
                  }
                  | HapticPeriodic {
                    hapticEffectType :: Word16
                  , hapticPeriodicDirection :: HapticDirection
                  , hapticPeriodicLength :: Word32
                  , hapticPeriodicDelay :: Word16
                  , hapticPeriodicButton :: Word16
                  , hapticPeriodicInterval :: Word16
                  , hapticPeriodicPeriod :: Word16
                  , hapticPeriodicMagnitude :: Int16
                  , hapticPeriodicOffset :: Int16
                  , hapticPeriodicPhase :: Word16
                  , hapticPeriodicAttackLength :: Word16
                  , hapticPeriodicAttackLevel :: Word16
                  , hapticPeriodicFadeLength :: Word16
                  , hapticPeriodicFadeLevel :: Word16
                  }
                  | HapticCondition {
                    hapticEffectType :: Word16
                  , hapticConditionLength :: Word32
                  , hapticConditionDelay :: Word16
                  , hapticConditionButton :: Word16
                  , hapticConditionInterval :: Word16
                  , hapticConditionRightSat :: [Word16]
                  , hapticConditionLeftSat :: [Word16]
                  , hapticConditionRightCoeff :: [Int16]
                  , hapticConditionLeftCoeff :: [Int16]
                  , hapticConditionDeadband :: [Word16]
                  , hapticConditionCenter :: [Int16]
                  }
                  | HapticRamp {
                    hapticEffectType :: Word16
                  , hapticRampDirection :: HapticDirection
                  , hapticRampLength :: Word32
                  , hapticRampDelay :: Word16
                  , hapticRampButton :: Word16
                  , hapticRampInterval :: Word16
                  , hapticRampStart :: Int16
                  , hapticRampEnd :: Int16
                  , hapticRampAttackLength :: Word16
                  , hapticRampAttackLevel :: Word16
                  , hapticRampFadeLength :: Word16
                  , hapticRampFadeLevel :: Word16
                  }
                  | HapticLeftRight {
                    hapticEffectType :: Word16
                  , hapticLeftRightLength :: Word32
                  , hapticLeftRightLargeMagnitude :: Word16
                  , hapticLeftRightSmallMagnitude :: Word16
                  }
                  | HapticCustom {
                    hapticEffectType :: Word16
                  , hapticCustomDirection :: HapticDirection
                  , hapticCustomLength :: Word32
                  , hapticCustomDelay :: Word16
                  , hapticCustomButton :: Word16
                  , hapticCustomInterval :: Word16
                  , hapticCustomChannels :: Word8
                  , hapticCustomPeriod :: Word16
                  , hapticCustomSamples :: Word16
                  , hapticCustomData :: Ptr Word16
                  , hapticCustomAttackLength :: Word16
                  , hapticCustomAttackLevel :: Word16
                  , hapticCustomFadeLength :: Word16
                  , hapticCustomFadeLevel :: Word16
                  } deriving (Eq, Show)

instance Storable HapticEffect where
	sizeOf _ = ((72))
{-# LINE 892 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		typ <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 895 "src/SDL/Raw/Types.hsc" #-}
		case typ of
			(1) -> do
{-# LINE 897 "src/SDL/Raw/Types.hsc" #-}
				direction <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 898 "src/SDL/Raw/Types.hsc" #-}
				len <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 899 "src/SDL/Raw/Types.hsc" #-}
				delay <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 900 "src/SDL/Raw/Types.hsc" #-}
				button <- ((\hsc_ptr -> peekByteOff hsc_ptr 26)) ptr
{-# LINE 901 "src/SDL/Raw/Types.hsc" #-}
				interval <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 902 "src/SDL/Raw/Types.hsc" #-}
				level <- ((\hsc_ptr -> peekByteOff hsc_ptr 30)) ptr
{-# LINE 903 "src/SDL/Raw/Types.hsc" #-}
				attack_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 904 "src/SDL/Raw/Types.hsc" #-}
				attack_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 34)) ptr
{-# LINE 905 "src/SDL/Raw/Types.hsc" #-}
				fade_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 906 "src/SDL/Raw/Types.hsc" #-}
				fade_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 38)) ptr
{-# LINE 907 "src/SDL/Raw/Types.hsc" #-}
				return $! HapticConstant typ direction len delay button interval level attack_length attack_level fade_length fade_level

			(2) -> hapticperiodic $ HapticPeriodic typ
{-# LINE 910 "src/SDL/Raw/Types.hsc" #-}
			(8) -> hapticperiodic $ HapticPeriodic typ
{-# LINE 911 "src/SDL/Raw/Types.hsc" #-}
			(16) -> hapticperiodic $ HapticPeriodic typ
{-# LINE 912 "src/SDL/Raw/Types.hsc" #-}
			(32) -> hapticperiodic $ HapticPeriodic typ
{-# LINE 913 "src/SDL/Raw/Types.hsc" #-}

			(64) -> do
{-# LINE 915 "src/SDL/Raw/Types.hsc" #-}
				direction <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 916 "src/SDL/Raw/Types.hsc" #-}
				len <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 917 "src/SDL/Raw/Types.hsc" #-}
				delay <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 918 "src/SDL/Raw/Types.hsc" #-}
				button <- ((\hsc_ptr -> peekByteOff hsc_ptr 26)) ptr
{-# LINE 919 "src/SDL/Raw/Types.hsc" #-}
				interval <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 920 "src/SDL/Raw/Types.hsc" #-}
				start <- ((\hsc_ptr -> peekByteOff hsc_ptr 30)) ptr
{-# LINE 921 "src/SDL/Raw/Types.hsc" #-}
				end <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 922 "src/SDL/Raw/Types.hsc" #-}
				attack_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 34)) ptr
{-# LINE 923 "src/SDL/Raw/Types.hsc" #-}
				attack_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 924 "src/SDL/Raw/Types.hsc" #-}
				fade_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 38)) ptr
{-# LINE 925 "src/SDL/Raw/Types.hsc" #-}
				fade_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 926 "src/SDL/Raw/Types.hsc" #-}
				return $! HapticRamp typ direction len delay button interval start end attack_length attack_level fade_length fade_level

			(128) -> hapticcondition $ HapticCondition typ
{-# LINE 929 "src/SDL/Raw/Types.hsc" #-}
			(256) -> hapticcondition $ HapticCondition typ
{-# LINE 930 "src/SDL/Raw/Types.hsc" #-}
			(512) -> hapticcondition $ HapticCondition typ
{-# LINE 931 "src/SDL/Raw/Types.hsc" #-}
			(1024) -> hapticcondition $ HapticCondition typ
{-# LINE 932 "src/SDL/Raw/Types.hsc" #-}

			(4) -> do
{-# LINE 934 "src/SDL/Raw/Types.hsc" #-}
				len <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 935 "src/SDL/Raw/Types.hsc" #-}
				large_magnitude <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 936 "src/SDL/Raw/Types.hsc" #-}
				small_magnitude <- ((\hsc_ptr -> peekByteOff hsc_ptr 10)) ptr
{-# LINE 937 "src/SDL/Raw/Types.hsc" #-}
				return $! HapticLeftRight typ len large_magnitude small_magnitude

			(2048) -> do
{-# LINE 940 "src/SDL/Raw/Types.hsc" #-}
				direction <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 941 "src/SDL/Raw/Types.hsc" #-}
				len <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 942 "src/SDL/Raw/Types.hsc" #-}
				delay <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 943 "src/SDL/Raw/Types.hsc" #-}
				button <- ((\hsc_ptr -> peekByteOff hsc_ptr 26)) ptr
{-# LINE 944 "src/SDL/Raw/Types.hsc" #-}
				interval <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 945 "src/SDL/Raw/Types.hsc" #-}
				channels <- ((\hsc_ptr -> peekByteOff hsc_ptr 30)) ptr
{-# LINE 946 "src/SDL/Raw/Types.hsc" #-}
				period <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 947 "src/SDL/Raw/Types.hsc" #-}
				samples <- ((\hsc_ptr -> peekByteOff hsc_ptr 34)) ptr
{-# LINE 948 "src/SDL/Raw/Types.hsc" #-}
				datum <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 949 "src/SDL/Raw/Types.hsc" #-}
				attack_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 950 "src/SDL/Raw/Types.hsc" #-}
				attack_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 50)) ptr
{-# LINE 951 "src/SDL/Raw/Types.hsc" #-}
				fade_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 52)) ptr
{-# LINE 952 "src/SDL/Raw/Types.hsc" #-}
				fade_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 54)) ptr
{-# LINE 953 "src/SDL/Raw/Types.hsc" #-}
				return $! HapticCustom typ direction len delay button interval channels period samples datum attack_length attack_level fade_length fade_level
			_ -> error $ "Unknown type " ++ show typ ++ " for SDL_HapticEffect"
		where
		hapticperiodic f = do
			direction <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 958 "src/SDL/Raw/Types.hsc" #-}
			len <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 959 "src/SDL/Raw/Types.hsc" #-}
			delay <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 960 "src/SDL/Raw/Types.hsc" #-}
			button <- ((\hsc_ptr -> peekByteOff hsc_ptr 26)) ptr
{-# LINE 961 "src/SDL/Raw/Types.hsc" #-}
			interval <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 962 "src/SDL/Raw/Types.hsc" #-}
			period <- ((\hsc_ptr -> peekByteOff hsc_ptr 30)) ptr
{-# LINE 963 "src/SDL/Raw/Types.hsc" #-}
			magnitude <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 964 "src/SDL/Raw/Types.hsc" #-}
			offset <- ((\hsc_ptr -> peekByteOff hsc_ptr 34)) ptr
{-# LINE 965 "src/SDL/Raw/Types.hsc" #-}
			phase <- ((\hsc_ptr -> peekByteOff hsc_ptr 36)) ptr
{-# LINE 966 "src/SDL/Raw/Types.hsc" #-}
			attack_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 38)) ptr
{-# LINE 967 "src/SDL/Raw/Types.hsc" #-}
			attack_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 968 "src/SDL/Raw/Types.hsc" #-}
			fade_length <- ((\hsc_ptr -> peekByteOff hsc_ptr 42)) ptr
{-# LINE 969 "src/SDL/Raw/Types.hsc" #-}
			fade_level <- ((\hsc_ptr -> peekByteOff hsc_ptr 44)) ptr
{-# LINE 970 "src/SDL/Raw/Types.hsc" #-}
			return $! f direction len delay button interval period magnitude offset phase attack_length attack_level fade_length fade_level

		hapticcondition f = do
			len <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 974 "src/SDL/Raw/Types.hsc" #-}
			delay <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 975 "src/SDL/Raw/Types.hsc" #-}
			button <- ((\hsc_ptr -> peekByteOff hsc_ptr 26)) ptr
{-# LINE 976 "src/SDL/Raw/Types.hsc" #-}
			interval <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 977 "src/SDL/Raw/Types.hsc" #-}
			right_sat <- peekArray 3 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 30)) ptr
{-# LINE 978 "src/SDL/Raw/Types.hsc" #-}
			left_sat <- peekArray 3 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 36)) ptr
{-# LINE 979 "src/SDL/Raw/Types.hsc" #-}
			right_coeff <- peekArray 3 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 42)) ptr
{-# LINE 980 "src/SDL/Raw/Types.hsc" #-}
			left_coeff <- peekArray 3 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 48)) ptr
{-# LINE 981 "src/SDL/Raw/Types.hsc" #-}
			deadband <- peekArray 3 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 54)) ptr
{-# LINE 982 "src/SDL/Raw/Types.hsc" #-}
			center <- peekArray 3 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 60)) ptr
{-# LINE 983 "src/SDL/Raw/Types.hsc" #-}
			return $! f len delay button interval right_sat left_sat right_coeff left_coeff deadband center
	poke ptr event = case event of
		HapticConstant typ direction len delay button interval level attack_length attack_level fade_length fade_level -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 987 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr direction
{-# LINE 988 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr len
{-# LINE 989 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr delay
{-# LINE 990 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 26)) ptr button
{-# LINE 991 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr interval
{-# LINE 992 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 30)) ptr level
{-# LINE 993 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr attack_length
{-# LINE 994 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 34)) ptr attack_level
{-# LINE 995 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr fade_length
{-# LINE 996 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 38)) ptr fade_level
{-# LINE 997 "src/SDL/Raw/Types.hsc" #-}
		HapticPeriodic typ direction len delay button interval period magnitude offset phase attack_length attack_level fade_length fade_level -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 999 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr direction
{-# LINE 1000 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr len
{-# LINE 1001 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr delay
{-# LINE 1002 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 26)) ptr button
{-# LINE 1003 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr interval
{-# LINE 1004 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 30)) ptr period
{-# LINE 1005 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr magnitude
{-# LINE 1006 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 34)) ptr offset
{-# LINE 1007 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr phase
{-# LINE 1008 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 38)) ptr attack_length
{-# LINE 1009 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr attack_level
{-# LINE 1010 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 42)) ptr fade_length
{-# LINE 1011 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 44)) ptr fade_level
{-# LINE 1012 "src/SDL/Raw/Types.hsc" #-}
		HapticCondition typ len delay button interval right_sat left_sat right_coeff left_coeff deadband center -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 1014 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr len
{-# LINE 1015 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr delay
{-# LINE 1016 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 26)) ptr button
{-# LINE 1017 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr interval
{-# LINE 1018 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 30)) ptr) right_sat
{-# LINE 1019 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 36)) ptr) left_sat
{-# LINE 1020 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 42)) ptr) right_coeff
{-# LINE 1021 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 48)) ptr) left_coeff
{-# LINE 1022 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 54)) ptr) deadband
{-# LINE 1023 "src/SDL/Raw/Types.hsc" #-}
			pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 60)) ptr) center
{-# LINE 1024 "src/SDL/Raw/Types.hsc" #-}
		HapticRamp typ direction len delay button interval start end attack_length attack_level fade_length fade_level -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 1026 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr direction
{-# LINE 1027 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr len
{-# LINE 1028 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr delay
{-# LINE 1029 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 26)) ptr button
{-# LINE 1030 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr interval
{-# LINE 1031 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 30)) ptr start
{-# LINE 1032 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr end
{-# LINE 1033 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 34)) ptr attack_length
{-# LINE 1034 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 36)) ptr attack_level
{-# LINE 1035 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 38)) ptr fade_length
{-# LINE 1036 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr fade_level
{-# LINE 1037 "src/SDL/Raw/Types.hsc" #-}
		HapticLeftRight typ len large_magnitude small_magnitude -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 1039 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr len
{-# LINE 1040 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr large_magnitude
{-# LINE 1041 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 10)) ptr small_magnitude
{-# LINE 1042 "src/SDL/Raw/Types.hsc" #-}
		HapticCustom typ direction len delay button interval channels period samples datum attack_length attack_level fade_length fade_level -> do
			((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr typ
{-# LINE 1044 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr direction
{-# LINE 1045 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr len
{-# LINE 1046 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr delay
{-# LINE 1047 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 26)) ptr button
{-# LINE 1048 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr interval
{-# LINE 1049 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 30)) ptr channels
{-# LINE 1050 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr period
{-# LINE 1051 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 34)) ptr samples
{-# LINE 1052 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr datum
{-# LINE 1053 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 48)) ptr attack_length
{-# LINE 1054 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 50)) ptr attack_level
{-# LINE 1055 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 52)) ptr fade_length
{-# LINE 1056 "src/SDL/Raw/Types.hsc" #-}
			((\hsc_ptr -> pokeByteOff hsc_ptr 54)) ptr fade_level
{-# LINE 1057 "src/SDL/Raw/Types.hsc" #-}

data JoystickGUID = JoystickGUID {
                    joystickGUID :: [Word8]
                  } deriving (Eq, Show)

instance Storable JoystickGUID where
	sizeOf _ = ((16))
{-# LINE 1064 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		guid <- peekArray 16 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) ptr
{-# LINE 1067 "src/SDL/Raw/Types.hsc" #-}
		return $! JoystickGUID guid
	poke ptr (JoystickGUID guid) =
		pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 0)) ptr) guid
{-# LINE 1070 "src/SDL/Raw/Types.hsc" #-}

data Keysym = Keysym {
              keysymScancode :: Scancode
            , keysymKeycode :: Keycode
            , keysymMod :: Word16
            } deriving (Eq, Show)

instance Storable Keysym where
	sizeOf _ = ((16))
{-# LINE 1079 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		scancode <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1082 "src/SDL/Raw/Types.hsc" #-}
		sym <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 1083 "src/SDL/Raw/Types.hsc" #-}
		mod' <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1084 "src/SDL/Raw/Types.hsc" #-}
		return $! Keysym scancode sym mod'
	poke ptr (Keysym scancode sym mod') = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr scancode
{-# LINE 1087 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr sym
{-# LINE 1088 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr mod'
{-# LINE 1089 "src/SDL/Raw/Types.hsc" #-}

data MessageBoxButtonData = MessageBoxButtonData {
                            messageBoxButtonDataFlags :: Word32
                          , messageBoxButtonButtonID :: CInt
                          , messageBoxButtonText :: CString
                          } deriving (Eq, Show)

instance Storable MessageBoxButtonData where
	sizeOf _ = ((16))
{-# LINE 1098 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1101 "src/SDL/Raw/Types.hsc" #-}
		buttonid <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 1102 "src/SDL/Raw/Types.hsc" #-}
		text <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1103 "src/SDL/Raw/Types.hsc" #-}
		return $! MessageBoxButtonData flags buttonid text
	poke ptr (MessageBoxButtonData flags buttonid text) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr flags
{-# LINE 1106 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr buttonid
{-# LINE 1107 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr text
{-# LINE 1108 "src/SDL/Raw/Types.hsc" #-}

data MessageBoxColor = MessageBoxColor {
                       messageBoxColorR :: Word8
                     , messageBoxColorG :: Word8
                     , messageBoxColorB :: Word8
                     } deriving (Eq, Show)

instance Storable MessageBoxColor where
	sizeOf _ = ((3))
{-# LINE 1117 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		r <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1120 "src/SDL/Raw/Types.hsc" #-}
		g <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) ptr
{-# LINE 1121 "src/SDL/Raw/Types.hsc" #-}
		b <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
{-# LINE 1122 "src/SDL/Raw/Types.hsc" #-}
		return $! MessageBoxColor r g b
	poke ptr (MessageBoxColor r g b) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr r
{-# LINE 1125 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 1)) ptr g
{-# LINE 1126 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 2)) ptr b
{-# LINE 1127 "src/SDL/Raw/Types.hsc" #-}

data MessageBoxColorScheme = MessageBoxColorScheme {
                             messageBoxColorSchemeColorBackground :: MessageBoxColor
                           , messageBoxColorSchemeColorText :: MessageBoxColor
                           , messageBoxColorSchemeColorButtonBorder :: MessageBoxColor
                           , messageBoxColorSchemeColorButtonBackground :: MessageBoxColor
                           , messageBoxColorSchemeColorButtonSelected :: MessageBoxColor
                           } deriving (Eq, Show)

instance Storable MessageBoxColorScheme where
	sizeOf _ = ((15))
{-# LINE 1138 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		background <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1141 "src/SDL/Raw/Types.hsc" #-}
		text <- ((\hsc_ptr -> peekByteOff hsc_ptr 3)) ptr
{-# LINE 1142 "src/SDL/Raw/Types.hsc" #-}
		button_border <- ((\hsc_ptr -> peekByteOff hsc_ptr 6)) ptr
{-# LINE 1143 "src/SDL/Raw/Types.hsc" #-}
		button_background <- ((\hsc_ptr -> peekByteOff hsc_ptr 9)) ptr
{-# LINE 1144 "src/SDL/Raw/Types.hsc" #-}
		button_selected <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 1145 "src/SDL/Raw/Types.hsc" #-}
		return $! MessageBoxColorScheme background text button_border button_background button_selected
	poke ptr (MessageBoxColorScheme background text button_border button_background button_selected) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr background
{-# LINE 1148 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 3)) ptr text
{-# LINE 1149 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 6)) ptr button_border
{-# LINE 1150 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 9)) ptr button_background
{-# LINE 1151 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr button_selected
{-# LINE 1152 "src/SDL/Raw/Types.hsc" #-}

data MessageBoxData = MessageBoxData {
                      messageBoxDataFlags :: Word32
                    , messageBoxDataWindow :: Window
                    , messageBoxDataTitle :: CString
                    , messageBoxDataMessage :: CString
                    , messageBoxDataNumButtons :: CInt
                    , messageBoxDataButtons :: Ptr MessageBoxButtonData
                    , messageBoxDataColorScheme :: Ptr MessageBoxColorScheme
                    } deriving (Eq, Show)

instance Storable MessageBoxData where
	sizeOf _ = ((56))
{-# LINE 1165 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1168 "src/SDL/Raw/Types.hsc" #-}
		window <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1169 "src/SDL/Raw/Types.hsc" #-}
		title <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 1170 "src/SDL/Raw/Types.hsc" #-}
		message <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 1171 "src/SDL/Raw/Types.hsc" #-}
		numbuttons <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 1172 "src/SDL/Raw/Types.hsc" #-}
		buttons <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 1173 "src/SDL/Raw/Types.hsc" #-}
		color_scheme <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) ptr
{-# LINE 1174 "src/SDL/Raw/Types.hsc" #-}
		return $! MessageBoxData flags window title message numbuttons buttons color_scheme
	poke ptr (MessageBoxData flags window title message numbuttons buttons color_scheme) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr flags
{-# LINE 1177 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr window
{-# LINE 1178 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr title
{-# LINE 1179 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr message
{-# LINE 1180 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr numbuttons
{-# LINE 1181 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr buttons
{-# LINE 1182 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 48)) ptr color_scheme
{-# LINE 1183 "src/SDL/Raw/Types.hsc" #-}

data Palette = Palette {
               paletteNColors :: CInt
             , paletteColors :: Ptr Color
             } deriving (Eq, Show)

instance Storable Palette where
	sizeOf _ = ((24))
{-# LINE 1191 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		ncolors <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1194 "src/SDL/Raw/Types.hsc" #-}
		colors <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1195 "src/SDL/Raw/Types.hsc" #-}
		return $! Palette ncolors colors
	poke ptr (Palette ncolors colors) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr ncolors
{-# LINE 1198 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr colors
{-# LINE 1199 "src/SDL/Raw/Types.hsc" #-}

data PixelFormat = PixelFormat {
                   pixelFormatFormat :: Word32
                 , pixelFormatPalette :: Ptr Palette
                 , pixelFormatBitsPerPixel :: Word8
                 , pixelFormatBytesPerPixel :: Word8
                 , pixelFormatRMask :: Word32
                 , pixelFormatGMask :: Word32
                 , pixelFormatBMask :: Word32
                 , pixelFormatAMask :: Word32
                 } deriving (Eq, Show)

instance Storable PixelFormat where
	sizeOf _ = ((56))
{-# LINE 1213 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		format <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1216 "src/SDL/Raw/Types.hsc" #-}
		palette <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1217 "src/SDL/Raw/Types.hsc" #-}
		bits_per_pixel <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 1218 "src/SDL/Raw/Types.hsc" #-}
		bytes_per_pixel <- ((\hsc_ptr -> peekByteOff hsc_ptr 17)) ptr
{-# LINE 1219 "src/SDL/Raw/Types.hsc" #-}
		rmask <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 1220 "src/SDL/Raw/Types.hsc" #-}
		gmask <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 1221 "src/SDL/Raw/Types.hsc" #-}
		bmask <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) ptr
{-# LINE 1222 "src/SDL/Raw/Types.hsc" #-}
		amask <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 1223 "src/SDL/Raw/Types.hsc" #-}
		return $! PixelFormat format palette bits_per_pixel bytes_per_pixel rmask gmask bmask amask
	poke ptr (PixelFormat format palette bits_per_pixel bytes_per_pixel rmask gmask bmask amask) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr format
{-# LINE 1226 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr palette
{-# LINE 1227 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr bits_per_pixel
{-# LINE 1228 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 17)) ptr bytes_per_pixel
{-# LINE 1229 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr rmask
{-# LINE 1230 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr gmask
{-# LINE 1231 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 28)) ptr bmask
{-# LINE 1232 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr amask
{-# LINE 1233 "src/SDL/Raw/Types.hsc" #-}

data Point = Point {
             pointX :: CInt
           , pointY :: CInt
           } deriving (Eq, Show)

instance Storable Point where
	sizeOf _ = ((8))
{-# LINE 1241 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		x <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1244 "src/SDL/Raw/Types.hsc" #-}
		y <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 1245 "src/SDL/Raw/Types.hsc" #-}
		return $! Point x y
	poke ptr (Point x y) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr x
{-# LINE 1248 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr y
{-# LINE 1249 "src/SDL/Raw/Types.hsc" #-}

data Rect = Rect {
            rectX :: CInt
          , rectY :: CInt
          , rectW :: CInt
          , rectH :: CInt
          } deriving (Eq, Show)

instance Storable Rect where
	sizeOf _ = ((16))
{-# LINE 1259 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		x <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1262 "src/SDL/Raw/Types.hsc" #-}
		y <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 1263 "src/SDL/Raw/Types.hsc" #-}
		w <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1264 "src/SDL/Raw/Types.hsc" #-}
		h <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 1265 "src/SDL/Raw/Types.hsc" #-}
		return $! Rect x y w h
	poke ptr (Rect x y w h) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr x
{-# LINE 1268 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr y
{-# LINE 1269 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr w
{-# LINE 1270 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr h
{-# LINE 1271 "src/SDL/Raw/Types.hsc" #-}

data RendererInfo = RendererInfo {
                    rendererInfoName :: CString
                  , rendererInfoFlags :: Word32
                  , rendererInfoNumTextureFormats :: Word32
                  , rendererInfoTextureFormats :: [Word32]
                  , rendererInfoMaxTextureWidth :: CInt
                  , rendererInfoMaxTextureHeight :: CInt
                  } deriving (Eq, Show)

instance Storable RendererInfo where
	sizeOf _ = ((88))
{-# LINE 1283 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		name <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1286 "src/SDL/Raw/Types.hsc" #-}
		flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1287 "src/SDL/Raw/Types.hsc" #-}
		num_texture_formats <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 1288 "src/SDL/Raw/Types.hsc" #-}
		texture_formats <- peekArray 16 $ ((\hsc_ptr -> hsc_ptr `plusPtr` 16)) ptr
{-# LINE 1289 "src/SDL/Raw/Types.hsc" #-}
		max_texture_width <- ((\hsc_ptr -> peekByteOff hsc_ptr 80)) ptr
{-# LINE 1290 "src/SDL/Raw/Types.hsc" #-}
		max_texture_height <- ((\hsc_ptr -> peekByteOff hsc_ptr 84)) ptr
{-# LINE 1291 "src/SDL/Raw/Types.hsc" #-}
		return $! RendererInfo name flags num_texture_formats texture_formats max_texture_width max_texture_height
	poke ptr (RendererInfo name flags num_texture_formats texture_formats max_texture_width max_texture_height) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr name
{-# LINE 1294 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr flags
{-# LINE 1295 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr num_texture_formats
{-# LINE 1296 "src/SDL/Raw/Types.hsc" #-}
		pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 16)) ptr) texture_formats
{-# LINE 1297 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 80)) ptr max_texture_width
{-# LINE 1298 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 84)) ptr max_texture_height
{-# LINE 1299 "src/SDL/Raw/Types.hsc" #-}

data RWops = RWops {
             rwopsSize :: FunPtr (Ptr RWops -> IO Int64)
           , rwopsSeek :: FunPtr (Ptr RWops -> Int64 -> CInt -> IO Int64)
           , rwopsRead :: FunPtr (Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize)
           , rwopsWrite :: FunPtr (Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize)
           , rwopsClose :: FunPtr (Ptr RWops -> IO CInt)
           , rwopsType :: Word32
           } deriving (Eq, Show)

instance Storable RWops where
	sizeOf _ = ((72))
{-# LINE 1311 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		size <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1314 "src/SDL/Raw/Types.hsc" #-}
		seek <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1315 "src/SDL/Raw/Types.hsc" #-}
		read' <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 1316 "src/SDL/Raw/Types.hsc" #-}
		write <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr
{-# LINE 1317 "src/SDL/Raw/Types.hsc" #-}
		close <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 1318 "src/SDL/Raw/Types.hsc" #-}
		typ <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 1319 "src/SDL/Raw/Types.hsc" #-}
		return $! RWops size seek read' write close typ
	poke ptr (RWops size seek read' write close typ) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr size
{-# LINE 1322 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr seek
{-# LINE 1323 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr read'
{-# LINE 1324 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 24)) ptr write
{-# LINE 1325 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr close
{-# LINE 1326 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr typ
{-# LINE 1327 "src/SDL/Raw/Types.hsc" #-}

data Surface = Surface {
               surfaceFormat :: Ptr PixelFormat
             , surfaceW :: CInt
             , surfaceH :: CInt
             , surfacePixels :: Ptr ()
             , surfaceUserdata :: Ptr ()
             , surfaceClipRect :: Rect
             , surfaceRefcount :: CInt
             } deriving (Eq, Show)

instance Storable Surface where
	sizeOf _ = ((96))
{-# LINE 1340 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		format <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 1343 "src/SDL/Raw/Types.hsc" #-}
		w <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 1344 "src/SDL/Raw/Types.hsc" #-}
		h <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 1345 "src/SDL/Raw/Types.hsc" #-}
		pixels <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) ptr
{-# LINE 1346 "src/SDL/Raw/Types.hsc" #-}
		userdata <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) ptr
{-# LINE 1347 "src/SDL/Raw/Types.hsc" #-}
		cliprect <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) ptr
{-# LINE 1348 "src/SDL/Raw/Types.hsc" #-}
		refcount <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) ptr
{-# LINE 1349 "src/SDL/Raw/Types.hsc" #-}
		return $! Surface format w h pixels userdata cliprect refcount
	poke ptr (Surface format w h pixels userdata cliprect refcount) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr format
{-# LINE 1352 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr w
{-# LINE 1353 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr h
{-# LINE 1354 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 32)) ptr pixels
{-# LINE 1355 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 40)) ptr userdata
{-# LINE 1356 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 64)) ptr cliprect
{-# LINE 1357 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 88)) ptr refcount
{-# LINE 1358 "src/SDL/Raw/Types.hsc" #-}

data Version = Version {
               versionMajor :: Word8
             , versionMinor :: Word8
             , versionPatch :: Word8
             } deriving (Eq, Show)

instance Storable Version where
	sizeOf _ = ((3))
{-# LINE 1367 "src/SDL/Raw/Types.hsc" #-}
	alignment = sizeOf
	peek ptr = do
		major <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 1370 "src/SDL/Raw/Types.hsc" #-}
		minor <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) ptr
{-# LINE 1371 "src/SDL/Raw/Types.hsc" #-}
		patch <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) ptr
{-# LINE 1372 "src/SDL/Raw/Types.hsc" #-}
		return $! Version major minor patch
	poke ptr (Version major minor patch) = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr major
{-# LINE 1375 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 1)) ptr minor
{-# LINE 1376 "src/SDL/Raw/Types.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 2)) ptr patch
{-# LINE 1377 "src/SDL/Raw/Types.hsc" #-}
