{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Events
  ( Event(..)
  , EventPayload(..)
  , KeyMotion(..)
  , MouseButton(..)
  , MouseMotion(..)
  , WindowEvent(..)
  , pollEvent
  , Raw.pumpEvents
  , waitEvent
  ) where

import Control.Applicative
import Data.Text (Text)
import Foreign
import Foreign.C
import Linear
import Linear.Affine (Point(P))
import SDL.Internal.Types (WindowID(WindowID))

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Encoding as Text
import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw
import qualified SDL.Raw.Types as Raw

data Event = Event
  { eventTimestamp :: Word32
  , eventPayload :: EventPayload}
  deriving (Eq, Show)

data KeyMotion = KeyUp | KeyDown
  deriving (Eq,Show)

data KeyState = KeyPressed | KeyReleased
  deriving (Eq, Show)

cToKeyState :: (Eq a, Num a) => a -> KeyState
cToKeyState c
  | c == Raw.keyPressed = KeyPressed
  | c == Raw.keyReleased = KeyReleased

data WindowEvent
  = WindowShown
  | WindowHidden
  | WindowExposed
  | WindowMoved (Point V2 Int32)
  | WindowResized (V2 Int32)
  | WindowSizeChanged
  | WindowMinimized
  | WindowMaximized
  | WindowRestored
  | WindowGainedMouseFocus
  | WindowLostMouseFocus
  | WindowGainedKeyboardFocus
  | WindowLostKeyboardFocus
  | WindowClosed
  deriving (Eq,Show)

data MouseMotion = MouseButtonUp | MouseButtonDown
  deriving (Eq,Show)

data MouseButton = ButtonLeft | ButtonMiddle | ButtonRight | ButtonX1 | ButtonX2
  deriving (Eq,Show)

data EventPayload
  = WindowEvent {windowEventWindowID :: WindowID
                ,windowEventEvent :: WindowEvent}
  | KeyboardEvent {keyboardEventWindowID :: WindowID
                  ,keyboardEventKeyMotion :: KeyMotion
                  ,keyboardEventState :: KeyState
                  ,keyboardEventRepeat :: Bool
                  ,keyboardEventKeysym :: Raw.Keysym}
  | TextEditingEvent {textEditingEventWindowID :: WindowID
                     ,textEditingEventText :: Text
                     ,textEditingEventStart :: Int32
                     ,textEditingEventLength :: Int32}
  | TextInputEvent {textInputEventWindowID :: WindowID
                   ,textInputEventText :: Text}
  | MouseMotionEvent {mouseMotionEventWindowID :: WindowID
                     ,mouseMotionEventWhich :: Word32
                     ,mouseMotionEventState :: Word32
                     ,mouseMotionEventPos :: Point V2 Int32
                     ,mouseMotionEventRelMotion :: V2 Int32}
  | MouseButtonEvent {mouseButtonEventWindowID :: WindowID
                     ,mouseButtonEventMotion :: MouseMotion
                     ,mouseButtonEventWhich :: Word32
                     ,mouseButtonEventButton :: MouseButton
                     ,mouseButtonEventState :: Word8
                     ,mouseButtonEventClicks :: Word8
                     ,mouseButtonEventPos :: Point V2 Int32}
  | MouseWheelEvent {mouseWheelEventWindowID :: WindowID
                    ,mouseWheelEventWhich :: Word32
                    ,mouseWheelEventPos :: V2 Int32}
  | JoyAxisEvent {joyAxisEventWhich :: Raw.JoystickID
                 ,joyAxisEventAxis :: Word8
                 ,joyAxisEventValue :: Int16}
  | JoyBallEvent {joyBallEventWhich :: Raw.JoystickID
                 ,joyBallEventBall :: Word8
                 ,joyBallEventRelMotion :: V2 Int16}
  | JoyHatEvent {joyHatEventWhich :: Raw.JoystickID
                ,joyHatEventHat :: Word8
                ,joyHatEventValue :: Word8}
  | JoyButtonEvent {joyButtonEventWhich :: Raw.JoystickID
                   ,joyButtonEventButton :: Word8
                   ,joyButtonEventState :: Word8}
  | JoyDeviceEvent {joyDeviceEventWhich :: Int32}
  | ControllerAxisEvent {controllerAxisEventWhich :: Raw.JoystickID
                        ,controllerAxisEventAxis :: Word8
                        ,controllerAxisEventValue :: Int16}
  | ControllerButtonEvent {controllerButtonEventWhich :: Raw.JoystickID
                          ,controllerButtonEventButton :: Word8
                          ,controllerButtonEventState :: Word8}
  | ControllerDeviceEvent {controllerDeviceEventWhich :: Int32}
  | QuitEvent
  | UserEvent {userEventWindowID :: WindowID
              ,userEventCode :: Int32
              ,userEventData1 :: Ptr ()
              ,userEventData2 :: Ptr ()}
  | SysWMEvent {sysWMEventMsg :: Raw.SysWMmsg}
  | TouchFingerEvent {touchFingerEventTouchID :: Raw.TouchID
                     ,touchFingerEventFingerID :: Raw.FingerID
                     ,touchFingerEventPos :: Point V2 CFloat
                     ,touchFingerEventRelMotion :: V2 CFloat
                     ,touchFingerEventPressure :: CFloat}
  | MultiGestureEvent {multiGestureEventTouchID :: Raw.TouchID
                      ,multiGestureEventDTheta :: CFloat
                      ,multiGestureEventDDist :: CFloat
                      ,multiGestureEventPos :: Point V2 CFloat
                      ,multiGestureEventNumFingers :: Word16}
  | DollarGestureEvent {dollarGestureEventTouchID :: Raw.TouchID
                       ,dollarGestureEventGestureID :: Raw.GestureID
                       ,dollarGestureEventNumFingers :: Word32
                       ,dollarGestureEventError :: CFloat
                       ,dollagGestureEventPos :: Point V2 CFloat}
  | DropEvent {dropEventFile :: CString}
  | ClipboardUpdateEvent
  | UnknownEvent {unknownEventType :: Word32}
  deriving (Eq,Show)

ccharStringToText :: [CChar] -> Text
ccharStringToText = Text.decodeUtf8 . BSC8.pack . map castCCharToChar

convertRaw :: Raw.Event -> Event
convertRaw (Raw.WindowEvent _ ts a b c d)
  = Event ts $ WindowEvent (WindowID a) $
    if | b == Raw.windowEventShown -> WindowShown
       | b == Raw.windowEventHidden -> WindowHidden
       | b == Raw.windowEventExposed -> WindowExposed
       | b == Raw.windowEventMoved -> WindowMoved (P (V2 c d))
       | b == Raw.windowEventResized -> WindowResized (V2 c d)
       | b == Raw.windowEventSizeChanged -> WindowSizeChanged
       | b == Raw.windowEventMinimized -> WindowMinimized
       | b == Raw.windowEventMaximized -> WindowMaximized
       | b == Raw.windowEventRestored -> WindowRestored
       | b == Raw.windowEventEnter -> WindowGainedMouseFocus
       | b == Raw.windowEventLeave -> WindowLostMouseFocus
       | b == Raw.windowEventFocusGained -> WindowGainedKeyboardFocus
       | b == Raw.windowEventFocusLost -> WindowLostKeyboardFocus
       | b == Raw.windowEventClose -> WindowClosed
convertRaw (Raw.KeyboardEvent t ts a b c d)
  = let motion | t == Raw.eventTypeKeyDown = KeyDown
               | t == Raw.eventTypeKeyUp = KeyUp
    in Event ts (KeyboardEvent (WindowID a) motion (cToKeyState b) (c /= 0) d)
convertRaw (Raw.TextEditingEvent _ ts a b c d) = Event ts (TextEditingEvent (WindowID a) (ccharStringToText b) c d)
convertRaw (Raw.TextInputEvent _ ts a b) = Event ts (TextInputEvent (WindowID a) (ccharStringToText b))
convertRaw (Raw.MouseMotionEvent _ ts a b c d e f g) = Event ts (MouseMotionEvent (WindowID a) b c (P (V2 d e)) (V2 f g))
convertRaw (Raw.MouseButtonEvent t ts a b c d e f g)
  = let motion | t == Raw.eventTypeMouseButtonUp = MouseButtonUp
               | t == Raw.eventTypeMouseButtonDown = MouseButtonDown
        button | c == Raw.buttonLeft = ButtonLeft
               | c == Raw.buttonMiddle = ButtonMiddle
               | c == Raw.buttonRight = ButtonRight
               | c == Raw.buttonX1 = ButtonX1
               | c == Raw.buttonX2 = ButtonX2
    in Event ts (MouseButtonEvent (WindowID a) motion b button d e (P (V2 f g)))
convertRaw (Raw.MouseWheelEvent _ ts a b c d) = Event ts (MouseWheelEvent (WindowID a) b (V2 c d))
convertRaw (Raw.JoyAxisEvent _ ts a b c) = Event ts (JoyAxisEvent a b c)
convertRaw (Raw.JoyBallEvent _ ts a b c d) = Event ts (JoyBallEvent a b (V2 c d))
convertRaw (Raw.JoyHatEvent _ ts a b c) = Event ts (JoyHatEvent a b c)
convertRaw (Raw.JoyButtonEvent _ ts a b c) = Event ts (JoyButtonEvent a b c)
convertRaw (Raw.JoyDeviceEvent _ ts a) = Event ts (JoyDeviceEvent a)
convertRaw (Raw.ControllerAxisEvent _ ts a b c) = Event ts (ControllerAxisEvent a b c)
convertRaw (Raw.ControllerButtonEvent _ ts a b c) = Event ts (ControllerButtonEvent a b c)
convertRaw (Raw.ControllerDeviceEvent _ ts a) = Event ts (ControllerDeviceEvent a)
convertRaw (Raw.QuitEvent _ ts) = Event ts QuitEvent
convertRaw (Raw.UserEvent _ ts a b c d) = Event ts (UserEvent (WindowID a) b c d)
convertRaw (Raw.SysWMEvent _ ts a) = Event ts (SysWMEvent a)
convertRaw (Raw.TouchFingerEvent _ ts a b c d e f g) = Event ts (TouchFingerEvent a b (P (V2 c d)) (V2 e f) g)
convertRaw (Raw.MultiGestureEvent _ ts a b c d e f) = Event ts (MultiGestureEvent a b c (P (V2 d e)) f)
convertRaw (Raw.DollarGestureEvent _ ts a b c d e f) = Event ts (DollarGestureEvent a b c d (P (V2 e f)))
convertRaw (Raw.DropEvent _ ts a) = Event ts (DropEvent a)
convertRaw (Raw.ClipboardUpdateEvent _ ts) = Event ts ClipboardUpdateEvent
convertRaw (Raw.UnknownEvent t ts) = Event ts (UnknownEvent t)

pollEvent :: IO (Maybe Event)
pollEvent = alloca $ \e -> do
  n <- Raw.pollEvent e
  if n == 0
     then return Nothing
     else Just . convertRaw <$> peek e

waitEvent :: IO Event
waitEvent = alloca $ \e -> do
  SDLEx.throwIfNeg_ "SDL.Events.waitEvent" "SDL_WaitEvent" $
    Raw.waitEvent e
  convertRaw <$> peek e
