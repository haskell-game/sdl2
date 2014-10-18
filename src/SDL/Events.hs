{-# LANGUAGE OverloadedStrings #-}
module SDL.Events
  ( Event(..)
  , EventPayload(..)
  , KeyMotion(..)
  , pollEvent
  , Raw.pumpEvents
  , waitEvent
  ) where

import Control.Applicative
import Foreign
import Foreign.C
import Linear
import Linear.Affine (Point(P))
import SDL.Raw.Types hiding (Event(..), Point)

import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

data Event = Event
  { eventTimestamp :: Word32
  , eventPayload :: EventPayload}
  deriving (Eq, Show)

data KeyMotion = KeyUp | KeyDown
  deriving (Eq,Show)

data EventPayload
  = WindowEvent {indowEventWindowID :: Word32
                ,windowEventEvent :: Word8
                ,windowEventData1 :: Int32
                ,windowEventData2 :: Int32}
  | KeyboardEvent {keyboardEventKeyMotion :: KeyMotion
                  ,keyboardEventWindowID :: Word32
                  ,keyboardEventState :: Word8
                  ,keyboardEventRepeat :: Word8
                  ,keyboardEventKeysym :: Keysym}
  | TextEditingEvent {textEditingEventWindowID :: Word32
                     ,textEditingEventText :: [CChar]
                     ,textEditingEventStart :: Int32
                     ,textEditingEventLength :: Int32}
  | TextInputEvent {textInputEventWindowID :: Word32
                   ,textInputEventText :: [CChar]}
  | MouseMotionEvent {mouseMotionEventWindowID :: Word32
                     ,mouseMotionEventWhich :: Word32
                     ,mouseMotionEventState :: Word32
                     ,mouseMotionEventPos :: Point V2 Int32
                     ,mouseMotionEventRelMotion :: V2 Int32}
  | MouseButtonEvent {mouseButtonEventWindowID :: Word32
                     ,mouseButtonEventWhich :: Word32
                     ,mouseButtonEventButton :: Word8
                     ,mouseButtonEventState :: Word8
                     ,mouseButtonEventClicks :: Word8
                     ,mouseButtonEventPos :: Point V2 Int32}
  | MouseWheelEvent {mouseWheelEventWindowID :: Word32
                    ,mouseWheelEventWhich :: Word32
                    ,mouseWheelEventPos :: Point V2 Int32}
  | JoyAxisEvent {joyAxisEventWhich :: JoystickID
                 ,joyAxisEventAxis :: Word8
                 ,joyAxisEventValue :: Int16}
  | JoyBallEvent {joyBallEventWhich :: JoystickID
                 ,joyBallEventBall :: Word8
                 ,joyBallEventRelMotion :: V2 Int16}
  | JoyHatEvent {joyHatEventWhich :: JoystickID
                ,joyHatEventHat :: Word8
                ,joyHatEventValue :: Word8}
  | JoyButtonEvent {joyButtonEventWhich :: JoystickID
                   ,joyButtonEventButton :: Word8
                   ,joyButtonEventState :: Word8}
  | JoyDeviceEvent {joyDeviceEventWhich :: Int32}
  | ControllerAxisEvent {controllerAxisEventWhich :: JoystickID
                        ,controllerAxisEventAxis :: Word8
                        ,controllerAxisEventValue :: Int16}
  | ControllerButtonEvent {controllerButtonEventWhich :: JoystickID
                          ,controllerButtonEventButton :: Word8
                          ,controllerButtonEventState :: Word8}
  | ControllerDeviceEvent {controllerDeviceEventWhich :: Int32}
  | QuitEvent
  | UserEvent {userEventWindowID :: Word32
              ,userEventCode :: Int32
              ,userEventData1 :: Ptr ()
              ,userEventData2 :: Ptr ()}
  | SysWMEvent {sysWMEventMsg :: SysWMmsg}
  | TouchFingerEvent {touchFingerEventTouchID :: TouchID
                     ,touchFingerEventFingerID :: FingerID
                     ,touchFingerEventPos :: Point V2 CFloat
                     ,touchFingerEventRelMotion :: V2 CFloat
                     ,touchFingerEventPressure :: CFloat}
  | MultiGestureEvent {multiGestureEventTouchID :: TouchID
                      ,multiGestureEventDTheta :: CFloat
                      ,multiGestureEventDDist :: CFloat
                      ,multiGestureEventPos :: Point V2 CFloat
                      ,multiGestureEventNumFingers :: Word16}
  | DollarGestureEvent {dollarGestureEventTouchID :: TouchID
                       ,dollarGestureEventGestureID :: GestureID
                       ,dollarGestureEventNumFingers :: Word32
                       ,dollarGestureEventError :: CFloat
                       ,dollagGestureEventPos :: Point V2 CFloat}
  | DropEvent {dropEventFile :: CString}
  | ClipboardUpdateEvent
  | UnknownEvent {unknownEventType :: Word32}
  deriving (Eq,Show)

convertRaw :: Raw.Event -> Event
convertRaw (Raw.WindowEvent _ ts a b c d) = Event ts (WindowEvent a b c d)
convertRaw (Raw.KeyboardEvent t ts a b c d)
  | t == Raw.eventTypeKeyDown = Event ts (KeyboardEvent KeyDown a b c d)
  | t == Raw.eventTypeKeyUp = Event ts (KeyboardEvent KeyUp a b c d)
convertRaw (Raw.TextEditingEvent _ ts a b c d) = Event ts (TextEditingEvent a b c d)
convertRaw (Raw.TextInputEvent _ ts a b) = Event ts (TextInputEvent a b)
convertRaw (Raw.MouseMotionEvent _ ts a b c d e f g) = Event ts (MouseMotionEvent a b c (P (V2 d e)) (V2 f g))
convertRaw (Raw.MouseButtonEvent _ ts a b c d e f g) = Event ts (MouseButtonEvent a b c d e (P (V2 f g)))
convertRaw (Raw.MouseWheelEvent _ ts a b c d) = Event ts (MouseWheelEvent a b (P (V2 c d)))
convertRaw (Raw.JoyAxisEvent _ ts a b c) = Event ts (JoyAxisEvent a b c)
convertRaw (Raw.JoyBallEvent _ ts a b c d) = Event ts (JoyBallEvent a b (V2 c d))
convertRaw (Raw.JoyHatEvent _ ts a b c) = Event ts (JoyHatEvent a b c)
convertRaw (Raw.JoyButtonEvent _ ts a b c) = Event ts (JoyButtonEvent a b c)
convertRaw (Raw.JoyDeviceEvent _ ts a) = Event ts (JoyDeviceEvent a)
convertRaw (Raw.ControllerAxisEvent _ ts a b c) = Event ts (ControllerAxisEvent a b c)
convertRaw (Raw.ControllerButtonEvent _ ts a b c) = Event ts (ControllerButtonEvent a b c)
convertRaw (Raw.ControllerDeviceEvent _ ts a) = Event ts (ControllerDeviceEvent a)
convertRaw (Raw.QuitEvent _ ts) = Event ts QuitEvent
convertRaw (Raw.UserEvent _ ts a b c d) = Event ts (UserEvent a b c d)
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
