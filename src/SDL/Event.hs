{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Event
  ( Event(..)
  , EventPayload(..)
  , KeyMotion(..)
  , KeyState(..)
  , MouseButton(..)
  , MouseMotion(..)
  , WindowID
  , pollEvent
  , Raw.pumpEvents
  , waitEvent
  , waitEventTimeout
  ) where

import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Typeable
import Foreign
import Foreign.C
import Linear
import Linear.Affine (Point(P))
import SDL.Internal.Numbered
import SDL.Internal.Types (WindowID(WindowID))
import SDL.Input.Keyboard
import SDL.Input.Mouse

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Encoding as Text
import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

data Event = Event
  { eventTimestamp :: Word32
  , eventPayload :: EventPayload
  } deriving (Eq, Show, Typeable)

data KeyMotion = KeyUp | KeyDown
  deriving (Eq, Show, Typeable)

data KeyState = KeyPressed | KeyReleased
  deriving (Eq, Show, Typeable)

instance FromNumber KeyState Word8 where
  fromNumber n' = case n' of
    n | n == Raw.keyPressed -> KeyReleased
    n | n == Raw.keyReleased -> KeyReleased

data EventPayload
  = WindowShown
    { windowEventWindowID :: WindowID
    }
  | WindowHidden
    { windowEventWindowID :: WindowID
    }
  | WindowExposed
    { windowEventWindowID :: WindowID
    }
  | WindowMoved
    { windowEventWindowID :: WindowID
    , windowEventPosition :: Point V2 Int32
    }
  | WindowResized
    { windowEventWindowID :: WindowID
    , windowEventSize :: V2 Int32
    }
  | WindowSizeChanged
    { windowEventWindowID :: WindowID
    }
  | WindowMinimized
    { windowEventWindowID :: WindowID
    }
  | WindowMaximized
    { windowEventWindowID :: WindowID
    }
  | WindowRestored
    { windowEventWindowID :: WindowID
    }
  | WindowGainedMouseFocus
    { windowEventWindowID :: WindowID
    }
  | WindowLostMouseFocus
    { windowEventWindowID :: WindowID
    }
  | WindowGainedKeyboardFocus
    { windowEventWindowID :: WindowID
    }
  | WindowLostKeyboardFocus
    { windowEventWindowID :: WindowID
    }
  | WindowClosed
    { windowEventWindowID :: WindowID
    }
  | KeyboardEvent
    { keyboardEventWindowID :: WindowID
    , keyboardEventKeyMotion :: KeyMotion
    , keyboardEventState :: KeyState
    , keyboardEventRepeat :: Bool
    , keyboardEventKeysym :: Keysym
    }
  | TextEditingEvent
    { textEditingEventWindowID :: WindowID
    , textEditingEventText :: Text
    , textEditingEventStart :: Int32
    , textEditingEventLength :: Int32
    }
  | TextInputEvent
    { textInputEventWindowID :: WindowID
    , textInputEventText :: Text
    }
  | MouseMotionEvent
    { mouseMotionEventWindowID :: WindowID
    , mouseMotionEventWhich :: MouseDevice
    , mouseMotionEventState :: [MouseButton]
    , mouseMotionEventPos :: Point V2 Int32
    , mouseMotionEventRelMotion :: V2 Int32
    }
  | MouseButtonEvent
    { mouseButtonEventWindowID :: WindowID
    , mouseButtonEventMotion :: MouseMotion
    , mouseButtonEventWhich :: MouseDevice
    , mouseButtonEventButton :: MouseButton
    , mouseButtonEventState :: Word8
    , mouseButtonEventClicks :: Word8
    , mouseButtonEventPos :: Point V2 Int32
    }
  | MouseWheelEvent
    { mouseWheelEventWindowID :: WindowID
    , mouseWheelEventWhich :: MouseDevice
    , mouseWheelEventPos :: V2 Int32
    }
  | JoyAxisEvent
    { joyAxisEventWhich :: Raw.JoystickID
    , joyAxisEventAxis :: Word8
    , joyAxisEventValue :: Int16
    }
  | JoyBallEvent
    { joyBallEventWhich :: Raw.JoystickID
    , joyBallEventBall :: Word8
    , joyBallEventRelMotion :: V2 Int16
    }
  | JoyHatEvent
    { joyHatEventWhich :: Raw.JoystickID
    , joyHatEventHat :: Word8
    , joyHatEventValue :: Word8
    }
  | JoyButtonEvent
    { joyButtonEventWhich :: Raw.JoystickID
    , joyButtonEventButton :: Word8
    , joyButtonEventState :: Word8
    }
  | JoyDeviceEvent
    { joyDeviceEventWhich :: Int32
    }
  | ControllerAxisEvent
    { controllerAxisEventWhich :: Raw.JoystickID
    , controllerAxisEventAxis :: Word8
    , controllerAxisEventValue :: Int16
    }
  | ControllerButtonEvent
    { controllerButtonEventWhich :: Raw.JoystickID
    , controllerButtonEventButton :: Word8
    , controllerButtonEventState :: Word8
    }
  | ControllerDeviceEvent
    { controllerDeviceEventWhich :: Int32
    }
  | QuitEvent
  | UserEvent
    { userEventWindowID :: WindowID
    , userEventCode :: Int32
    , userEventData1 :: Ptr ()
    , userEventData2 :: Ptr ()
    }
  | SysWMEvent
    { sysWMEventMsg :: Raw.SysWMmsg
    }
  | TouchFingerEvent
    { touchFingerEventTouchID :: Raw.TouchID
    , touchFingerEventFingerID :: Raw.FingerID
    , touchFingerEventPos :: Point V2 CFloat
    , touchFingerEventRelMotion :: V2 CFloat
    , touchFingerEventPressure :: CFloat
    }
  | MultiGestureEvent
    { multiGestureEventTouchID :: Raw.TouchID
    , multiGestureEventDTheta :: CFloat
    , multiGestureEventDDist :: CFloat
    , multiGestureEventPos :: Point V2 CFloat
    , multiGestureEventNumFingers :: Word16
    }
  | DollarGestureEvent
    { dollarGestureEventTouchID :: Raw.TouchID
    , dollarGestureEventGestureID :: Raw.GestureID
    , dollarGestureEventNumFingers :: Word32
    , dollarGestureEventError :: CFloat
    , dollagGestureEventPos :: Point V2 CFloat
    }
  | DropEvent
    { dropEventFile :: CString
    }
  | ClipboardUpdateEvent
  | UnknownEvent
    { unknownEventType :: Word32
    }
  deriving (Eq, Show, Typeable)

ccharStringToText :: [CChar] -> Text
ccharStringToText = Text.decodeUtf8 . BSC8.pack . map castCCharToChar

fromRawKeysym :: Raw.Keysym -> Keysym
fromRawKeysym (Raw.Keysym scancode keycode modifier) =
  Keysym scancode' keycode' modifier'
  where scancode' = fromNumber scancode
        keycode'  = fromNumber keycode
        modifier' = fromNumber (fromIntegral modifier)

convertRaw :: Raw.Event -> Event
convertRaw (Raw.WindowEvent t ts a b c d) = Event ts $
  let w' = WindowID a in case b of
    n | n == Raw.windowEventShown -> WindowShown w'
    n | n == Raw.windowEventHidden -> WindowHidden w'
    n | n == Raw.windowEventExposed -> WindowExposed w'
    n | n == Raw.windowEventMoved -> WindowMoved w' (P (V2 c d))
    n | n == Raw.windowEventResized -> WindowResized w' (V2 c d)
    n | n == Raw.windowEventSizeChanged -> WindowSizeChanged w'
    n | n == Raw.windowEventMinimized -> WindowMinimized w'
    n | n == Raw.windowEventMaximized -> WindowMaximized w'
    n | n == Raw.windowEventRestored -> WindowRestored w'
    n | n == Raw.windowEventEnter -> WindowGainedMouseFocus w'
    n | n == Raw.windowEventLeave -> WindowLostMouseFocus w'
    n | n == Raw.windowEventFocusGained -> WindowGainedKeyboardFocus w'
    n | n == Raw.windowEventFocusLost -> WindowLostKeyboardFocus w'
    n | n == Raw.windowEventClose -> WindowClosed w'
    _ -> UnknownEvent t
convertRaw (Raw.KeyboardEvent t ts a b c d)
  = let motion | t == Raw.eventTypeKeyDown = KeyDown
               | t == Raw.eventTypeKeyUp = KeyUp
    in Event ts (KeyboardEvent (WindowID a) motion (fromNumber b) (c /= 0) (fromRawKeysym d))
convertRaw (Raw.TextEditingEvent _ ts a b c d) = Event ts (TextEditingEvent (WindowID a) (ccharStringToText b) c d)
convertRaw (Raw.TextInputEvent _ ts a b) = Event ts (TextInputEvent (WindowID a) (ccharStringToText b))
convertRaw (Raw.MouseMotionEvent _ ts a b c d e f g)
  = let buttons = catMaybes
                  [ (Raw.buttonLMask `test` c) ButtonLeft
                  , (Raw.buttonRMask `test` c) ButtonRight
                  , (Raw.buttonMMask `test` c) ButtonMiddle
                  , (Raw.buttonX1Mask `test` c) ButtonX1
                  , (Raw.buttonX2Mask `test` c) ButtonX2 ]
     in Event ts (MouseMotionEvent (WindowID a) (fromNumber b) buttons (P (V2 d e)) (V2 f g))
  where mask `test` x = if mask .&. x /= 0 then Just else const Nothing
convertRaw (Raw.MouseButtonEvent t ts a b c d e f g)
  = let motion | t == Raw.eventTypeMouseButtonUp = MouseButtonUp
               | t == Raw.eventTypeMouseButtonDown = MouseButtonDown
        button | c == Raw.buttonLeft = ButtonLeft
               | c == Raw.buttonMiddle = ButtonMiddle
               | c == Raw.buttonRight = ButtonRight
               | c == Raw.buttonX1 = ButtonX1
               | c == Raw.buttonX2 = ButtonX2
               | otherwise = ButtonExtra $ fromIntegral c
    in Event ts (MouseButtonEvent (WindowID a) motion (fromNumber b) button d e (P (V2 f g)))
convertRaw (Raw.MouseWheelEvent _ ts a b c d) = Event ts (MouseWheelEvent (WindowID a) (fromNumber b) (V2 c d))
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

waitEventTimeout :: CInt -> IO (Maybe Event)
waitEventTimeout timeout = alloca $ \e -> do
  n <- Raw.waitEventTimeout e timeout
  if n == 0
     then return Nothing
     else Just . convertRaw <$> peek e
