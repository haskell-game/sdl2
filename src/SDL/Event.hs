{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Event
  ( Event(..)
  , EventPayload(..)
  , WindowShownEventData(..)
  , WindowHiddenEventData(..)
  , WindowExposedEventData(..)
  , WindowMovedEventData(..)
  , WindowResizedEventData(..)
  , WindowSizeChangedEventData(..)
  , WindowMinimizedEventData(..)
  , WindowMaximizedEventData(..)
  , WindowRestoredEventData(..)
  , WindowGainedMouseFocusEventData(..)
  , WindowLostMouseFocusEventData(..)
  , WindowGainedKeyboardFocusEventData(..)
  , WindowLostKeyboardFocusEventData(..)
  , WindowClosedEventData(..)
  , KeyboardEventData(..)
  , TextEditingEventData(..)
  , TextInputEventData(..)
  , MouseMotionEventData(..)
  , MouseButtonEventData(..)
  , MouseWheelEventData(..)
  , JoyAxisEventData(..)
  , JoyBallEventData(..)
  , JoyHatEventData(..)
  , JoyButtonEventData(..)
  , JoyDeviceEventData(..)
  , ControllerAxisEventData(..)
  , ControllerButtonEventData(..)
  , ControllerDeviceEventData(..)
  , UserEventData(..)
  , SysWMEventData(..)
  , TouchFingerEventData(..)
  , MultiGestureEventData(..)
  , DollarGestureEventData(..)
  , DropEventData(..)
  , ClipboardUpdateEventData(..)
  , UnknownEventData(..)
  , KeyMotion(..)
  , KeyState(..)
  , MouseButton(..)
  , MouseMotion(..)
  , pollEvent
  , pollEvents
  , mapEvents
  , Raw.pumpEvents
  , waitEvent
  , waitEventTimeout
  ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data (Data)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Typeable
import Foreign
import Foreign.C
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point(P))
import SDL.Input.Keyboard
import SDL.Input.Mouse
import SDL.Internal.Numbered
import SDL.Internal.Types (Window(Window))

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Encoding as Text
import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

-- | A single SDL event. This event occured at 'eventTimestamp' and carries data under 'eventPayload'.
data Event = Event
  { eventTimestamp :: Word32
    -- ^ The time the event occured.
  , eventPayload :: EventPayload
    -- ^ Data pertaining to this event.
  } deriving (Eq, Ord, Generic, Show, Typeable)

data EventPayload
  = WindowShownEvent WindowShownEventData
  | WindowHiddenEvent WindowHiddenEventData
  | WindowExposedEvent WindowExposedEventData
  | WindowMovedEvent WindowMovedEventData
  | WindowResizedEvent WindowResizedEventData
  | WindowSizeChangedEvent WindowSizeChangedEventData
  | WindowMinimizedEvent WindowMinimizedEventData
  | WindowMaximizedEvent WindowMaximizedEventData
  | WindowRestoredEvent WindowRestoredEventData
  | WindowGainedMouseFocusEvent WindowGainedMouseFocusEventData
  | WindowLostMouseFocusEvent WindowLostMouseFocusEventData
  | WindowGainedKeyboardFocusEvent WindowGainedKeyboardFocusEventData
  | WindowLostKeyboardFocusEvent WindowLostKeyboardFocusEventData
  | WindowClosedEvent WindowClosedEventData
  | KeyboardEvent KeyboardEventData
  | TextEditingEvent TextEditingEventData
  | TextInputEvent TextInputEventData
  | MouseMotionEvent MouseMotionEventData
  | MouseButtonEvent MouseButtonEventData
  | MouseWheelEvent MouseWheelEventData
  | JoyAxisEvent JoyAxisEventData
  | JoyBallEvent JoyBallEventData
  | JoyHatEvent JoyHatEventData
  | JoyButtonEvent JoyButtonEventData
  | JoyDeviceEvent JoyDeviceEventData
  | ControllerAxisEvent ControllerAxisEventData
  | ControllerButtonEvent ControllerButtonEventData
  | ControllerDeviceEvent ControllerDeviceEventData
  | QuitEvent
  | UserEvent UserEventData
  | SysWMEvent SysWMEventData
  | TouchFingerEvent TouchFingerEventData
  | MultiGestureEvent MultiGestureEventData
  | DollarGestureEvent DollarGestureEventData
  | DropEvent DropEventData
  | ClipboardUpdateEvent ClipboardUpdateEventData
  | UnknownEvent UnknownEventData
  deriving (Eq, Ord, Generic, Show, Typeable)

data WindowShownEventData =
  WindowShownEventData {windowShownEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowHiddenEventData =
  WindowHiddenEventData {windowHiddenEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowExposedEventData =
  WindowExposedEventData {windowExposedEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowMovedEventData =
  WindowMovedEventData {windowMovedEventWindow :: Window
                       ,windowMovedEventPosition :: Point V2 Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowResizedEventData =
  WindowResizedEventData {windowResizedEventWindow :: Window
                         ,windowResizedEventSize :: V2 Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowSizeChangedEventData =
  WindowSizeChangedEventData {windowSizeChangedEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowMinimizedEventData =
  WindowMinimizedEventData {windowMinimizedEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowMaximizedEventData =
  WindowMaximizedEventData {windowMaximizedEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowRestoredEventData =
  WindowRestoredEventData {windowRestoredEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowGainedMouseFocusEventData =
  WindowGainedMouseFocusEventData {windowGainedMouseFocusEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowLostMouseFocusEventData =
  WindowLostMouseFocusEventData {windowLostMouseFocusEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowGainedKeyboardFocusEventData =
  WindowGainedKeyboardFocusEventData {windowGainedKeyboardFocusEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowLostKeyboardFocusEventData =
  WindowLostKeyboardFocusEventData {windowLostKeyboardFocusEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data WindowClosedEventData =
  WindowClosedEventData {windowClosedEventWindow :: Window}
  deriving (Eq,Ord,Generic,Show,Typeable)

data KeyboardEventData =
  KeyboardEventData {keyboardEventWindow :: Window
                    ,keyboardEventKeyMotion :: KeyMotion
                    ,keyboardEventState :: KeyState
                    ,keyboardEventRepeat :: Bool
                    ,keyboardEventKeysym :: Keysym}
  deriving (Eq,Ord,Generic,Show,Typeable)

data TextEditingEventData =
  TextEditingEventData {textEditingEventWindow :: Window
                       ,textEditingEventText :: Text
                       ,textEditingEventStart :: Int32
                       ,textEditingEventLength :: Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data TextInputEventData =
  TextInputEventData {textInputEventWindow :: Window
                     ,textInputEventText :: Text}
  deriving (Eq,Ord,Generic,Show,Typeable)

data MouseMotionEventData =
  MouseMotionEventData {mouseMotionEventWindow :: Window
                       ,mouseMotionEventWhich :: MouseDevice
                       ,mouseMotionEventState :: [MouseButton]
                       ,mouseMotionEventPos :: Point V2 Int32
                       ,mouseMotionEventRelMotion :: V2 Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data MouseButtonEventData =
  MouseButtonEventData {mouseButtonEventWindow :: Window
                       ,mouseButtonEventMotion :: MouseMotion
                       ,mouseButtonEventWhich :: MouseDevice
                       ,mouseButtonEventButton :: MouseButton
                       ,mouseButtonEventState :: Word8
                       ,mouseButtonEventClicks :: Word8
                       ,mouseButtonEventPos :: Point V2 Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data MouseWheelEventData =
  MouseWheelEventData {mouseWheelEventWindow :: Window
                      ,mouseWheelEventWhich :: MouseDevice
                      ,mouseWheelEventPos :: V2 Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data JoyAxisEventData =
  JoyAxisEventData {joyAxisEventWhich :: Raw.JoystickID
                   ,joyAxisEventAxis :: Word8
                   ,joyAxisEventValue :: Int16}
  deriving (Eq,Ord,Generic,Show,Typeable)

data JoyBallEventData =
  JoyBallEventData {joyBallEventWhich :: Raw.JoystickID
                   ,joyBallEventBall :: Word8
                   ,joyBallEventRelMotion :: V2 Int16}
  deriving (Eq,Ord,Generic,Show,Typeable)

data JoyHatEventData =
  JoyHatEventData {joyHatEventWhich :: Raw.JoystickID
                  ,joyHatEventHat :: Word8
                  ,joyHatEventValue :: Word8}
  deriving (Eq,Ord,Generic,Show,Typeable)

data JoyButtonEventData =
  JoyButtonEventData {joyButtonEventWhich :: Raw.JoystickID
                     ,joyButtonEventButton :: Word8
                     ,joyButtonEventState :: Word8}
  deriving (Eq,Ord,Generic,Show,Typeable)

data JoyDeviceEventData =
  JoyDeviceEventData {joyDeviceEventWhich :: Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data ControllerAxisEventData =
  ControllerAxisEventData {controllerAxisEventWhich :: Raw.JoystickID
                          ,controllerAxisEventAxis :: Word8
                          ,controllerAxisEventValue :: Int16}
  deriving (Eq,Ord,Generic,Show,Typeable)

data ControllerButtonEventData =
  ControllerButtonEventData {controllerButtonEventWhich :: Raw.JoystickID
                            ,controllerButtonEventButton :: Word8
                            ,controllerButtonEventState :: Word8}
  deriving (Eq,Ord,Generic,Show,Typeable)

data ControllerDeviceEventData =
  ControllerDeviceEventData {controllerDeviceEventWhich :: Int32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data UserEventData =
  UserEventData {userEventWindow :: Window
                ,userEventCode :: Int32
                ,userEventData1 :: Ptr ()
                ,userEventData2 :: Ptr ()}
  deriving (Eq,Ord,Generic,Show,Typeable)

data SysWMEventData =
  SysWMEventData {sysWMEventMsg :: Raw.SysWMmsg}
  deriving (Eq,Ord,Generic,Show,Typeable)

data TouchFingerEventData =
  TouchFingerEventData {touchFingerEventTouchID :: Raw.TouchID
                       ,touchFingerEventFingerID :: Raw.FingerID
                       ,touchFingerEventPos :: Point V2 CFloat
                       ,touchFingerEventRelMotion :: V2 CFloat
                       ,touchFingerEventPressure :: CFloat}
  deriving (Eq,Ord,Generic,Show,Typeable)

data MultiGestureEventData =
  MultiGestureEventData {multiGestureEventTouchID :: Raw.TouchID
                        ,multiGestureEventDTheta :: CFloat
                        ,multiGestureEventDDist :: CFloat
                        ,multiGestureEventPos :: Point V2 CFloat
                        ,multiGestureEventNumFingers :: Word16}
  deriving (Eq,Ord,Generic,Show,Typeable)

data DollarGestureEventData =
  DollarGestureEventData {dollarGestureEventTouchID :: Raw.TouchID
                         ,dollarGestureEventGestureID :: Raw.GestureID
                         ,dollarGestureEventNumFingers :: Word32
                         ,dollarGestureEventError :: CFloat
                         ,dollagGestureEventPos :: Point V2 CFloat}
  deriving (Eq,Ord,Generic,Show,Typeable)

data DropEventData =
  DropEventData {dropEventFile :: CString}
  deriving (Eq,Ord,Generic,Show,Typeable)

data ClipboardUpdateEventData =
  ClipboardUpdateEventData
  deriving (Eq,Ord,Generic,Show,Typeable)

data UnknownEventData =
  UnknownEventData {unknownEventType :: Word32}
  deriving (Eq,Ord,Generic,Show,Typeable)

data KeyMotion = KeyUp | KeyDown
  deriving (Bounded, Enum, Eq, Ord, Read, Data, Generic, Show, Typeable)

data KeyState = KeyPressed | KeyReleased
  deriving (Bounded, Enum, Eq, Ord, Read, Data, Generic, Show, Typeable)

instance FromNumber KeyState Word8 where
  fromNumber n' = case n' of
    Raw.SDL_PRESSED -> KeyPressed
    Raw.SDL_RELEASED -> KeyReleased

ccharStringToText :: [CChar] -> Text
ccharStringToText = Text.decodeUtf8 . BSC8.pack . map castCCharToChar

fromRawKeysym :: Raw.Keysym -> Keysym
fromRawKeysym (Raw.Keysym scancode keycode modifier) =
  Keysym scancode' keycode' modifier'
  where scancode' = fromNumber scancode
        keycode'  = fromNumber keycode
        modifier' = fromNumber (fromIntegral modifier)

convertRaw :: Raw.Event -> IO Event
convertRaw (Raw.WindowEvent t ts a b c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (case b of
                      Raw.SDL_WINDOWEVENT_SHOWN ->
                        WindowShownEvent (WindowShownEventData w')
                      Raw.SDL_WINDOWEVENT_HIDDEN ->
                        WindowHiddenEvent (WindowHiddenEventData w')
                      Raw.SDL_WINDOWEVENT_EXPOSED ->
                        WindowExposedEvent (WindowExposedEventData w')
                      Raw.SDL_WINDOWEVENT_MOVED ->
                        WindowMovedEvent
                          (WindowMovedEventData w'
                                                (P (V2 c d)))
                      Raw.SDL_WINDOWEVENT_RESIZED ->
                        WindowResizedEvent
                          (WindowResizedEventData w'
                                                  (V2 c d))
                      Raw.SDL_WINDOWEVENT_SIZE_CHANGED ->
                        WindowSizeChangedEvent (WindowSizeChangedEventData w')
                      Raw.SDL_WINDOWEVENT_MINIMIZED ->
                        WindowMinimizedEvent (WindowMinimizedEventData w')
                      Raw.SDL_WINDOWEVENT_MAXIMIZED ->
                        WindowMaximizedEvent (WindowMaximizedEventData w')
                      Raw.SDL_WINDOWEVENT_RESTORED ->
                        WindowRestoredEvent (WindowRestoredEventData w')
                      Raw.SDL_WINDOWEVENT_ENTER ->
                        WindowGainedMouseFocusEvent (WindowGainedMouseFocusEventData w')
                      Raw.SDL_WINDOWEVENT_LEAVE ->
                        WindowLostMouseFocusEvent (WindowLostMouseFocusEventData w')
                      Raw.SDL_WINDOWEVENT_FOCUS_GAINED ->
                        WindowGainedKeyboardFocusEvent (WindowGainedKeyboardFocusEventData w')
                      Raw.SDL_WINDOWEVENT_FOCUS_LOST ->
                        WindowLostKeyboardFocusEvent (WindowLostKeyboardFocusEventData w')
                      Raw.SDL_WINDOWEVENT_CLOSE ->
                        WindowClosedEvent (WindowClosedEventData w')
                      _ ->
                        UnknownEvent (UnknownEventData t)))
convertRaw (Raw.KeyboardEvent Raw.SDL_KEYDOWN ts a b c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (KeyboardEvent
                      (KeyboardEventData w'
                                         KeyDown
                                         (fromNumber b)
                                         (c /= 0)
                                         (fromRawKeysym d))))
convertRaw (Raw.KeyboardEvent Raw.SDL_KEYUP ts a b c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (KeyboardEvent
                      (KeyboardEventData w'
                                         KeyUp
                                         (fromNumber b)
                                         (c /= 0)
                                         (fromRawKeysym d))))
convertRaw (Raw.TextEditingEvent _ ts a b c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (TextEditingEvent
                      (TextEditingEventData w'
                                            (ccharStringToText b)
                                            c
                                            d)))
convertRaw (Raw.TextInputEvent _ ts a b) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (TextInputEvent
                      (TextInputEventData w'
                                          (ccharStringToText b))))
convertRaw (Raw.MouseMotionEvent _ ts a b c d e f g) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     let buttons =
           catMaybes [(Raw.SDL_BUTTON_LMASK `test` c) ButtonLeft
                     ,(Raw.SDL_BUTTON_RMASK `test` c) ButtonRight
                     ,(Raw.SDL_BUTTON_MMASK `test` c) ButtonMiddle
                     ,(Raw.SDL_BUTTON_X1MASK `test` c) ButtonX1
                     ,(Raw.SDL_BUTTON_X2MASK `test` c) ButtonX2]
     return (Event ts
                   (MouseMotionEvent
                      (MouseMotionEventData w'
                                            (fromNumber b)
                                            buttons
                                            (P (V2 d e))
                                            (V2 f g))))
  where mask `test` x =
          if mask .&. x /= 0
             then Just
             else const Nothing
convertRaw (Raw.MouseButtonEvent t ts a b c d e f g) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     let motion
           | t == Raw.SDL_MOUSEBUTTONUP = MouseButtonUp
           | t == Raw.SDL_MOUSEBUTTONDOWN = MouseButtonDown
         button
           | c == Raw.SDL_BUTTON_LEFT = ButtonLeft
           | c == Raw.SDL_BUTTON_MIDDLE = ButtonMiddle
           | c == Raw.SDL_BUTTON_RIGHT = ButtonRight
           | c == Raw.SDL_BUTTON_X1 = ButtonX1
           | c == Raw.SDL_BUTTON_X2 = ButtonX2
           | otherwise = ButtonExtra $ fromIntegral c
     return (Event ts
                   (MouseButtonEvent
                      (MouseButtonEventData w'
                                            motion
                                            (fromNumber b)
                                            button
                                            d
                                            e
                                            (P (V2 f g)))))
convertRaw (Raw.MouseWheelEvent _ ts a b c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (MouseWheelEvent
                      (MouseWheelEventData w'
                                           (fromNumber b)
                                           (V2 c d))))
convertRaw (Raw.JoyAxisEvent _ ts a b c) =
  return (Event ts (JoyAxisEvent (JoyAxisEventData a b c)))
convertRaw (Raw.JoyBallEvent _ ts a b c d) =
  return (Event ts
                (JoyBallEvent
                   (JoyBallEventData a
                                     b
                                     (V2 c d))))
convertRaw (Raw.JoyHatEvent _ ts a b c) =
  return (Event ts (JoyHatEvent (JoyHatEventData a b c)))
convertRaw (Raw.JoyButtonEvent _ ts a b c) =
  return (Event ts (JoyButtonEvent (JoyButtonEventData a b c)))
convertRaw (Raw.JoyDeviceEvent _ ts a) =
  return (Event ts (JoyDeviceEvent (JoyDeviceEventData a)))
convertRaw (Raw.ControllerAxisEvent _ ts a b c) =
  return (Event ts (ControllerAxisEvent (ControllerAxisEventData a b c)))
convertRaw (Raw.ControllerButtonEvent _ ts a b c) =
  return (Event ts (ControllerButtonEvent (ControllerButtonEventData a b c)))
convertRaw (Raw.ControllerDeviceEvent _ ts a) =
  return (Event ts (ControllerDeviceEvent (ControllerDeviceEventData a)))
convertRaw (Raw.QuitEvent _ ts) =
  return (Event ts QuitEvent)
convertRaw (Raw.UserEvent _ ts a b c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts (UserEvent (UserEventData w' b c d)))
convertRaw (Raw.SysWMEvent _ ts a) =
  return (Event ts (SysWMEvent (SysWMEventData a)))
convertRaw (Raw.TouchFingerEvent _ ts a b c d e f g) =
  return (Event ts
                (TouchFingerEvent
                   (TouchFingerEventData a
                                         b
                                         (P (V2 c d))
                                         (V2 e f)
                                         g)))
convertRaw (Raw.MultiGestureEvent _ ts a b c d e f) =
  return (Event ts
                (MultiGestureEvent
                   (MultiGestureEventData a
                                          b
                                          c
                                          (P (V2 d e))
                                          f)))
convertRaw (Raw.DollarGestureEvent _ ts a b c d e f) =
  return (Event ts
                (DollarGestureEvent
                   (DollarGestureEventData a
                                           b
                                           c
                                           d
                                           (P (V2 e f)))))
convertRaw (Raw.DropEvent _ ts a) =
  return (Event ts (DropEvent (DropEventData a)))
convertRaw (Raw.ClipboardUpdateEvent _ ts) =
  return (Event ts (ClipboardUpdateEvent ClipboardUpdateEventData))
convertRaw (Raw.UnknownEvent t ts) =
  return (Event ts (UnknownEvent (UnknownEventData t)))

pollEvent :: MonadIO m => m (Maybe Event)
pollEvent = liftIO $ alloca $ \e -> do
  n <- Raw.pollEvent e
  if n == 0
     then return Nothing
     else fmap Just (peek e >>= convertRaw)

pollEvents :: (Functor m, MonadIO m) => m [Event]
pollEvents =
  do e <- pollEvent
     case e of
       Nothing -> return []
       Just e' -> (e' :) <$> pollEvents

mapEvents :: MonadIO m => (Event -> m ()) -> m ()
mapEvents h = do
  event' <- pollEvent
  case event' of
    Just event -> h event >> mapEvents h
    Nothing -> return ()

waitEvent :: MonadIO m => m Event
waitEvent = liftIO $ alloca $ \e -> do
  SDLEx.throwIfNeg_ "SDL.Events.waitEvent" "SDL_WaitEvent" $
    Raw.waitEvent e
  peek e >>= convertRaw

waitEventTimeout :: MonadIO m => CInt -> m (Maybe Event)
waitEventTimeout timeout = liftIO $ alloca $ \e -> do
  n <- Raw.waitEventTimeout e timeout
  if n == 0
     then return Nothing
     else fmap Just (peek e >>= convertRaw)
