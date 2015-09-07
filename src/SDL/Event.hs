{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | "SDL.Event" exports an interface for working with the SDL event model. Event handling allows your application to receive input from the user. Internally, SDL stores all the events waiting to be handled in an event queue. Using functions like 'pollEvent' and 'waitEvent' you can observe and handle waiting input events.
--
-- The event queue itself is composed of a series of 'Event' values, one for each waiting event. 'Event' values are read from the queue with the 'pollEvent' function and it is then up to the application to process the information stored with them.
module SDL.Event
  ( -- * Polling events
    pollEvent
  , pollEvents
  , mapEvents
  , pumpEvents
  , waitEvent
  , waitEventTimeout
    -- * Event data
  , Event(..)
  , EventPayload(..)
    -- ** Window events
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
  , SysWMEventData(..)
    -- ** Keyboard events
  , KeyboardEventData(..)
  , TextEditingEventData(..)
  , TextInputEventData(..)
    -- ** Mouse events
  , MouseMotionEventData(..)
  , MouseButtonEventData(..)
  , MouseWheelEventData(..)
    -- ** Joystick events
  , JoyAxisEventData(..)
  , JoyBallEventData(..)
  , JoyHatEventData(..)
  , JoyButtonEventData(..)
  , JoyDeviceEventData(..)
    -- ** Controller events
  , ControllerAxisEventData(..)
  , ControllerButtonEventData(..)
  , ControllerDeviceEventData(..)
    -- ** User events
  , UserEventData(..)
    -- ** Touch events
  , TouchFingerEventData(..)
    -- ** Gesture events
  , MultiGestureEventData(..)
  , DollarGestureEventData(..)
    -- ** Drag and drop events
  , DropEventData(..)
    -- ** Clipboard events
  , ClipboardUpdateEventData(..)
    -- ** Unknown events
  , UnknownEventData(..)
    -- * Auxiliary event data
  , InputMotion(..)
  , MouseButton(..)
  ) where

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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | A single SDL event. This event occured at 'eventTimestamp' and carries data under 'eventPayload'.
data Event = Event
  { eventTimestamp :: Word32
    -- ^ The time the event occured.
  , eventPayload :: EventPayload
    -- ^ Data pertaining to this event.
  } deriving (Eq, Ord, Generic, Show, Typeable)

-- | An enumeration of all possible SDL event types. This data type pairs up event types with
-- their payload, where possible.
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

-- | A window has been shown.
data WindowShownEventData =
  WindowShownEventData {windowShownEventWindow :: Window
                        -- ^ The associated 'Window'.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A window has been hidden.
data WindowHiddenEventData =
  WindowHiddenEventData {windowHiddenEventWindow :: Window
                         -- ^ The associated 'Window'.
                        }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A part of a window has been exposed - where exposure means to become visible (for example, an overlapping window no longer overlaps with the window).
data WindowExposedEventData =
  WindowExposedEventData {windowExposedEventWindow :: Window
                          -- ^ The associated 'Window'.
                         }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A 'Window' has been moved.
data WindowMovedEventData =
  WindowMovedEventData {windowMovedEventWindow :: Window
                        -- ^ The associated 'Window'.
                       ,windowMovedEventPosition :: Point V2 Int32
                        -- ^ The new position of the 'Window'.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Window has been resized. This is event is always preceded by 'WindowSizeChangedEvent'.
data WindowResizedEventData =
  WindowResizedEventData {windowResizedEventWindow :: Window
                          -- ^ The associated 'Window'.
                         ,windowResizedEventSize :: V2 Int32
                          -- ^ The new size of the 'Window'.
                         }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window size has changed, either as a result of an API call or through the system or user changing the window size; this event is followed by 'WindowResizedEvent' if the size was changed by an external event, i.e. the user or the window manager.
data WindowSizeChangedEventData =
  WindowSizeChangedEventData {windowSizeChangedEventWindow :: Window
                              -- ^ The associated 'Window'.
                             }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has been minimized.
data WindowMinimizedEventData =
  WindowMinimizedEventData {windowMinimizedEventWindow :: Window
                            -- ^ The associated 'Window'.
                           }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has been maximized.
data WindowMaximizedEventData =
  WindowMaximizedEventData {windowMaximizedEventWindow :: Window
                            -- ^ The associated 'Window'.
                           }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has been restored to normal size and position.
data WindowRestoredEventData =
  WindowRestoredEventData {windowRestoredEventWindow :: Window
                           -- ^ The associated 'Window'.
                          }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has gained mouse focus.
data WindowGainedMouseFocusEventData =
  WindowGainedMouseFocusEventData {windowGainedMouseFocusEventWindow :: Window
                                   -- ^ The associated 'Window'.
                                  }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has lost mouse focus.
data WindowLostMouseFocusEventData =
  WindowLostMouseFocusEventData {windowLostMouseFocusEventWindow :: Window
                                 -- ^ The associated 'Window'.
                                }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has gained keyboard focus.
data WindowGainedKeyboardFocusEventData =
  WindowGainedKeyboardFocusEventData {windowGainedKeyboardFocusEventWindow :: Window
                                      -- ^ The associated 'Window'.
                                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has lost keyboard focus.
data WindowLostKeyboardFocusEventData =
  WindowLostKeyboardFocusEventData {windowLostKeyboardFocusEventWindow :: Window
                                    -- ^ The associated 'Window'.
                                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window manager requests that the window be closed.
data WindowClosedEventData =
  WindowClosedEventData {windowClosedEventWindow :: Window
                         -- ^ The associated 'Window'.
                        }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A keyboard key has been pressed or released.
data KeyboardEventData =
  KeyboardEventData {keyboardEventWindow :: Window
                     -- ^ The associated 'Window'.
                    ,keyboardEventKeyMotion :: InputMotion
                     -- ^ Whether the key was pressed or released.
                    ,keyboardEventRepeat :: Bool
                     -- ^ 'True' if this is a repeating key press from the user holding the key down.
                    ,keyboardEventKeysym :: Keysym
                     -- ^ A description of the key that this event pertains to.
                    }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Keyboard text editing event information.
data TextEditingEventData =
  TextEditingEventData {textEditingEventWindow :: Window
                        -- ^ The associated 'Window'.
                       ,textEditingEventText :: Text
                        -- ^ The editing text.
                       ,textEditingEventStart :: Int32
                        -- ^ The location to begin editing from.
                       ,textEditingEventLength :: Int32
                        -- ^ The number of characters to edit from the start point.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Keyboard text input event information.
data TextInputEventData =
  TextInputEventData {textInputEventWindow :: Window
                      -- ^ The associated 'Window'.
                     ,textInputEventText :: Text
                      -- ^ The input text.
                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A mouse or pointer device was moved.
data MouseMotionEventData =
  MouseMotionEventData {mouseMotionEventWindow :: Window
                        -- ^ The associated 'Window'.
                       ,mouseMotionEventWhich :: MouseDevice
                        -- ^ The 'MouseDevice' that was moved.
                       ,mouseMotionEventState :: [MouseButton]
                        -- ^ A collection of 'MouseButton's that are currently held down.
                       ,mouseMotionEventPos :: Point V2 Int32
                        -- ^ The new position of the mouse.
                       ,mouseMotionEventRelMotion :: V2 Int32
                        -- ^ The relative mouse motion of the mouse.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A mouse or pointer device button was pressed or released.
data MouseButtonEventData =
  MouseButtonEventData {mouseButtonEventWindow :: Window
                        -- ^ The associated 'Window'.
                       ,mouseButtonEventMotion :: InputMotion
                        -- ^ Whether the button was pressed or released.
                       ,mouseButtonEventWhich :: MouseDevice
                        -- ^ The 'MouseDevice' whose button was pressed or released.
                       ,mouseButtonEventButton :: MouseButton
                        -- ^ The button that was pressed or released.
                       ,mouseButtonEventClicks :: Word8
                        -- ^ The amount of clicks. 1 for a single-click, 2 for a double-click, etc.
                       ,mouseButtonEventPos :: Point V2 Int32
                        -- ^ The coordinates of the mouse click.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Mouse wheel event information.
data MouseWheelEventData =
  MouseWheelEventData {mouseWheelEventWindow :: Window
                       -- ^ The associated 'Window'.
                      ,mouseWheelEventWhich :: MouseDevice
                       -- ^ The 'MouseDevice' whose wheel was scrolled.
                      ,mouseWheelEventPos :: V2 Int32
                       -- ^ The amount scrolled.
                      }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick axis motion event information
data JoyAxisEventData =
  JoyAxisEventData {joyAxisEventWhich :: Raw.JoystickID
                    -- ^ The instance id of the joystick that reported the event.
                   ,joyAxisEventAxis :: Word8
                    -- ^ The index of the axis that changed.
                   ,joyAxisEventValue :: Int16
                    -- ^ The current position of the axis, ranging between -32768 and 32767.
                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick trackball motion event information.
data JoyBallEventData =
  JoyBallEventData {joyBallEventWhich :: Raw.JoystickID
                    -- ^ The instance id of the joystick that reported the event.
                   ,joyBallEventBall :: Word8
                    -- ^ The index of the trackball that changed.
                   ,joyBallEventRelMotion :: V2 Int16
                    -- ^ The relative motion of the trackball.
                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick hat position change event information
data JoyHatEventData =
  JoyHatEventData {joyHatEventWhich :: Raw.JoystickID
                    -- ^ The instance id of the joystick that reported the event.
                  ,joyHatEventHat :: Word8
                   -- ^ The index of the hat that changed.
                  ,joyHatEventValue :: Word8
                   -- ^ The new position of the hat.
                  }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick button event information.
data JoyButtonEventData =
  JoyButtonEventData {joyButtonEventWhich :: Raw.JoystickID
                      -- ^ The instance id of the joystick that reported the event.
                     ,joyButtonEventButton :: Word8
                      -- ^ The index of the button that changed.
                     ,joyButtonEventState :: Word8
                      -- ^ The state of the button.
                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick device event information.
data JoyDeviceEventData =
  JoyDeviceEventData {joyDeviceEventWhich :: Int32
                      -- ^ The instance id of the joystick that reported the event.
                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Game controller axis motion event information.
data ControllerAxisEventData =
  ControllerAxisEventData {controllerAxisEventWhich :: Raw.JoystickID
                           -- ^ The joystick instance ID that reported the event.
                          ,controllerAxisEventAxis :: Word8
                           -- ^ The index of the axis.
                          ,controllerAxisEventValue :: Int16
                           -- ^ The axis value ranging between -32768 and 32767.
                          }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Game controller button event information
data ControllerButtonEventData =
  ControllerButtonEventData {controllerButtonEventWhich :: Raw.JoystickID
                           -- ^ The joystick instance ID that reported the event.
                            ,controllerButtonEventButton :: Word8
                             -- ^ The controller button.
                            ,controllerButtonEventState :: Word8
                             -- ^ The state of the button.
                            }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Controller device event information
data ControllerDeviceEventData =
  ControllerDeviceEventData {controllerDeviceEventWhich :: Int32
                             -- ^ The joystick instance ID that reported the event.
                            }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Event data for application-defined events.
data UserEventData =
  UserEventData {userEventWindow :: Window
                 -- ^ The associated 'Window'.
                ,userEventCode :: Int32
                 -- ^ User defined event code.
                ,userEventData1 :: Ptr ()
                 -- ^ User defined data pointer.
                ,userEventData2 :: Ptr ()
                 -- ^ User defined data pointer.
                }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A video driver dependent system event
data SysWMEventData =
  SysWMEventData {sysWMEventMsg :: Raw.SysWMmsg}
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Finger touch event information.
data TouchFingerEventData =
  TouchFingerEventData {touchFingerEventTouchID :: Raw.TouchID
                        -- ^ The touch device index.
                       ,touchFingerEventFingerID :: Raw.FingerID
                        -- ^ The finger index.
                       ,touchFingerEventPos :: Point V2 CFloat
                        -- ^ The location of the touch event, normalized between 0 and 1.
                       ,touchFingerEventRelMotion :: V2 CFloat
                        -- ^ The distance moved, normalized between -1 and 1.
                       ,touchFingerEventPressure :: CFloat
                        -- ^ The quantity of the pressure applied, normalized between 0 and 1.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Multiple finger gesture event information
data MultiGestureEventData =
  MultiGestureEventData {multiGestureEventTouchID :: Raw.TouchID
                         -- ^ The touch device index.
                        ,multiGestureEventDTheta :: CFloat
                         -- ^ The amount that the fingers rotated during this motion.
                        ,multiGestureEventDDist :: CFloat
                         -- ^ The amount that the fingers pinched during this motion.
                        ,multiGestureEventPos :: Point V2 CFloat
                         -- ^ The normalized center of the gesture.
                        ,multiGestureEventNumFingers :: Word16
                         -- ^ The number of fingers used in this gesture.
                        }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Complex gesture event information.
data DollarGestureEventData =
  DollarGestureEventData {dollarGestureEventTouchID :: Raw.TouchID
                          -- ^ The touch device index.
                         ,dollarGestureEventGestureID :: Raw.GestureID
                          -- ^ The unique id of the closest gesture to the performed stroke.
                         ,dollarGestureEventNumFingers :: Word32
                          -- ^ The number of fingers used to draw the stroke.
                         ,dollarGestureEventError :: CFloat
                          -- ^ The difference between the gesture template and the actual performed gesture (lower errors correspond to closer matches).
                         ,dollarGestureEventPos :: Point V2 CFloat
                          -- ^ The normalized center of the gesture.
                         }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | An event used to request a file open by the system
data DropEventData =
  DropEventData {dropEventFile :: CString
                 -- ^ The file name.
                }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The clipboard changed.
data ClipboardUpdateEventData =
  ClipboardUpdateEventData
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | SDL reported an unknown event type.
data UnknownEventData =
  UnknownEventData {unknownEventType :: Word32
                    -- ^ The unknown event code.
                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

data InputMotion = Released | Pressed
  deriving (Bounded, Enum, Eq, Ord, Read, Data, Generic, Show, Typeable)

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
convertRaw (Raw.KeyboardEvent Raw.SDL_KEYDOWN ts a _ c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (KeyboardEvent
                      (KeyboardEventData w'
                                         Pressed
                                         (c /= 0)
                                         (fromRawKeysym d))))
convertRaw (Raw.KeyboardEvent Raw.SDL_KEYUP ts a _ c d) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (KeyboardEvent
                      (KeyboardEventData w'
                                         Released
                                         (c /= 0)
                                         (fromRawKeysym d))))
convertRaw (Raw.KeyboardEvent _ _ _ _ _ _) = error "convertRaw: Unknown keyboard motion"
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
convertRaw (Raw.MouseButtonEvent t ts a b c _ e f g) =
  do w' <- fmap Window (Raw.getWindowFromID a)
     let motion
           | t == Raw.SDL_MOUSEBUTTONUP = Released
           | t == Raw.SDL_MOUSEBUTTONDOWN = Pressed
           | otherwise = error "convertRaw: Unexpected mouse button motion"
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

-- | Poll for currently pending events. You can only call this function in the thread that set the video mode.
pollEvent :: MonadIO m => m (Maybe Event)
pollEvent = liftIO $ alloca $ \e -> do
  n <- Raw.pollEvent e
  if n == 0
     then return Nothing
     else fmap Just (peek e >>= convertRaw)

-- | Clear the event queue by polling for all pending events.
pollEvents :: (Functor m, MonadIO m) => m [Event]
pollEvents =
  do e <- pollEvent
     case e of
       Nothing -> return []
       Just e' -> (e' :) <$> pollEvents

-- | Run a monadic computation, accumulating over all known 'Event's.
--
-- This can be useful when used with a state monad, allowing you to fold all events together.
mapEvents :: MonadIO m => (Event -> m ()) -> m ()
mapEvents h = do
  event' <- pollEvent
  case event' of
    Just event -> h event >> mapEvents h
    Nothing -> return ()

-- | Wait indefinitely for the next available event.
waitEvent :: MonadIO m => m Event
waitEvent = liftIO $ alloca $ \e -> do
  SDLEx.throwIfNeg_ "SDL.Events.waitEvent" "SDL_WaitEvent" $
    Raw.waitEvent e
  peek e >>= convertRaw

-- | Wait until the specified timeout for the next available amount.
waitEventTimeout :: MonadIO m
                 => CInt -- ^ The maximum amount of time to wait, in milliseconds.
                 -> m (Maybe Event)
waitEventTimeout timeout = liftIO $ alloca $ \e -> do
  n <- Raw.waitEventTimeout e timeout
  if n == 0
     then return Nothing
     else fmap Just (peek e >>= convertRaw)

-- | Pump the event loop, gathering events from the input devices.
--
-- This function updates the event queue and internal input device state.
--
-- This should only be run in the thread that initialized the video subsystem, and for extra safety, you should consider only doing those things on the main thread in any case.
--
-- 'pumpEvents' gathers all the pending input information from devices and places it in the event queue. Without calls to 'pumpEvents' no events would ever be placed on the queue. Often the need for calls to 'pumpEvents' is hidden from the user since 'pollEvent' and 'waitEvent' implicitly call 'pumpEvents'. However, if you are not polling or waiting for events (e.g. you are filtering them), then you must call 'pumpEvents' to force an event queue update.
--
-- See @<https://wiki.libsdl.org/SDL_PumpEvents SDL_PumpEvents>@ for C documentation.
pumpEvents :: MonadIO m => m ()
pumpEvents = Raw.pumpEvents
