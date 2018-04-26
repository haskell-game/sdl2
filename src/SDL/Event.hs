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
    -- * Registering user events
  , RegisteredEventType(..)
  , RegisteredEventData(..)
  , EventPushResult(..)
  , emptyRegisteredEvent
  , registerEvent
    -- * Watching events
  , EventWatchCallback
  , EventWatch
  , addEventWatch
  , delEventWatch
    -- * Event data
  , Event(..)
  , Timestamp
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
    -- ** Audio events
  , AudioDeviceEventData(..)
    -- ** User events
  , UserEventData(..)
    -- ** Touch events
  , TouchFingerEventData(..)
  , TouchFingerMotionEventData(..)
    -- ** Gesture events
  , MultiGestureEventData(..)
  , DollarGestureEventData(..)
    -- ** Drag and drop events
  , DropEventData(..)
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
import Foreign hiding (throwIfNeg_)
import Foreign.C
import GHC.Generics (Generic)
import SDL.Vect
import SDL.Input.Joystick
import SDL.Input.GameController
import SDL.Input.Keyboard
import SDL.Input.Mouse
import SDL.Internal.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types (Window(Window))
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Encoding as Text
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | A single SDL event. This event occured at 'eventTimestamp' and carries data under 'eventPayload'.
data Event = Event
  { eventTimestamp :: Timestamp
    -- ^ The time the event occured.
  , eventPayload :: EventPayload
    -- ^ Data pertaining to this event.
  } deriving (Eq, Ord, Generic, Show, Typeable)

type Timestamp = Word32

-- | An enumeration of all possible SDL event types. This data type pairs up event types with
-- their payload, where possible.
data EventPayload
  = WindowShownEvent !WindowShownEventData
  | WindowHiddenEvent !WindowHiddenEventData
  | WindowExposedEvent !WindowExposedEventData
  | WindowMovedEvent !WindowMovedEventData
  | WindowResizedEvent !WindowResizedEventData
  | WindowSizeChangedEvent !WindowSizeChangedEventData
  | WindowMinimizedEvent !WindowMinimizedEventData
  | WindowMaximizedEvent !WindowMaximizedEventData
  | WindowRestoredEvent !WindowRestoredEventData
  | WindowGainedMouseFocusEvent !WindowGainedMouseFocusEventData
  | WindowLostMouseFocusEvent !WindowLostMouseFocusEventData
  | WindowGainedKeyboardFocusEvent !WindowGainedKeyboardFocusEventData
  | WindowLostKeyboardFocusEvent !WindowLostKeyboardFocusEventData
  | WindowClosedEvent !WindowClosedEventData
  | KeyboardEvent !KeyboardEventData
  | TextEditingEvent !TextEditingEventData
  | TextInputEvent !TextInputEventData
  | KeymapChangedEvent
  | MouseMotionEvent !MouseMotionEventData
  | MouseButtonEvent !MouseButtonEventData
  | MouseWheelEvent !MouseWheelEventData
  | JoyAxisEvent !JoyAxisEventData
  | JoyBallEvent !JoyBallEventData
  | JoyHatEvent !JoyHatEventData
  | JoyButtonEvent !JoyButtonEventData
  | JoyDeviceEvent !JoyDeviceEventData
  | ControllerAxisEvent !ControllerAxisEventData
  | ControllerButtonEvent !ControllerButtonEventData
  | ControllerDeviceEvent !ControllerDeviceEventData
  | AudioDeviceEvent !AudioDeviceEventData
  | QuitEvent
  | UserEvent !UserEventData
  | SysWMEvent !SysWMEventData
  | TouchFingerEvent !TouchFingerEventData
  | TouchFingerMotionEvent !TouchFingerMotionEventData
  | MultiGestureEvent !MultiGestureEventData
  | DollarGestureEvent !DollarGestureEventData
  | DropEvent !DropEventData
  | ClipboardUpdateEvent
  | UnknownEvent !UnknownEventData
  deriving (Eq, Ord, Generic, Show, Typeable)

-- | A window has been shown.
newtype WindowShownEventData =
  WindowShownEventData {windowShownEventWindow :: Window
                        -- ^ The associated 'Window'.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A window has been hidden.
newtype WindowHiddenEventData =
  WindowHiddenEventData {windowHiddenEventWindow :: Window
                         -- ^ The associated 'Window'.
                        }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A part of a window has been exposed - where exposure means to become visible (for example, an overlapping window no longer overlaps with the window).
newtype WindowExposedEventData =
  WindowExposedEventData {windowExposedEventWindow :: Window
                          -- ^ The associated 'Window'.
                         }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A 'Window' has been moved.
data WindowMovedEventData =
  WindowMovedEventData {windowMovedEventWindow :: !Window
                        -- ^ The associated 'Window'.
                       ,windowMovedEventPosition :: !(Point V2 Int32)
                        -- ^ The new position of the 'Window'.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Window has been resized. This is event is always preceded by 'WindowSizeChangedEvent'.
data WindowResizedEventData =
  WindowResizedEventData {windowResizedEventWindow :: !Window
                          -- ^ The associated 'Window'.
                         ,windowResizedEventSize :: !(V2 Int32)
                          -- ^ The new size of the 'Window'.
                         }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window size has changed, either as a result of an API call or through the system or user changing the window size; this event is followed by 'WindowResizedEvent' if the size was changed by an external event, i.e. the user or the window manager.
data WindowSizeChangedEventData =
  WindowSizeChangedEventData {windowSizeChangedEventWindow :: !Window
                              -- ^ The associated 'Window'.
                             ,windowSizeChangedEventSize :: !(V2 Int32)
                              -- ^ The new size of the 'Window'.
                             }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has been minimized.
newtype WindowMinimizedEventData =
  WindowMinimizedEventData {windowMinimizedEventWindow :: Window
                            -- ^ The associated 'Window'.
                           }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has been maximized.
newtype WindowMaximizedEventData =
  WindowMaximizedEventData {windowMaximizedEventWindow :: Window
                            -- ^ The associated 'Window'.
                           }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has been restored to normal size and position.
newtype WindowRestoredEventData =
  WindowRestoredEventData {windowRestoredEventWindow :: Window
                           -- ^ The associated 'Window'.
                          }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has gained mouse focus.
newtype WindowGainedMouseFocusEventData =
  WindowGainedMouseFocusEventData {windowGainedMouseFocusEventWindow :: Window
                                   -- ^ The associated 'Window'.
                                  }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has lost mouse focus.
newtype WindowLostMouseFocusEventData =
  WindowLostMouseFocusEventData {windowLostMouseFocusEventWindow :: Window
                                 -- ^ The associated 'Window'.
                                }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has gained keyboard focus.
newtype WindowGainedKeyboardFocusEventData =
  WindowGainedKeyboardFocusEventData {windowGainedKeyboardFocusEventWindow :: Window
                                      -- ^ The associated 'Window'.
                                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window has lost keyboard focus.
newtype WindowLostKeyboardFocusEventData =
  WindowLostKeyboardFocusEventData {windowLostKeyboardFocusEventWindow :: Window
                                    -- ^ The associated 'Window'.
                                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | The window manager requests that the window be closed.
newtype WindowClosedEventData =
  WindowClosedEventData {windowClosedEventWindow :: Window
                         -- ^ The associated 'Window'.
                        }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A keyboard key has been pressed or released.
data KeyboardEventData =
  KeyboardEventData {keyboardEventWindow :: !(Maybe Window)
                     -- ^ The 'Window' with keyboard focus, if any.
                    ,keyboardEventKeyMotion :: !InputMotion
                     -- ^ Whether the key was pressed or released.
                    ,keyboardEventRepeat :: !Bool
                     -- ^ 'True' if this is a repeating key press from the user holding the key down.
                    ,keyboardEventKeysym :: !Keysym
                     -- ^ A description of the key that this event pertains to.
                    }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Keyboard text editing event information.
data TextEditingEventData =
  TextEditingEventData {textEditingEventWindow :: !(Maybe Window)
                        -- ^ The 'Window' with keyboard focus, if any.
                       ,textEditingEventText :: !Text
                        -- ^ The editing text.
                       ,textEditingEventStart :: !Int32
                        -- ^ The location to begin editing from.
                       ,textEditingEventLength :: !Int32
                        -- ^ The number of characters to edit from the start point.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Keyboard text input event information.
data TextInputEventData =
  TextInputEventData {textInputEventWindow :: !(Maybe Window)
                      -- ^ The 'Window' with keyboard focus, if any.
                     ,textInputEventText :: !Text
                      -- ^ The input text.
                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A mouse or pointer device was moved.
data MouseMotionEventData =
  MouseMotionEventData {mouseMotionEventWindow :: !(Maybe Window)
                        -- ^ The 'Window' with mouse focus, if any.
                       ,mouseMotionEventWhich :: !MouseDevice
                        -- ^ The 'MouseDevice' that was moved.
                       ,mouseMotionEventState :: ![MouseButton]
                        -- ^ A collection of 'MouseButton's that are currently held down.
                       ,mouseMotionEventPos :: !(Point V2 Int32)
                        -- ^ The new position of the mouse.
                       ,mouseMotionEventRelMotion :: !(V2 Int32)
                        -- ^ The relative mouse motion of the mouse.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A mouse or pointer device button was pressed or released.
data MouseButtonEventData =
  MouseButtonEventData {mouseButtonEventWindow :: !(Maybe Window)
                        -- ^ The 'Window' with mouse focus, if any.
                       ,mouseButtonEventMotion :: !InputMotion
                        -- ^ Whether the button was pressed or released.
                       ,mouseButtonEventWhich :: !MouseDevice
                        -- ^ The 'MouseDevice' whose button was pressed or released.
                       ,mouseButtonEventButton :: !MouseButton
                        -- ^ The button that was pressed or released.
                       ,mouseButtonEventClicks :: !Word8
                        -- ^ The amount of clicks. 1 for a single-click, 2 for a double-click, etc.
                       ,mouseButtonEventPos :: !(Point V2 Int32)
                        -- ^ The coordinates of the mouse click.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Mouse wheel event information.
data MouseWheelEventData =
  MouseWheelEventData {mouseWheelEventWindow :: !(Maybe Window)
                        -- ^ The 'Window' with mouse focus, if any.
                      ,mouseWheelEventWhich :: !MouseDevice
                       -- ^ The 'MouseDevice' whose wheel was scrolled.
                      ,mouseWheelEventPos :: !(V2 Int32)
                       -- ^ The amount scrolled.
                      ,mouseWheelEventDirection :: !MouseScrollDirection
                       -- ^ The scroll direction mode.
                      }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick axis motion event information
data JoyAxisEventData =
  JoyAxisEventData {joyAxisEventWhich :: !Raw.JoystickID
                    -- ^ The instance id of the joystick that reported the event.
                   ,joyAxisEventAxis :: !Word8
                    -- ^ The index of the axis that changed.
                   ,joyAxisEventValue :: !Int16
                    -- ^ The current position of the axis, ranging between -32768 and 32767.
                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick trackball motion event information.
data JoyBallEventData =
  JoyBallEventData {joyBallEventWhich :: !Raw.JoystickID
                    -- ^ The instance id of the joystick that reported the event.
                   ,joyBallEventBall :: !Word8
                    -- ^ The index of the trackball that changed.
                   ,joyBallEventRelMotion :: !(V2 Int16)
                    -- ^ The relative motion of the trackball.
                   }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick hat position change event information
data JoyHatEventData =
  JoyHatEventData {joyHatEventWhich :: !Raw.JoystickID
                    -- ^ The instance id of the joystick that reported the event.
                  ,joyHatEventHat :: !Word8
                   -- ^ The index of the hat that changed.
                  ,joyHatEventValue :: !JoyHatPosition
                   -- ^ The new position of the hat.
                  }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick button event information.
data JoyButtonEventData =
  JoyButtonEventData {joyButtonEventWhich :: !Raw.JoystickID
                      -- ^ The instance id of the joystick that reported the event.
                     ,joyButtonEventButton :: !Word8
                      -- ^ The index of the button that changed.
                     ,joyButtonEventState :: !JoyButtonState
                      -- ^ The state of the button.
                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Joystick device event information.
data JoyDeviceEventData =
  JoyDeviceEventData {joyDeviceEventConnection :: !JoyDeviceConnection
                      -- ^ Was the device added or removed?
                     ,joyDeviceEventWhich :: !Int32
                      -- ^ The instance id of the joystick that reported the event.
                     }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Game controller axis motion event information.
data ControllerAxisEventData =
  ControllerAxisEventData {controllerAxisEventWhich :: !Raw.JoystickID
                           -- ^ The joystick instance ID that reported the event.
                          ,controllerAxisEventAxis :: !Word8
                           -- ^ The index of the axis.
                          ,controllerAxisEventValue :: !Int16
                           -- ^ The axis value ranging between -32768 and 32767.
                          }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Game controller button event information
data ControllerButtonEventData =
  ControllerButtonEventData {controllerButtonEventWhich :: !Raw.JoystickID
                           -- ^ The joystick instance ID that reported the event.
                            ,controllerButtonEventButton :: !ControllerButton
                             -- ^ The controller button.
                            ,controllerButtonEventState :: !ControllerButtonState
                             -- ^ The state of the button.
                            }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Controller device event information
data ControllerDeviceEventData =
  ControllerDeviceEventData {controllerDeviceEventConnection :: !ControllerDeviceConnection
                             -- ^ Was the device added, removed, or remapped?
                            ,controllerDeviceEventWhich :: !Int32
                             -- ^ The joystick instance ID that reported the event.
                            }
  deriving (Eq,Ord,Generic,Show,Typeable)

data AudioDeviceEventData =
  AudioDeviceEventData {audioDeviceEventIsAddition :: !Bool
                        -- ^ If the audio device is an addition, or a removal.
                       ,audioDeviceEventWhich :: !Word32
                        -- ^ The audio device ID that reported the event.
                       ,audioDeviceEventIsCapture :: !Bool
                        -- ^ If the audio device is a capture device.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Event data for application-defined events.
data UserEventData =
  UserEventData {userEventType :: !Word32
                 -- ^ User defined event type.
                ,userEventWindow :: !(Maybe Window)
                 -- ^ The associated 'Window'.
                ,userEventCode :: !Int32
                 -- ^ User defined event code.
                ,userEventData1 :: !(Ptr ())
                 -- ^ User defined data pointer.
                ,userEventData2 :: !(Ptr ())
                 -- ^ User defined data pointer.
                }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A video driver dependent system event
newtype SysWMEventData =
  SysWMEventData {sysWMEventMsg :: Raw.SysWMmsg}
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Finger touch event information.
data TouchFingerEventData =
  TouchFingerEventData {touchFingerEventTouchID :: !Raw.TouchID
                        -- ^ The touch device index.
                       ,touchFingerEventFingerID :: !Raw.FingerID
                        -- ^ The finger index.
                       ,touchFingerEventMotion :: !InputMotion
                        -- ^ Whether the finger was pressed or released.
                       ,touchFingerEventPos :: !(Point V2 CFloat)
                        -- ^ The location of the touch event, normalized between 0 and 1.
                       ,touchFingerEventPressure :: !CFloat
                        -- ^ The quantity of the pressure applied, normalized between 0 and 1.
                       }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Finger motion event information.
data TouchFingerMotionEventData =
  TouchFingerMotionEventData {touchFingerMotionEventTouchID :: !Raw.TouchID
                              -- ^ The touch device index.
                             ,touchFingerMotionEventFingerID :: !Raw.FingerID
                              -- ^ The finger index.
                             ,touchFingerMotionEventPos :: !(Point V2 CFloat)
                              -- ^ The location of the touch event, normalized between 0 and 1.
                             ,touchFingerMotionEventRelMotion :: !(V2 CFloat)
                              -- ^ The distance moved, normalized between -1 and 1.
                             ,touchFingerMotionEventPressure :: !CFloat
                              -- ^ The quantity of the pressure applied, normalized between 0 and 1.
                             }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Multiple finger gesture event information
data MultiGestureEventData =
  MultiGestureEventData {multiGestureEventTouchID :: !Raw.TouchID
                         -- ^ The touch device index.
                        ,multiGestureEventDTheta :: !CFloat
                         -- ^ The amount that the fingers rotated during this motion.
                        ,multiGestureEventDDist :: !CFloat
                         -- ^ The amount that the fingers pinched during this motion.
                        ,multiGestureEventPos :: !(Point V2 CFloat)
                         -- ^ The normalized center of the gesture.
                        ,multiGestureEventNumFingers :: !Word16
                         -- ^ The number of fingers used in this gesture.
                        }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | Complex gesture event information.
data DollarGestureEventData =
  DollarGestureEventData {dollarGestureEventTouchID :: !Raw.TouchID
                          -- ^ The touch device index.
                         ,dollarGestureEventGestureID :: !Raw.GestureID
                          -- ^ The unique id of the closest gesture to the performed stroke.
                         ,dollarGestureEventNumFingers :: !Word32
                          -- ^ The number of fingers used to draw the stroke.
                         ,dollarGestureEventError :: !CFloat
                          -- ^ The difference between the gesture template and the actual performed gesture (lower errors correspond to closer matches).
                         ,dollarGestureEventPos :: !(Point V2 CFloat)
                          -- ^ The normalized center of the gesture.
                         }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | An event used to request a file open by the system
newtype DropEventData =
  DropEventData {dropEventFile :: CString
                 -- ^ The file name.
                }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | SDL reported an unknown event type.
newtype UnknownEventData =
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
  do w <- fmap Window (Raw.getWindowFromID a)
     return (Event ts
                   (case b of
                      Raw.SDL_WINDOWEVENT_SHOWN ->
                        WindowShownEvent (WindowShownEventData w)
                      Raw.SDL_WINDOWEVENT_HIDDEN ->
                        WindowHiddenEvent (WindowHiddenEventData w)
                      Raw.SDL_WINDOWEVENT_EXPOSED ->
                        WindowExposedEvent (WindowExposedEventData w)
                      Raw.SDL_WINDOWEVENT_MOVED ->
                        WindowMovedEvent
                          (WindowMovedEventData w
                                                (P (V2 c d)))
                      Raw.SDL_WINDOWEVENT_RESIZED ->
                        WindowResizedEvent
                          (WindowResizedEventData w
                                                  (V2 c d))
                      Raw.SDL_WINDOWEVENT_SIZE_CHANGED ->
                        WindowSizeChangedEvent (WindowSizeChangedEventData w (V2 c d))
                      Raw.SDL_WINDOWEVENT_MINIMIZED ->
                        WindowMinimizedEvent (WindowMinimizedEventData w)
                      Raw.SDL_WINDOWEVENT_MAXIMIZED ->
                        WindowMaximizedEvent (WindowMaximizedEventData w)
                      Raw.SDL_WINDOWEVENT_RESTORED ->
                        WindowRestoredEvent (WindowRestoredEventData w)
                      Raw.SDL_WINDOWEVENT_ENTER ->
                        WindowGainedMouseFocusEvent (WindowGainedMouseFocusEventData w)
                      Raw.SDL_WINDOWEVENT_LEAVE ->
                        WindowLostMouseFocusEvent (WindowLostMouseFocusEventData w)
                      Raw.SDL_WINDOWEVENT_FOCUS_GAINED ->
                        WindowGainedKeyboardFocusEvent (WindowGainedKeyboardFocusEventData w)
                      Raw.SDL_WINDOWEVENT_FOCUS_LOST ->
                        WindowLostKeyboardFocusEvent (WindowLostKeyboardFocusEventData w)
                      Raw.SDL_WINDOWEVENT_CLOSE ->
                        WindowClosedEvent (WindowClosedEventData w)
                      _ ->
                        UnknownEvent (UnknownEventData t)))
convertRaw (Raw.KeyboardEvent Raw.SDL_KEYDOWN ts a _ c d) =
  do w <- getWindowFromID a
     return (Event ts
                   (KeyboardEvent
                      (KeyboardEventData w
                                         Pressed
                                         (c /= 0)
                                         (fromRawKeysym d))))
convertRaw (Raw.KeyboardEvent Raw.SDL_KEYUP ts a _ c d) =
  do w <- getWindowFromID a
     return (Event ts
                   (KeyboardEvent
                      (KeyboardEventData w
                                         Released
                                         (c /= 0)
                                         (fromRawKeysym d))))
convertRaw Raw.KeyboardEvent{} = error "convertRaw: Unknown keyboard motion"
convertRaw (Raw.TextEditingEvent _ ts a b c d) =
  do w <- getWindowFromID a
     return (Event ts
                   (TextEditingEvent
                      (TextEditingEventData w
                                            (ccharStringToText b)
                                            c
                                            d)))
convertRaw (Raw.TextInputEvent _ ts a b) =
  do w <- getWindowFromID a
     return (Event ts
                   (TextInputEvent
                      (TextInputEventData w
                                          (ccharStringToText b))))
convertRaw (Raw.KeymapChangedEvent _ ts) =
  return (Event ts KeymapChangedEvent)
convertRaw (Raw.MouseMotionEvent _ ts a b c d e f g) =
  do w <- getWindowFromID a
     let buttons =
           catMaybes [(Raw.SDL_BUTTON_LMASK `test` c) ButtonLeft
                     ,(Raw.SDL_BUTTON_RMASK `test` c) ButtonRight
                     ,(Raw.SDL_BUTTON_MMASK `test` c) ButtonMiddle
                     ,(Raw.SDL_BUTTON_X1MASK `test` c) ButtonX1
                     ,(Raw.SDL_BUTTON_X2MASK `test` c) ButtonX2]
     return (Event ts
                   (MouseMotionEvent
                      (MouseMotionEventData w
                                            (fromNumber b)
                                            buttons
                                            (P (V2 d e))
                                            (V2 f g))))
  where mask `test` x =
          if mask .&. x /= 0
             then Just
             else const Nothing
convertRaw (Raw.MouseButtonEvent t ts a b c _ e f g) =
  do w <- getWindowFromID a
     let motion
           | t == Raw.SDL_MOUSEBUTTONUP = Released
           | t == Raw.SDL_MOUSEBUTTONDOWN = Pressed
           | otherwise = error "convertRaw: Unexpected mouse button motion"
     return (Event ts
                   (MouseButtonEvent
                      (MouseButtonEventData w
                                            motion
                                            (fromNumber b)
                                            (fromNumber c)
                                            e
                                            (P (V2 f g)))))
convertRaw (Raw.MouseWheelEvent _ ts a b c d e) =
  do w <- getWindowFromID a
     return (Event ts
                   (MouseWheelEvent
                      (MouseWheelEventData w
                                           (fromNumber b)
                                           (V2 c d)
                                           (fromNumber e))))
convertRaw (Raw.JoyAxisEvent _ ts a b c) =
  return (Event ts (JoyAxisEvent (JoyAxisEventData a b c)))
convertRaw (Raw.JoyBallEvent _ ts a b c d) =
  return (Event ts
                (JoyBallEvent
                   (JoyBallEventData a
                                     b
                                     (V2 c d))))
convertRaw (Raw.JoyHatEvent _ ts a b c) =
  return (Event ts
                (JoyHatEvent
                   (JoyHatEventData a
                                    b
                                    (fromNumber c))))
convertRaw (Raw.JoyButtonEvent _ ts a b c) =
  return (Event ts (JoyButtonEvent (JoyButtonEventData a b (fromNumber c))))
convertRaw (Raw.JoyDeviceEvent t ts a) =
  return (Event ts (JoyDeviceEvent (JoyDeviceEventData (fromNumber t) a)))
convertRaw (Raw.ControllerAxisEvent _ ts a b c) =
  return (Event ts (ControllerAxisEvent (ControllerAxisEventData a b c)))
convertRaw (Raw.ControllerButtonEvent t ts a b _) =
  return (Event ts 
           (ControllerButtonEvent
             (ControllerButtonEventData a 
                                        (fromNumber $ fromIntegral b)
                                        (fromNumber t))))
convertRaw (Raw.ControllerDeviceEvent t ts a) =
  return (Event ts (ControllerDeviceEvent (ControllerDeviceEventData (fromNumber t) a)))
convertRaw (Raw.AudioDeviceEvent Raw.SDL_AUDIODEVICEADDED ts a b) =
  return (Event ts (AudioDeviceEvent (AudioDeviceEventData True a (b /= 0))))
convertRaw (Raw.AudioDeviceEvent Raw.SDL_AUDIODEVICEREMOVED ts a b) =
  return (Event ts (AudioDeviceEvent (AudioDeviceEventData False a (b /= 0))))
convertRaw Raw.AudioDeviceEvent{} =
  error "convertRaw: Unknown audio device motion"
convertRaw (Raw.QuitEvent _ ts) =
  return (Event ts QuitEvent)
convertRaw (Raw.UserEvent t ts a b c d) =
  do w <- getWindowFromID a
     return (Event ts (UserEvent (UserEventData t w b c d)))
convertRaw (Raw.SysWMEvent _ ts a) =
  return (Event ts (SysWMEvent (SysWMEventData a)))
convertRaw (Raw.TouchFingerEvent t ts a b c d e f g) =
  do let touchFingerEvent motion = TouchFingerEvent
                                     (TouchFingerEventData a
                                                           b
                                                           motion
                                                           (P (V2 c d))
                                                           g)
     let touchFingerMotionEvent = TouchFingerMotionEvent
                                    (TouchFingerMotionEventData a
                                                                b
                                                                (P (V2 c d))
                                                                (V2 e f)
                                                                g)
     case t of
       Raw.SDL_FINGERDOWN   -> return (Event ts (touchFingerEvent Pressed))
       Raw.SDL_FINGERUP     -> return (Event ts (touchFingerEvent Released))
       Raw.SDL_FINGERMOTION -> return (Event ts touchFingerMotionEvent)
       _                    -> error "convertRaw: Unexpected touch finger event"
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
  return (Event ts ClipboardUpdateEvent)
convertRaw (Raw.UnknownEvent t ts) =
  return (Event ts (UnknownEvent (UnknownEventData t)))

-- | Poll for currently pending events. You can only call this function in the thread that set the video mode.
pollEvent :: MonadIO m => m (Maybe Event)
pollEvent =
  liftIO $ do
    n <- Raw.pollEvent nullPtr
    -- We use NULL first to check if there's an event.
    if n == 0
      then return Nothing
      else alloca $ \e -> do
             n <- Raw.pollEvent e
             -- Checking 0 again doesn't hurt and it's good to be safe.
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
  throwIfNeg_ "SDL.Events.waitEvent" "SDL_WaitEvent" $
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

-- | A user defined event structure that has been registered with SDL.
--
-- Use 'registerEvent', below, to obtain an instance.
data RegisteredEventType a =
  RegisteredEventType {pushRegisteredEvent :: a -> IO EventPushResult
                      ,getRegisteredEvent :: Event -> IO (Maybe a)
                      }

-- | A record used to convert between SDL Events and user-defined data structures.
--
-- Used for 'registerEvent', below.
data RegisteredEventData =
  RegisteredEventData {registeredEventWindow :: !(Maybe Window)
                       -- ^ The associated 'Window'.
                      ,registeredEventCode :: !Int32
                       -- ^ User defined event code.
                      ,registeredEventData1 :: !(Ptr ())
                       -- ^ User defined data pointer.
                      ,registeredEventData2 :: !(Ptr ())
                       -- ^ User defined data pointer.
                      }
  deriving (Eq,Ord,Generic,Show,Typeable)

-- | A registered event with no associated data.
--
-- This is a resonable baseline to modify for converting to
-- 'RegisteredEventData'.
emptyRegisteredEvent :: RegisteredEventData
emptyRegisteredEvent = RegisteredEventData Nothing 0 nullPtr nullPtr

-- | Possible results of an attempted push of an event to the queue.
data EventPushResult = EventPushSuccess | EventPushFiltered | EventPushFailure Text
  deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Register a new event type with SDL.
--
-- Provide functions that convert between 'UserEventData' and your structure.
-- You can then use 'pushRegisteredEvent' to add a custom event of the
-- registered type to the queue, and 'getRegisteredEvent' to test for such
-- events in the main loop.
registerEvent :: MonadIO m
              => (RegisteredEventData -> Timestamp -> IO (Maybe a))
              -> (a -> IO RegisteredEventData)
              -> m (Maybe (RegisteredEventType a))
registerEvent registeredEventDataToEvent eventToRegisteredEventData = do
  typ <- Raw.registerEvents 1
  if typ == maxBound
  then return Nothing
  else
    let pushEv ev = do
          RegisteredEventData mWin code d1 d2 <- eventToRegisteredEventData ev
          windowID <- case mWin of
            Just (Window w) -> Raw.getWindowID w
            Nothing         -> return 0
          -- timestamp will be filled in by SDL
          let rawEvent = Raw.UserEvent typ 0 windowID code d1 d2
          liftIO . alloca $ \eventPtr -> do
            poke eventPtr rawEvent
            pushResult <- Raw.pushEvent eventPtr
            case pushResult of
              1 -> return $ EventPushSuccess
              0 -> return $ EventPushFiltered
              _ -> EventPushFailure <$> getError

        getEv (Event ts (UserEvent (UserEventData typ mWin code d1 d2))) =
          registeredEventDataToEvent (RegisteredEventData mWin code d1 d2) ts
        getEv _ = return Nothing

    in return . Just $ RegisteredEventType pushEv getEv

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

-- | An 'EventWatchCallback' can process and respond to an event
-- when it is added to the event queue.
type EventWatchCallback = Event -> IO ()
newtype EventWatch = EventWatch {runEventWatchRemoval :: IO ()}

-- | Trigger an 'EventWatchCallback' when an event is added to the SDL
-- event queue.
--
-- See @<https://wiki.libsdl.org/SDL_AddEventWatch>@ for C documentation.
addEventWatch :: MonadIO m => EventWatchCallback -> m EventWatch
addEventWatch callback = liftIO $ do
  rawFilter <- Raw.mkEventFilter wrappedCb
  Raw.addEventWatch rawFilter nullPtr
  return (EventWatch $ auxRemove rawFilter)
  where
    wrappedCb :: Ptr () -> Ptr Raw.Event -> IO CInt
    wrappedCb _ evPtr = 0 <$ (callback =<< convertRaw =<< peek evPtr)

    auxRemove :: Raw.EventFilter -> IO ()
    auxRemove rawFilter = do
      Raw.delEventWatch rawFilter nullPtr
      freeHaskellFunPtr rawFilter

-- | Remove an 'EventWatch'.
--
-- See @<https://wiki.libsdl.org/SDL_DelEventWatch>@ for C documentation.
delEventWatch :: MonadIO m => EventWatch -> m ()
delEventWatch = liftIO . runEventWatchRemoval

-- | Checks raw Windows for null references.
getWindowFromID :: MonadIO m => Word32 -> m (Maybe Window)
getWindowFromID id = do
  rawWindow <- Raw.getWindowFromID id
  return $ if rawWindow == nullPtr then Nothing else Just $ Window rawWindow
