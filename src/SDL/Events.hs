module SDL.Event (Event(..)) where

data Event = Event
  { eventTimestamp :: Word32
  , eventPayload :: EventPayload}

data EventPayload
  = WindowEvent {indowEventWindowID :: Word32
                ,windowEventEvent :: Word8
                ,windowEventData1 :: Int32
                ,windowEventData2 :: Int32}
  | KeyboardEvent {keyboardEventWindowID :: Word32
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
                     ,mouseMotionEventPos :: P V2 Int32
                     ,mouseMotionEventRelMotion :: V2 Int32}
  | MouseButtonEvent {mouseButtonEventWindowID :: Word32
                     ,mouseButtonEventWhich :: Word32
                     ,mouseButtonEventButton :: Word8
                     ,mouseButtonEventState :: Word8
                     ,mouseButtonEventClicks :: Word8
                     ,mouseButtonEventPos :: P V2 Int32}
  | MouseWheelEvent {mouseWheelEventWindowID :: Word32
                    ,mouseWheelEventWhich :: Word32
                    ,mouseWheelEventPos :: P V2 Int32}
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
                     ,touchFingerEventPos :: P V2 CFloat
                     ,touchFingerEventRelMotion :: V2 CFloat
                     ,touchFingerEventPressure :: CFloat}
  | MultiGestureEvent {multiGestureEventTouchID :: TouchID
                      ,multiGestureEventDTheta :: CFloat
                      ,multiGestureEventDDist :: CFloat
                      ,multiGestureEventPos :: P V2 CFloat
                      ,multiGestureEventNumFingers :: Word16}
  | DollarGestureEvent {dollarGestureEventTouchID :: TouchID
                       ,dollarGestureEventGestureID :: GestureID
                       ,dollarGestureEventNumFingers :: Word32
                       ,dollarGestureEventError :: CFloat
                       ,dollagGestureEventPos :: P V2 CFloat}
  | DropEvent {dropEventFile :: CString}
  | ClipboardUpdateEvent
  | UnknownEvent {unknownEventType :: Word32}
  deriving (Eq,Show)
