module UserEvents where

import Control.Concurrent (myThreadId)
import Data.Maybe (Maybe(Nothing))
import Data.Word (Word32)
import qualified Data.Text as Text
import Foreign.Ptr (nullPtr)
import SDL

-- | A timer event with timestamp
data TimerEvent = TimerEvent Word32

timerEvent :: IO TimerEvent
timerEvent = do
  t <- show <$> ticks
  tid <- show <$> myThreadId
  putStrLn $ "Created timer event at " ++ t ++ " ticks. Threadid: " ++ tid
  return $ TimerEvent 0

main :: IO ()
main = do
  initializeAll
  let toTimerEvent _ = return . Just . TimerEvent
      fromTimerEvent = const $ return emptyRegisteredEvent
  registeredEvent <- registerEvent toTimerEvent fromTimerEvent
  case registeredEvent of
    Nothing -> putStrLn "Fatal error: unable to register timer events."
    Just registeredTimerEvent -> do
      addTimer 1000 $ mkTimerCb registeredTimerEvent
      putStrLn "press q at any time to quit"
      appLoop registeredTimerEvent

mkTimerCb :: RegisteredEventType TimerEvent -> TimerCallback
mkTimerCb (RegisteredEventType pushTimerEvent _) interval = do
  pushResult <- pushTimerEvent =<< timerEvent
  case pushResult of
    EventPushSuccess -> return ()
    EventPushFiltered -> putStrLn "event push was filtered: this is impossible"
    EventPushFailure e -> putStrLn $ "Couldn't push event: " ++ Text.unpack e
  return $ Reschedule interval

appLoop :: RegisteredEventType TimerEvent -> IO ()
appLoop (RegisteredEventType pushTimerEvent getTimerEvent) = waitEvent >>= go
  where
  go :: Event -> IO ()
  go ev =
    case eventPayload ev of
      -- Press Q to quit
      KeyboardEvent keyboardEvent
        |  keyboardEventKeyMotion keyboardEvent == Pressed &&
           keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        -> return ()
      UserEvent _ -> do
        maybeTimerEvent <- getTimerEvent ev
        case maybeTimerEvent of
          Just (TimerEvent ts) -> do
             t <- show <$> ticks
             tid <- show <$> myThreadId
             putStrLn $ "Got timer event from queue at " ++ t ++ " ticks."
             putStrLn $ "Timestamp: " ++ show ts
             putStrLn $ "Threadid: " ++ tid
          Nothing -> return ()
        waitEvent >>= go
      _ -> waitEvent >>= go
