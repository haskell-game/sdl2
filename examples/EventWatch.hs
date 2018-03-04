{-# LANGUAGE OverloadedStrings #-}

{-|

The following example shows how setting a watch for the WindowSizeChangedEvent
allows us to handle the events as they are generated. Handling them in the
event loop, on the other hand, only allows us to see a final, coalesced, event.

To demonstrate this, run the program, resize the window with your mouse,
and check your console output.

-}
module EventWatch where

import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "resize" WindowConfig {
      windowBorder       = True
    , windowHighDPI      = False
    , windowInputGrabbed = False
    , windowMode         = Windowed
    , windowOpenGL       = Nothing
    , windowPosition     = Wherever
    , windowResizable    = True
    , windowInitialSize  = V2 800 600
    , windowVisible      = True
    , windowVulkan       = False
  }
  renderer <- createRenderer window (-1) defaultRenderer
  addEventWatch $ \ev ->
    case eventPayload ev of
      WindowSizeChangedEvent sizeChangeData ->
        putStrLn $ "eventWatch windowSizeChanged: " ++ show sizeChangeData
      _ -> return ()
  appLoop

appLoop :: IO ()
appLoop = waitEvent >>= go
  where
  go :: Event -> IO ()
  go ev =
    case eventPayload ev of
      WindowSizeChangedEvent sizeChangeData -> do
        putStrLn $ "waitEvent windowSizeChanged: " ++ show sizeChangeData
        waitEvent >>= go
      KeyboardEvent keyboardEvent
        |  keyboardEventKeyMotion keyboardEvent == Pressed &&
           keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        -> return ()
      _ -> waitEvent >>= go
