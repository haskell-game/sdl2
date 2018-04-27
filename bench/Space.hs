{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test that space usage is low and avoid GCs where possible.

module Main where

import Foreign.C (CInt)
import SDL
import Weigh

-- | Main entry point.
main :: IO ()
main =
  mainWith
    (do setColumns [Case, Allocated, GCs]
        sequence_
          [ validateAction
            ("pollEvent " ++ show i)
            pollEventTest
            i
            (\weight ->
               if weightGCs weight > 0
                 then Just "Non-zero number of garbage collections!"
                 else if weightAllocatedBytes weight > 2000
                        then Just
                               "Allocated >2KB! Allocations should be constant."
                        else Nothing)
          | i <- [1, 10, 100, 1000, 10000]
          ]
        sequence_
          [ validateAction
            ("pollEvent + clear " ++ show i)
            pollEventClearTest
            i
            (\weight ->
               if weightGCs weight > 0
                 then Just "Non-zero number of garbage collections!"
                 else if weightAllocatedBytes weight > 3000
                        then Just
                               "Allocated >3KB! Allocations should be constant."
                        else Nothing)
          | i <- [1, 10, 100, 1000, 10000]
          ]
        sequence_
          [ validateAction
            ("pollEvent + present " ++ show i)
            pollEventPresentTest
            i
            (\weight ->
               if weightGCs weight > 0
                 then Just "Non-zero number of garbage collections!"
                 else if weightAllocatedBytes weight > 4000
                        then Just
                               "Allocated >4KB! Allocations should be constant."
                        else Nothing)
          | i <- [1, 10, 100, 1000]
          ]
        sequence_
          [ validateAction
            ("pollEvent + drawColor " ++ show i)
            pollEventDrawColorTest
            i
            (\weight ->
               if weightGCs weight > 0
                 then Just "Non-zero number of garbage collections!"
                 else if weightAllocatedBytes weight > 4000
                        then Just
                               "Allocated >KB! Allocations should be constant."
                        else Nothing)
          | i <- [1, 10, 100, 1000, 2000]
          ]
        sequence_
          [ validateAction
            ("pollEvent + drawRect " ++ show i)
            pollEventDrawRectTest
            i
            (\weight ->
               if weightGCs weight > 0
                 then Just "Non-zero number of garbage collections!"
                 else if weightAllocatedBytes weight > 4000
                        then Just
                               "Allocated >4KB! Allocations should be constant."
                        else Nothing)
          | i <- [1, 10, 100, 1000]
          ]
        sequence_
          [ validateAction
            ("animated rect " ++ show i)
            pollEventAnimRectTest
            i
            (\weight ->
               if weightGCs weight > 0
                 then Just "Non-zero number of garbage collections!"
                 else if weightAllocatedBytes weight > 4000
                        then Just
                               "Allocated >4KB! Allocations should be constant."
                        else Nothing)
          | i <- [1, 10, 100, 1000, 2000]
          ])

-- | Test that merely polling does not allocate or engage the GC.
-- <https://github.com/haskell-game/sdl2/issues/178>
pollEventTest :: Int -> IO ()
pollEventTest iters = do
  initializeAll
  let go :: Int -> IO ()
      go 0 = pure ()
      go i = do
        _ <- pollEvent
        go (i - 1)
  go iters

-- | Test that merely polling and clearing the screen does not
-- allocate or engage the GC.
pollEventClearTest :: Int -> IO ()
pollEventClearTest iters = do
  initializeAll
  window <- createWindow "pollEventClearTest" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let go :: Int -> IO ()
      go 0 = pure ()
      go i = do
        _ <- pollEvent
        clear renderer
        go (i - 1)
  go iters

-- | Test that merely polling and presenting does not allocate or
-- engage the GC.
pollEventPresentTest :: Int -> IO ()
pollEventPresentTest iters = do
  initializeAll
  window <- createWindow "pollEventPresentTest" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let go :: Int -> IO ()
      go 0 = pure ()
      go i = do
        _ <- pollEvent
        clear renderer
        present renderer
        go (i - 1)
  go iters

-- | Test that merely polling and drawColoring does not allocate or
-- engage the GC.
pollEventDrawColorTest :: Int -> IO ()
pollEventDrawColorTest iters = do
  initializeAll
  window <- createWindow "pollEventDrawColorTest" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let go :: Int -> IO ()
      go 0 = pure ()
      go i = do
        _ <- pollEvent
        rendererDrawColor renderer $= V4 0 0 255 255
        clear renderer
        present renderer
        go (i - 1)
  go iters

-- | Draw a rectangle on screen.
pollEventDrawRectTest :: Int -> IO ()
pollEventDrawRectTest iters = do
  initializeAll
  window <- createWindow "pollEventDrawRectTest" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  let go :: Int -> IO ()
      go 0 = pure ()
      go i = do
        _ <- pollEvent
        rendererDrawColor renderer $= V4 40 40 40 255
        clear renderer
        rendererDrawColor renderer $= V4 255 255 255 255
        fillRect renderer (Just (Rectangle (P (V2 40 40)) (V2 80 80)))
        present renderer
        go (i - 1)
  go iters

--------------------------------------------------------------------------------
-- Animated rect test

data State = State
  { stateI :: !CInt
  , stateV :: !(V2 CInt)
  , stateP :: !(V2 CInt)
  }

-- | Animate a rectangle on the screen for n iterations.
pollEventAnimRectTest :: CInt -> IO ()
pollEventAnimRectTest iters = do
  initializeAll
  window <-
    createWindow
      "pollEventAnimRectTest"
      defaultWindow {windowInitialSize = defaultWindowSize}
  renderer <- createRenderer window (-1) defaultRenderer
  let go ::  State -> IO ()
      go !(State 0 _ _) = pure ()
      go !(State i (V2 xv yv) p@(V2 x y)) = do
        _ <- pollEvent
        rendererDrawColor renderer $= V4 40 40 40 255
        clear renderer
        rendererDrawColor renderer $= V4 255 255 255 255
        let xv'
              | x + w > mw = -xv
              | x < 0 = -xv
              | otherwise = xv
            yv'
              | y + h > mh = -yv
              | y < 0 = -yv
              | otherwise = yv
            v' = V2 xv' yv'
            p' = p + v'
        fillRect renderer (Just (Rectangle (P p') (V2 w h)))
        present renderer
        go (State (i - 1) v' p')
  go (State iters (V2 2 1) (V2 0 0))
  where
    defaultWindowSize :: V2 CInt
    defaultWindowSize = V2 800 600
    mw :: CInt
    mh :: CInt
    V2 mw mh = defaultWindowSize
    w :: CInt
    h :: CInt
    (w, h) = (100, 100)
