{-# LANGUAGE OverloadedStrings #-}

-- | Test that space usage is low and avoid GCs where possible.

module Main where

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
                 else if weightAllocatedBytes weight > 3000
                        then Just
                               "Allocated >3KB! Allocations should be constant."
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
                 else if weightAllocatedBytes weight > 3000
                        then Just
                               "Allocated >3KB! Allocations should be constant."
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
  window <- createWindow "pollEventClearTest" defaultWindow
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
  window <- createWindow "pollEventClearTest" defaultWindow
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
