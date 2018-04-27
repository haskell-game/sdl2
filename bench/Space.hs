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
