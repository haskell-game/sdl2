{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson08 (main) where

import Control.Applicative
import Control.Monad
import Data.Foldable (for_)
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  hintSet <- SDL.setHint SDL.HintRenderScaleQuality SDL.ScaleLinear
  unless hintSet $
    putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      (SDL.RendererConfig
         { SDL.rendererAccelerated = True
         , SDL.rendererSoftware = False
         , SDL.rendererTargetTexture = False
         , SDL.rendererPresentVSync = False
         })

  SDL.withRenderer renderer $ SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        SDL.withRenderer renderer $ do
          SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
          SDL.renderClear

          SDL.setRenderDrawColor (V4 maxBound 0 0 maxBound)
          SDL.renderFillRect (Just $ SDL.Rectangle (P $ V2 (screenWidth `div` 4) (screenHeight `div` 4))
                                                          (V2 (screenWidth `div` 2) (screenHeight `div` 2)))

          SDL.setRenderDrawColor (V4 0 0 maxBound maxBound)
          SDL.renderDrawRect (SDL.Rectangle (P $ V2 (screenWidth `div` 6) (screenHeight `div` 6))
                                                   (V2 (screenWidth * 2 `div` 3) (screenHeight * 2 `div` 3)))

          SDL.setRenderDrawColor (V4 0 maxBound 0 maxBound)
          SDL.renderDrawLine (P (V2 0 (screenHeight `div` 2))) (P (V2 screenWidth (screenHeight `div` 2)))

          SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
          for_ [0, 4 .. screenHeight] $ \i ->
            SDL.renderDrawPoint (P (V2 (screenWidth `div` 2) i))

          SDL.renderPresent

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
