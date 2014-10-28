{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lazyfoo.Lesson43 (main) where

import Prelude hiding (any, mapM_)
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import Data.Maybe
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

createBlank :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
createBlank r sz access = Texture <$> SDL.createTexture r SDL.RGBA8888 access sz <*> pure sz

renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> IO ()
renderTexture r (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.renderCopyEx r
                      t
                      clip
                      (Just (SDL.Rectangle xy dstSize))
                      (fromMaybe 0 theta)
                      center
                      (fromMaybe (pure False) flips)

setAsRenderTarget :: SDL.Renderer -> Maybe Texture -> IO ()
setAsRenderTarget r Nothing = SDL.setRenderTarget r Nothing
setAsRenderTarget r (Just (Texture t _)) = SDL.setRenderTarget r (Just t)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  hintSet <- SDL.setHint SDL.HintRenderScaleQuality SDL.ScaleLinear
  unless hintSet $
    putStrLn "Warning: Linear texture filtering not enabled!"

  SDL.withWindow "SDL Tutorial" SDL.defaultWindow {SDL.windowSize = V2 screenWidth screenHeight} $ \window -> do
    SDL.showWindow window

    renderer <-
      SDL.createRenderer
        window
        (-1)
        (SDL.RendererConfig
           { SDL.rendererAccelerated = True
           , SDL.rendererSoftware = False
           , SDL.rendererTargetTexture = False
           , SDL.rendererPresentVSync = True
           })

    SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

    targetTexture <- createBlank renderer (V2 screenWidth screenHeight) SDL.TextureAccessTarget

    let
      screenCenter = P (V2 (screenWidth `div` 2) (screenHeight `div` 2))

      loop theta = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents

        events <- map SDL.eventPayload <$> collectEvents
        let quit = any (== SDL.QuitEvent) events

        setAsRenderTarget renderer (Just targetTexture)

        SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)
        SDL.renderClear renderer

        SDL.setRenderDrawColor renderer (V4 maxBound 0 0 maxBound)
        SDL.renderFillRect renderer (Just $ SDL.Rectangle (P $ V2 (screenWidth `div` 4) (screenHeight `div` 4))
                                                          (V2 (screenWidth `div` 2) (screenHeight `div` 2)))

        SDL.setRenderDrawColor renderer (V4 0 0 maxBound maxBound)
        SDL.renderDrawRect renderer (SDL.Rectangle (P $ V2 (screenWidth `div` 6) (screenHeight `div` 6))
                                                   (V2 (screenWidth * 2 `div` 3) (screenHeight * 2 `div` 3)))

        SDL.setRenderDrawColor renderer (V4 0 maxBound 0 maxBound)
        SDL.renderDrawLine renderer (P (V2 0 (screenHeight `div` 2))) (P (V2 screenWidth (screenHeight `div` 2)))

        SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)
        for_ [0, 4 .. screenHeight] $ \i ->
          SDL.renderDrawPoint renderer (P (V2 (screenWidth `div` 2) i))

        setAsRenderTarget renderer Nothing

        renderTexture renderer targetTexture 0 Nothing (Just (fromIntegral theta)) (Just screenCenter) Nothing

        SDL.renderPresent renderer

        unless quit (loop (theta + 2 `mod` 360))

    loop (0 :: Int)

    SDL.quit
