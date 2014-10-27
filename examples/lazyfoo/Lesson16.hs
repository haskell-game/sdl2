{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson16 (main) where

import Prelude hiding (foldl1)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Foreign.C.Types
import Linear
import Linear.Affine
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: FilePath -> SDL.RenderM Texture
loadTexture filePath = do
  (surface, size) <- SDL.liftRender loadSurface
  t <- SDL.createTextureFromSurface surface
  SDL.liftRender $ SDL.freeSurface surface
  return (Texture t size)
    where
      loadSurface = do
        surface <- getDataFileName filePath >>= SDL.loadBMP
        size <- SDL.surfaceDimensions surface
        format <- SDL.surfaceFormat surface
        key <- SDL.mapRGB format (V3 0 maxBound maxBound)
        SDL.setColorKey surface (Just key)
        return (surface, size)

renderTexture :: Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> Maybe CDouble -> Maybe (Point V2 CInt) -> Maybe (V2 Bool) -> SDL.RenderM ()
renderTexture (Texture t size) xy clip theta center flips =
  let dstSize =
        maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.renderCopyEx t
                      clip
                      (Just (SDL.Rectangle xy dstSize))
                      (fromMaybe 0 theta)
                      center
                      (fromMaybe (pure False) flips)

data ButtonSprite = MouseOut | MouseOver | MouseDown | MouseUp

data Button = Button (Point V2 CInt) ButtonSprite

buttonSize :: V2 CInt
buttonWidth, buttonHeight :: CInt
buttonSize@(V2 buttonWidth buttonHeight) = V2 300 200

handleEvent :: Point V2 CInt -> SDL.EventPayload -> Button -> Button
handleEvent mousePos e (Button buttonPos _) =
  let inside = foldl1 (&&) ((>=) <$> mousePos <*> buttonPos) &&
               foldl1 (&&) ((<=) <$> mousePos <*> buttonPos .+^ buttonSize)
      sprite
        | inside = case e of
                     SDL.MouseButtonEvent{..}
                       | mouseButtonEventMotion == SDL.MouseButtonDown -> MouseDown
                       | mouseButtonEventMotion == SDL.MouseButtonUp -> MouseUp
                       | otherwise -> MouseOver
                     _ -> MouseOver
        | otherwise = MouseOut

  in Button buttonPos sprite

renderButton :: Texture -> Button -> SDL.RenderM ()
renderButton spriteSheet (Button xy sprite) =
  renderTexture spriteSheet xy (Just spriteClipRect) Nothing Nothing Nothing
  where
  spriteClipRect =
    let i = case sprite of
              MouseOut -> 0
              MouseOver -> 1
              MouseDown -> 2
              MouseUp -> 3
    in SDL.Rectangle (P (V2 0 (i * 200))) (V2 300 200)

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
         , SDL.rendererPresentVSync = True
         })

  buttonSpriteSheet <- SDL.withRenderer renderer $ do
    SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)

    loadTexture "examples/lazyfoo/button.bmp"

  let loop buttons = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents

        events <- collectEvents
        mousePos <- SDL.getMouseState

        let (Any quit, Endo updateButton) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         e -> (mempty, Endo (handleEvent mousePos e))) $
              map SDL.eventPayload events

            buttons' = map (\b -> updateButton b) buttons

        SDL.withRenderer renderer $ do
          SDL.setRenderDrawColor (V4 maxBound maxBound maxBound maxBound)
          SDL.renderClear

          for_ buttons' (renderButton buttonSpriteSheet)

          SDL.renderPresent

        unless quit (loop buttons')

  loop (let newButton xy = Button xy MouseOut
        in [ newButton (P (V2 0 0))
           , newButton (P (V2 (screenWidth - buttonWidth) 0))
           , newButton (P (V2 0 (screenHeight - buttonHeight)))
           , newButton (P (V2 screenWidth screenHeight - buttonSize))
           ])

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
