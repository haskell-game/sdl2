{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lazyfoo.Lesson17 (main) where

import Prelude hiding (foldl1)
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL

import Paths_sdl2 (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data Texture = Texture SDL.Texture (V2 CInt)

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  format <- SDL.surfaceFormat surface
  key <- SDL.mapRGB format (V3 0 maxBound maxBound)
  SDL.colorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

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
                     SDL.MouseButtonEvent e
                       | SDL.mouseButtonEventMotion e == SDL.Pressed -> MouseDown
                       | SDL.mouseButtonEventMotion e == SDL.Released -> MouseUp
                       | otherwise -> MouseOver
                     _ -> MouseOver
        | otherwise = MouseOut

  in Button buttonPos sprite

renderButton :: SDL.Renderer -> Texture -> Button -> IO ()
renderButton r spriteSheet (Button xy sprite) =
  renderTexture r spriteSheet xy (Just spriteClipRect) Nothing Nothing Nothing
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

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      (SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        })

  SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  buttonSpriteSheet <- loadTexture renderer "examples/lazyfoo/button.bmp"

  let loop buttons = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents

        events <- collectEvents
        mousePos <- SDL.getMouseLocation

        let (Any quit, Endo updateButton) =
              foldMap (\case
                         SDL.QuitEvent -> (Any True, mempty)
                         e -> (mempty, Endo (handleEvent mousePos e))) $
              map SDL.eventPayload events

        SDL.renderDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.renderClear renderer

        let buttons' = map (\b -> updateButton b) buttons
        for_ buttons' (renderButton renderer buttonSpriteSheet)

        SDL.renderPresent renderer

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
