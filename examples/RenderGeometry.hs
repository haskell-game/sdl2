{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module RenderGeometry where

import Control.Monad
import Data.Word (Word8)
import Foreign (castPtr, plusPtr, sizeOf)
import Foreign.C.Types
import SDL.Vect
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import           System.Exit (exitFailure)
import           System.IO

import SDL (($=))
import qualified SDL
-- import qualified Graphics.Rendering.OpenGL as GL
import SDL.Raw.Types (FPoint(..), Color(..))

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow
        { SDL.windowInitialSize = V2 screenWidth screenHeight
        , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
            { SDL.glProfile = SDL.Core SDL.Normal 3 2
            }
        }
  SDL.showWindow window

  -- SDL.windowOpacity window $= 0.5
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  _ <- SDL.glCreateContext window

  let
    l = fromIntegral screenWidth * 0.33
    t = fromIntegral screenHeight * 0.33
    r = fromIntegral screenWidth * 0.66
    b = fromIntegral screenHeight * 0.66

    triVertices = V.fromList
      [ SDL.Vertex
          (FPoint l b)
          (Color 0xFF 0 0 255)
          (FPoint 0 0)
      , SDL.Vertex
          (FPoint r b)
          (Color 0 0xFF 0 255)
          (FPoint 0 1)
      , SDL.Vertex
          (FPoint r t)
          (Color 0 0 0xFF 255)
          (FPoint 1 1)
      ]

  let
    l = fromIntegral screenWidth * 0.2
    t = fromIntegral screenHeight * 0.2
    r = fromIntegral screenWidth * 0.8
    b = fromIntegral screenHeight * 0.8

    quadVertices = V.fromList
      [ SDL.Vertex
          (FPoint l b)
          (Color 0xFF 0 0xFF 127)
          (FPoint 0 0)
      , SDL.Vertex
          (FPoint r b)
          (Color 0xFF 0 0xFF 127)
          (FPoint 1 0)
      , SDL.Vertex
          (FPoint r t)
          (Color 0xFF 0xFF 0 127)
          (FPoint 1 1)
      , SDL.Vertex
          (FPoint l t)
          (Color 0 0 0 127)
          (FPoint 0 1)
      ]
    quadIndices = V.fromList
      [ 0, 1, 3
      , 2, 3, 1
      ]
    stride = fromIntegral $ sizeOf (undefined :: SDL.Vertex)

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
        SDL.clear renderer

        SDL.renderGeometry
          renderer
          Nothing
          triVertices
          mempty

        SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
        V.unsafeWith quadVertices $ \ptr ->
          SDL.renderGeometryRaw
            renderer
            Nothing
            (castPtr ptr)
            stride
            (castPtr ptr `plusPtr` sizeOf (undefined :: FPoint))
            stride
            (castPtr ptr `plusPtr` sizeOf (undefined :: FPoint) `plusPtr` sizeOf (undefined :: Color))
            stride
            (fromIntegral $ V.length quadVertices)
            (quadIndices :: V.Vector Word8)

        SDL.present renderer

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit
