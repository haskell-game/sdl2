--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenGLExample where

import Control.Monad
import Foreign.C.Types
import Linear
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import           System.Exit (exitFailure)
import           System.IO

import SDL (($=))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

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
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.showWindow window

  _ <- SDL.glCreateContext(window)
  (prog, attrib) <- initResources

  let loop = do
        events <- SDL.pollEvents
        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        GL.clear [GL.ColorBuffer]
        draw prog attrib
        SDL.glSwapWindow window

        unless quit (loop)

  loop

  SDL.destroyWindow window
  SDL.quit

initResources :: IO (GL.Program, GL.AttribLocation)
initResources = do
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        exitFailure

    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.attribLocation program "coord2d" $= GL.AttribLocation 0
    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program

    return (program, GL.AttribLocation 0)

draw :: GL.Program -> GL.AttribLocation -> IO ()
draw program attrib = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

    GL.currentProgram $= Just program
    GL.vertexAttribArray attrib $= GL.Enabled
    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
    GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    GL.vertexAttribArray attrib $= GL.Disabled

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
            "attribute vec2 coord2d; "
           , ""
           , "void main(void) { "
           , " gl_Position = vec4(coord2d, 0.0, 1.0); "
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
            ""
           , "#version 120"
           , "void main(void) {"
           , "gl_FragColor = vec4((gl_FragCoord.x/640), (gl_FragCoord.y/480), 0, 1);"
           , "}"
           ]

vertices :: V.Vector Float
vertices = V.fromList [  0.0,  0.8
                      , -0.8, -0.8
                      ,  0.8, -0.8
                      ]

