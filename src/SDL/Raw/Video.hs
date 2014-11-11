module SDL.Raw.Video (
  -- * Display and Window Management
  createWindow,
  createWindowAndRenderer,
  createWindowFrom,
  destroyWindow,
  disableScreenSaver,
  enableScreenSaver,
  glBindTexture,
  glCreateContext,
  glDeleteContext,
  glExtensionSupported,
  glGetAttribute,
  glGetCurrentContext,
  glGetCurrentWindow,
  glGetDrawableSize,
  glGetProcAddress,
  glGetSwapInterval,
  glLoadLibrary,
  glMakeCurrent,
  glResetAttributes,
  glSetAttribute,
  glSetSwapInterval,
  glSwapWindow,
  glUnbindTexture,
  glUnloadLibrary,
  getClosestDisplayMode,
  getCurrentDisplayMode,
  getCurrentVideoDriver,
  getDesktopDisplayMode,
  getDisplayBounds,
  getDisplayMode,
  getDisplayName,
  getNumDisplayModes,
  getNumVideoDisplays,
  getNumVideoDrivers,
  getVideoDriver,
  getWindowBrightness,
  getWindowData,
  getWindowDisplayIndex,
  getWindowDisplayMode,
  getWindowFlags,
  getWindowFromID,
  getWindowGammaRamp,
  getWindowGrab,
  getWindowID,
  getWindowMaximumSize,
  getWindowMinimumSize,
  getWindowPixelFormat,
  getWindowPosition,
  getWindowSize,
  getWindowSurface,
  getWindowTitle,
  hideWindow,
  isScreenSaverEnabled,
  maximizeWindow,
  minimizeWindow,
  raiseWindow,
  restoreWindow,
  setWindowBordered,
  setWindowBrightness,
  setWindowData,
  setWindowDisplayMode,
  setWindowFullscreen,
  setWindowGammaRamp,
  setWindowGrab,
  setWindowIcon,
  setWindowMaximumSize,
  setWindowMinimumSize,
  setWindowPosition,
  setWindowSize,
  setWindowTitle,
  showMessageBox,
  showSimpleMessageBox,
  showWindow,
  updateWindowSurface,
  updateWindowSurfaceRects,
  videoInit,
  videoQuit,

  -- * 2D Accelerated Rendering
  createRenderer,
  createSoftwareRenderer,
  createTexture,
  createTextureFromSurface,
  destroyRenderer,
  destroyTexture,
  getNumRenderDrivers,
  getRenderDrawBlendMode,
  getRenderDrawColor,
  getRenderDriverInfo,
  getRenderTarget,
  getRenderer,
  getRendererInfo,
  getRendererOutputSize,
  getTextureAlphaMod,
  getTextureBlendMode,
  getTextureColorMod,
  lockTexture,
  queryTexture,
  renderClear,
  renderCopy,
  renderCopyEx,
  renderDrawLine,
  renderDrawLines,
  renderDrawPoint,
  renderDrawPoints,
  renderDrawRect,
  renderDrawRects,
  renderFillRect,
  renderFillRects,
  renderGetClipRect,
  renderGetLogicalSize,
  renderGetScale,
  renderGetViewport,
  renderPresent,
  renderReadPixels,
  renderSetClipRect,
  renderSetLogicalSize,
  renderSetScale,
  renderSetViewport,
  renderTargetSupported,
  setRenderDrawBlendMode,
  setRenderDrawColor,
  setRenderTarget,
  setTextureAlphaMod,
  setTextureBlendMode,
  setTextureColorMod,
  unlockTexture,
  updateTexture,
  updateYUVTexture,

  -- * Pixel Formats and Conversion Routines
  allocFormat,
  allocPalette,
  calculateGammaRamp,
  freeFormat,
  freePalette,
  getPixelFormatName,
  getRGB,
  getRGBA,
  mapRGB,
  mapRGBA,
  masksToPixelFormatEnum,
  pixelFormatEnumToMasks,
  setPaletteColors,
  setPixelFormatPalette,

  -- * Rectangle Functions
  enclosePoints,
  hasIntersection,
  intersectRect,
  intersectRectAndLine,
  unionRect,

  -- * Surface Creation and Simple Drawing
  blitScaled,
  blitSurface,
  convertPixels,
  convertSurface,
  convertSurfaceFormat,
  createRGBSurface,
  createRGBSurfaceFrom,
  fillRect,
  fillRects,
  freeSurface,
  getClipRect,
  getColorKey,
  getSurfaceAlphaMod,
  getSurfaceBlendMode,
  getSurfaceColorMod,
  loadBMP,
  loadBMP_RW,
  lockSurface,
  lowerBlit,
  lowerBlitScaled,
  saveBMP,
  saveBMP_RW,
  setClipRect,
  setColorKey,
  setSurfaceAlphaMod,
  setSurfaceBlendMode,
  setSurfaceColorMod,
  setSurfacePalette,
  setSurfaceRLE,
  unlockSurface,

  -- * Platform-specific Window Management
  getWindowWMInfo,

  -- * Clipboard Handling
  getClipboardText,
  hasClipboardText,
  setClipboardText
) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum
import SDL.Raw.Filesystem
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_CreateWindow" createWindow' :: CString -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO Window
foreign import ccall "SDL.h SDL_CreateWindowAndRenderer" createWindowAndRenderer' :: CInt -> CInt -> Word32 -> Ptr Window -> Ptr Renderer -> IO CInt
foreign import ccall "SDL.h SDL_CreateWindowFrom" createWindowFrom' :: Ptr () -> IO Window
foreign import ccall "SDL.h SDL_DestroyWindow" destroyWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_DisableScreenSaver" disableScreenSaver' :: IO ()
foreign import ccall "SDL.h SDL_EnableScreenSaver" enableScreenSaver' :: IO ()
foreign import ccall "SDL.h SDL_GL_BindTexture" glBindTexture' :: Texture -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "SDL.h SDL_GL_CreateContext" glCreateContext' :: Window -> IO GLContext
foreign import ccall "SDL.h SDL_GL_DeleteContext" glDeleteContext' :: GLContext -> IO ()
foreign import ccall "SDL.h SDL_GL_ExtensionSupported" glExtensionSupported' :: CString -> IO Bool
foreign import ccall "SDL.h SDL_GL_GetAttribute" glGetAttribute' :: GLattr -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_GetCurrentContext" glGetCurrentContext' :: IO GLContext
foreign import ccall "SDL.h SDL_GL_GetCurrentWindow" glGetCurrentWindow' :: IO Window
foreign import ccall "SDL.h SDL_GL_GetDrawableSize" glGetDrawableSize' :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GL_GetProcAddress" glGetProcAddress' :: CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GL_GetSwapInterval" glGetSwapInterval' :: IO CInt
foreign import ccall "SDL.h SDL_GL_LoadLibrary" glLoadLibrary' :: CString -> IO CInt
foreign import ccall "SDL.h SDL_GL_MakeCurrent" glMakeCurrent' :: Window -> GLContext -> IO CInt
foreign import ccall "SDL.h SDL_GL_ResetAttributes" glResetAttributes' :: IO ()
foreign import ccall "SDL.h SDL_GL_SetAttribute" glSetAttribute' :: GLattr -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_SetSwapInterval" glSetSwapInterval' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_SwapWindow" glSwapWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_GL_UnbindTexture" glUnbindTexture' :: Texture -> IO CInt
foreign import ccall "SDL.h SDL_GL_UnloadLibrary" glUnloadLibrary' :: IO ()
foreign import ccall "SDL.h SDL_GetClosestDisplayMode" getClosestDisplayMode' :: CInt -> Ptr DisplayMode -> Ptr DisplayMode -> IO (Ptr DisplayMode)
foreign import ccall "SDL.h SDL_GetCurrentDisplayMode" getCurrentDisplayMode' :: CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetCurrentVideoDriver" getCurrentVideoDriver' :: IO CString
foreign import ccall "SDL.h SDL_GetDesktopDisplayMode" getDesktopDisplayMode' :: CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayBounds" getDisplayBounds' :: CInt -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayMode" getDisplayMode' :: CInt -> CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayName" getDisplayName' :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetNumDisplayModes" getNumDisplayModes' :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetNumVideoDisplays" getNumVideoDisplays' :: IO CInt
foreign import ccall "SDL.h SDL_GetNumVideoDrivers" getNumVideoDrivers' :: IO CInt
foreign import ccall "SDL.h SDL_GetVideoDriver" getVideoDriver' :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetWindowBrightness" getWindowBrightness' :: Window -> IO CFloat
foreign import ccall "SDL.h SDL_GetWindowData" getWindowData' :: Window -> CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GetWindowDisplayIndex" getWindowDisplayIndex' :: Window -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowDisplayMode" getWindowDisplayMode' :: Window -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowFlags" getWindowFlags' :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowFromID" getWindowFromID' :: Word32 -> IO Window
foreign import ccall "SDL.h SDL_GetWindowGammaRamp" getWindowGammaRamp' :: Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowGrab" getWindowGrab' :: Window -> IO Bool
foreign import ccall "SDL.h SDL_GetWindowID" getWindowID' :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowMaximumSize" getWindowMaximumSize' :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowMinimumSize" getWindowMinimumSize' :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowPixelFormat" getWindowPixelFormat' :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowPosition" getWindowPosition' :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowSize" getWindowSize' :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowSurface" getWindowSurface' :: Window -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_GetWindowTitle" getWindowTitle' :: Window -> IO CString
foreign import ccall "SDL.h SDL_HideWindow" hideWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_IsScreenSaverEnabled" isScreenSaverEnabled' :: IO Bool
foreign import ccall "SDL.h SDL_MaximizeWindow" maximizeWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_MinimizeWindow" minimizeWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_RaiseWindow" raiseWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_RestoreWindow" restoreWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_SetWindowBordered" setWindowBordered' :: Window -> Bool -> IO ()
foreign import ccall "SDL.h SDL_SetWindowBrightness" setWindowBrightness' :: Window -> CFloat -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowData" setWindowData' :: Window -> CString -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_SetWindowDisplayMode" setWindowDisplayMode' :: Window -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowFullscreen" setWindowFullscreen' :: Window -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowGammaRamp" setWindowGammaRamp' :: Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowGrab" setWindowGrab' :: Window -> Bool -> IO ()
foreign import ccall "SDL.h SDL_SetWindowIcon" setWindowIcon' :: Window -> Ptr Surface -> IO ()
foreign import ccall "SDL.h SDL_SetWindowMaximumSize" setWindowMaximumSize' :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowMinimumSize" setWindowMinimumSize' :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowPosition" setWindowPosition' :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowSize" setWindowSize' :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowTitle" setWindowTitle' :: Window -> CString -> IO ()
foreign import ccall "SDL.h SDL_ShowMessageBox" showMessageBox' :: Ptr MessageBoxData -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_ShowSimpleMessageBox" showSimpleMessageBox' :: Word32 -> CString -> CString -> Window -> IO CInt
foreign import ccall "SDL.h SDL_ShowWindow" showWindow' :: Window -> IO ()
foreign import ccall "SDL.h SDL_UpdateWindowSurface" updateWindowSurface' :: Window -> IO CInt
foreign import ccall "SDL.h SDL_UpdateWindowSurfaceRects" updateWindowSurfaceRects' :: Window -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_VideoInit" videoInit' :: CString -> IO CInt
foreign import ccall "SDL.h SDL_VideoQuit" videoQuit' :: IO ()

foreign import ccall "SDL.h SDL_CreateRenderer" createRenderer' :: Window -> CInt -> Word32 -> IO Renderer
foreign import ccall "SDL.h SDL_CreateSoftwareRenderer" createSoftwareRenderer' :: Ptr Surface -> IO Renderer
foreign import ccall "SDL.h SDL_CreateTexture" createTexture' :: Renderer -> Word32 -> CInt -> CInt -> CInt -> IO Texture
foreign import ccall "SDL.h SDL_CreateTextureFromSurface" createTextureFromSurface' :: Renderer -> Ptr Surface -> IO Texture
foreign import ccall "SDL.h SDL_DestroyRenderer" destroyRenderer' :: Renderer -> IO ()
foreign import ccall "SDL.h SDL_DestroyTexture" destroyTexture' :: Texture -> IO ()
foreign import ccall "SDL.h SDL_GetNumRenderDrivers" getNumRenderDrivers' :: IO CInt
foreign import ccall "SDL.h SDL_GetRenderDrawBlendMode" getRenderDrawBlendMode' :: Renderer -> Ptr BlendMode -> IO Int
foreign import ccall "SDL.h SDL_GetRenderDrawColor" getRenderDrawColor' :: Renderer -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetRenderDriverInfo" getRenderDriverInfo' :: CInt -> Ptr RendererInfo -> IO CInt
foreign import ccall "SDL.h SDL_GetRenderTarget" getRenderTarget' :: Renderer -> IO Texture
foreign import ccall "SDL.h SDL_GetRenderer" getRenderer' :: Window -> IO Renderer
foreign import ccall "SDL.h SDL_GetRendererInfo" getRendererInfo' :: Renderer -> Ptr RendererInfo -> IO CInt
foreign import ccall "SDL.h SDL_GetRendererOutputSize" getRendererOutputSize' :: Renderer -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureAlphaMod" getTextureAlphaMod' :: Texture -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureBlendMode" getTextureBlendMode' :: Texture -> Ptr BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureColorMod" getTextureColorMod' :: Texture -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_LockTexture" lockTexture' :: Texture -> Ptr Rect -> Ptr (Ptr ()) -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_QueryTexture" queryTexture' :: Texture -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderClear" renderClear' :: Renderer -> IO CInt
foreign import ccall "SDL.h SDL_RenderCopy" renderCopy' :: Renderer -> Texture -> Ptr Rect -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderCopyEx" renderCopyEx' :: Renderer -> Texture -> Ptr Rect -> Ptr Rect -> CDouble -> Ptr Point -> RendererFlip -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawLine" renderDrawLine' :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawLines" renderDrawLines' :: Renderer -> Ptr Point -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawPoint" renderDrawPoint' :: Renderer -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawPoints" renderDrawPoints' :: Renderer -> Ptr Point -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawRect" renderDrawRect' :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawRects" renderDrawRects' :: Renderer -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderFillRect" renderFillRect' :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderFillRects" renderFillRects' :: Renderer -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderGetClipRect" renderGetClipRect' :: Renderer -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_RenderGetLogicalSize" renderGetLogicalSize' :: Renderer -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_RenderGetScale" renderGetScale' :: Renderer -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall "SDL.h SDL_RenderGetViewport" renderGetViewport' :: Renderer -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_RenderPresent" renderPresent' :: Renderer -> IO ()
foreign import ccall "SDL.h SDL_RenderReadPixels" renderReadPixels' :: Renderer -> Ptr Rect -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetClipRect" renderSetClipRect' :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetLogicalSize" renderSetLogicalSize' :: Renderer -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetScale" renderSetScale' :: Renderer -> CFloat -> CFloat -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetViewport" renderSetViewport' :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderTargetSupported" renderTargetSupported' :: Renderer -> IO Bool
foreign import ccall "SDL.h SDL_SetRenderDrawBlendMode" setRenderDrawBlendMode' :: Renderer -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetRenderDrawColor" setRenderDrawColor' :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetRenderTarget" setRenderTarget' :: Renderer -> Texture -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureAlphaMod" setTextureAlphaMod' :: Texture -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureBlendMode" setTextureBlendMode' :: Texture -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureColorMod" setTextureColorMod' :: Texture -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_UnlockTexture" unlockTexture' :: Texture -> IO ()
foreign import ccall "SDL.h SDL_UpdateTexture" updateTexture' :: Texture -> Ptr Rect -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_UpdateYUVTexture" updateYUVTexture' :: Texture -> Ptr Rect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "SDL.h SDL_AllocFormat" allocFormat' :: Word32 -> IO (Ptr PixelFormat)
foreign import ccall "SDL.h SDL_AllocPalette" allocPalette' :: CInt -> IO (Ptr Palette)
foreign import ccall "SDL.h SDL_CalculateGammaRamp" calculateGammaRamp' :: CFloat -> Ptr Word16 -> IO ()
foreign import ccall "SDL.h SDL_FreeFormat" freeFormat' :: Ptr PixelFormat -> IO ()
foreign import ccall "SDL.h SDL_FreePalette" freePalette' :: Ptr Palette -> IO ()
foreign import ccall "SDL.h SDL_GetPixelFormatName" getPixelFormatName' :: Word32 -> IO CString
foreign import ccall "SDL.h SDL_GetRGB" getRGB' :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetRGBA" getRGBA' :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_MapRGB" mapRGB' :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MapRGBA" mapRGBA' :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MasksToPixelFormatEnum" masksToPixelFormatEnum' :: CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO Word32
foreign import ccall "SDL.h SDL_PixelFormatEnumToMasks" pixelFormatEnumToMasks' :: Word32 -> Ptr CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Bool
foreign import ccall "SDL.h SDL_SetPaletteColors" setPaletteColors' :: Ptr Palette -> Ptr Color -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_SetPixelFormatPalette" setPixelFormatPalette' :: Ptr PixelFormat -> Ptr Palette -> IO CInt

foreign import ccall "SDL.h SDL_EnclosePoints" enclosePoints' :: Ptr Point -> CInt -> Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_HasIntersection" hasIntersection' :: Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_IntersectRect" intersectRect' :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_IntersectRectAndLine" intersectRectAndLine' :: Ptr Rect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Bool
foreign import ccall "SDL.h SDL_UnionRect" unionRect' :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO ()

foreign import ccall "SDL.h SDL_UpperBlitScaled" blitScaled' :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_UpperBlit" blitSurface' :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_ConvertPixels" convertPixels' :: CInt -> CInt -> Word32 -> Ptr () -> CInt -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_ConvertSurface" convertSurface' :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_ConvertSurfaceFormat" convertSurfaceFormat' :: Ptr Surface -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_CreateRGBSurface" createRGBSurface' :: Word32 -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_CreateRGBSurfaceFrom" createRGBSurfaceFrom' :: Ptr () -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_FillRect" fillRect' :: Ptr Surface -> Ptr Rect -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_FillRects" fillRects' :: Ptr Surface -> Ptr Rect -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_FreeSurface" freeSurface' :: Ptr Surface -> IO ()
foreign import ccall "SDL.h SDL_GetClipRect" getClipRect' :: Ptr Surface -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_GetColorKey" getColorKey' :: Ptr Surface -> Ptr Word32 -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceAlphaMod" getSurfaceAlphaMod' :: Ptr Surface -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceBlendMode" getSurfaceBlendMode' :: Ptr Surface -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceColorMod" getSurfaceColorMod' :: Ptr Surface -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_LoadBMP_RW" loadBMP_RW' :: Ptr RWops -> CInt -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_LockSurface" lockSurface' :: Ptr Surface -> IO CInt
foreign import ccall "SDL.h SDL_LowerBlit" lowerBlit' :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_LowerBlitScaled" lowerBlitScaled' :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_SaveBMP_RW" saveBMP_RW' :: Ptr Surface -> Ptr RWops -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_SetClipRect" setClipRect' :: Ptr Surface -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_SetColorKey" setColorKey' :: Ptr Surface -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceAlphaMod" setSurfaceAlphaMod' :: Ptr Surface -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceBlendMode" setSurfaceBlendMode' :: Ptr Surface -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceColorMod" setSurfaceColorMod' :: Ptr Surface -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfacePalette" setSurfacePalette' :: Ptr Surface -> Ptr Palette -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceRLE" setSurfaceRLE' :: Ptr Surface -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_UnlockSurface" unlockSurface' :: Ptr Surface -> IO ()

foreign import ccall "SDL.h SDL_GetWindowWMInfo" getWindowWMInfo' :: Window -> SysWMinfo -> IO Bool

foreign import ccall "SDL.h SDL_GetClipboardText" getClipboardText' :: IO CString
foreign import ccall "SDL.h SDL_HasClipboardText" hasClipboardText' :: IO Bool
foreign import ccall "SDL.h SDL_SetClipboardText" setClipboardText' :: CString -> IO CInt

createWindow :: MonadIO m => CString -> CInt -> CInt -> CInt -> CInt -> Word32 -> m Window
createWindow v1 v2 v3 v4 v5 v6 = liftIO $ createWindow' v1 v2 v3 v4 v5 v6
{-# INLINE createWindow #-}

createWindowAndRenderer :: MonadIO m => CInt -> CInt -> Word32 -> Ptr Window -> Ptr Renderer -> m CInt
createWindowAndRenderer v1 v2 v3 v4 v5 = liftIO $ createWindowAndRenderer' v1 v2 v3 v4 v5
{-# INLINE createWindowAndRenderer #-}

createWindowFrom :: MonadIO m => Ptr () -> m Window
createWindowFrom v1 = liftIO $ createWindowFrom' v1
{-# INLINE createWindowFrom #-}

destroyWindow :: MonadIO m => Window -> m ()
destroyWindow v1 = liftIO $ destroyWindow' v1
{-# INLINE destroyWindow #-}

disableScreenSaver :: MonadIO m => m ()
disableScreenSaver = liftIO disableScreenSaver'
{-# INLINE disableScreenSaver #-}

enableScreenSaver :: MonadIO m => m ()
enableScreenSaver = liftIO enableScreenSaver'
{-# INLINE enableScreenSaver #-}

glBindTexture :: MonadIO m => Texture -> Ptr CFloat -> Ptr CFloat -> m CInt
glBindTexture v1 v2 v3 = liftIO $ glBindTexture' v1 v2 v3
{-# INLINE glBindTexture #-}

glCreateContext :: MonadIO m => Window -> m GLContext
glCreateContext v1 = liftIO $ glCreateContext' v1
{-# INLINE glCreateContext #-}

glDeleteContext :: MonadIO m => GLContext -> m ()
glDeleteContext v1 = liftIO $ glDeleteContext' v1
{-# INLINE glDeleteContext #-}

glExtensionSupported :: MonadIO m => CString -> m Bool
glExtensionSupported v1 = liftIO $ glExtensionSupported' v1
{-# INLINE glExtensionSupported #-}

glGetAttribute :: MonadIO m => GLattr -> Ptr CInt -> m CInt
glGetAttribute v1 v2 = liftIO $ glGetAttribute' v1 v2
{-# INLINE glGetAttribute #-}

glGetCurrentContext :: MonadIO m => m GLContext
glGetCurrentContext = liftIO glGetCurrentContext'
{-# INLINE glGetCurrentContext #-}

glGetCurrentWindow :: MonadIO m => m Window
glGetCurrentWindow = liftIO glGetCurrentWindow'
{-# INLINE glGetCurrentWindow #-}

glGetDrawableSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
glGetDrawableSize v1 v2 v3 = liftIO $ glGetDrawableSize' v1 v2 v3
{-# INLINE glGetDrawableSize #-}

glGetProcAddress :: MonadIO m => CString -> m (Ptr ())
glGetProcAddress v1 = liftIO $ glGetProcAddress' v1
{-# INLINE glGetProcAddress #-}

glGetSwapInterval :: MonadIO m => m CInt
glGetSwapInterval = liftIO glGetSwapInterval'
{-# INLINE glGetSwapInterval #-}

glLoadLibrary :: MonadIO m => CString -> m CInt
glLoadLibrary v1 = liftIO $ glLoadLibrary' v1
{-# INLINE glLoadLibrary #-}

glMakeCurrent :: MonadIO m => Window -> GLContext -> m CInt
glMakeCurrent v1 v2 = liftIO $ glMakeCurrent' v1 v2
{-# INLINE glMakeCurrent #-}

glResetAttributes :: MonadIO m => m ()
glResetAttributes = liftIO glResetAttributes'
{-# INLINE glResetAttributes #-}

glSetAttribute :: MonadIO m => GLattr -> CInt -> m CInt
glSetAttribute v1 v2 = liftIO $ glSetAttribute' v1 v2
{-# INLINE glSetAttribute #-}

glSetSwapInterval :: MonadIO m => CInt -> m CInt
glSetSwapInterval v1 = liftIO $ glSetSwapInterval' v1
{-# INLINE glSetSwapInterval #-}

glSwapWindow :: MonadIO m => Window -> m ()
glSwapWindow v1 = liftIO $ glSwapWindow' v1
{-# INLINE glSwapWindow #-}

glUnbindTexture :: MonadIO m => Texture -> m CInt
glUnbindTexture v1 = liftIO $ glUnbindTexture' v1
{-# INLINE glUnbindTexture #-}

glUnloadLibrary :: MonadIO m => m ()
glUnloadLibrary = liftIO glUnloadLibrary'
{-# INLINE glUnloadLibrary #-}

getClosestDisplayMode :: MonadIO m => CInt -> Ptr DisplayMode -> Ptr DisplayMode -> m (Ptr DisplayMode)
getClosestDisplayMode v1 v2 v3 = liftIO $ getClosestDisplayMode' v1 v2 v3
{-# INLINE getClosestDisplayMode #-}

getCurrentDisplayMode :: MonadIO m => CInt -> Ptr DisplayMode -> m CInt
getCurrentDisplayMode v1 v2 = liftIO $ getCurrentDisplayMode' v1 v2
{-# INLINE getCurrentDisplayMode #-}

getCurrentVideoDriver :: MonadIO m => m CString
getCurrentVideoDriver = liftIO getCurrentVideoDriver'
{-# INLINE getCurrentVideoDriver #-}

getDesktopDisplayMode :: MonadIO m => CInt -> Ptr DisplayMode -> m CInt
getDesktopDisplayMode v1 v2 = liftIO $ getDesktopDisplayMode' v1 v2
{-# INLINE getDesktopDisplayMode #-}

getDisplayBounds :: MonadIO m => CInt -> Ptr Rect -> m CInt
getDisplayBounds v1 v2 = liftIO $ getDisplayBounds' v1 v2
{-# INLINE getDisplayBounds #-}

getDisplayMode :: MonadIO m => CInt -> CInt -> Ptr DisplayMode -> m CInt
getDisplayMode v1 v2 v3 = liftIO $ getDisplayMode' v1 v2 v3
{-# INLINE getDisplayMode #-}

getDisplayName :: MonadIO m => CInt -> m CString
getDisplayName v1 = liftIO $ getDisplayName' v1
{-# INLINE getDisplayName #-}

getNumDisplayModes :: MonadIO m => CInt -> m CInt
getNumDisplayModes v1 = liftIO $ getNumDisplayModes' v1
{-# INLINE getNumDisplayModes #-}

getNumVideoDisplays :: MonadIO m => m CInt
getNumVideoDisplays = liftIO getNumVideoDisplays'
{-# INLINE getNumVideoDisplays #-}

getNumVideoDrivers :: MonadIO m => m CInt
getNumVideoDrivers = liftIO getNumVideoDrivers'
{-# INLINE getNumVideoDrivers #-}

getVideoDriver :: MonadIO m => CInt -> m CString
getVideoDriver v1 = liftIO $ getVideoDriver' v1
{-# INLINE getVideoDriver #-}

getWindowBrightness :: MonadIO m => Window -> m CFloat
getWindowBrightness v1 = liftIO $ getWindowBrightness' v1
{-# INLINE getWindowBrightness #-}

getWindowData :: MonadIO m => Window -> CString -> m (Ptr ())
getWindowData v1 v2 = liftIO $ getWindowData' v1 v2
{-# INLINE getWindowData #-}

getWindowDisplayIndex :: MonadIO m => Window -> m CInt
getWindowDisplayIndex v1 = liftIO $ getWindowDisplayIndex' v1
{-# INLINE getWindowDisplayIndex #-}

getWindowDisplayMode :: MonadIO m => Window -> Ptr DisplayMode -> m CInt
getWindowDisplayMode v1 v2 = liftIO $ getWindowDisplayMode' v1 v2
{-# INLINE getWindowDisplayMode #-}

getWindowFlags :: MonadIO m => Window -> m Word32
getWindowFlags v1 = liftIO $ getWindowFlags' v1
{-# INLINE getWindowFlags #-}

getWindowFromID :: MonadIO m => Word32 -> m Window
getWindowFromID v1 = liftIO $ getWindowFromID' v1
{-# INLINE getWindowFromID #-}

getWindowGammaRamp :: MonadIO m => Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> m CInt
getWindowGammaRamp v1 v2 v3 v4 = liftIO $ getWindowGammaRamp' v1 v2 v3 v4
{-# INLINE getWindowGammaRamp #-}

getWindowGrab :: MonadIO m => Window -> m Bool
getWindowGrab v1 = liftIO $ getWindowGrab' v1
{-# INLINE getWindowGrab #-}

getWindowID :: MonadIO m => Window -> m Word32
getWindowID v1 = liftIO $ getWindowID' v1
{-# INLINE getWindowID #-}

getWindowMaximumSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowMaximumSize v1 v2 v3 = liftIO $ getWindowMaximumSize' v1 v2 v3
{-# INLINE getWindowMaximumSize #-}

getWindowMinimumSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowMinimumSize v1 v2 v3 = liftIO $ getWindowMinimumSize' v1 v2 v3
{-# INLINE getWindowMinimumSize #-}

getWindowPixelFormat :: MonadIO m => Window -> m Word32
getWindowPixelFormat v1 = liftIO $ getWindowPixelFormat' v1
{-# INLINE getWindowPixelFormat #-}

getWindowPosition :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowPosition v1 v2 v3 = liftIO $ getWindowPosition' v1 v2 v3
{-# INLINE getWindowPosition #-}

getWindowSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowSize v1 v2 v3 = liftIO $ getWindowSize' v1 v2 v3
{-# INLINE getWindowSize #-}

getWindowSurface :: MonadIO m => Window -> m (Ptr Surface)
getWindowSurface v1 = liftIO $ getWindowSurface' v1
{-# INLINE getWindowSurface #-}

getWindowTitle :: MonadIO m => Window -> m CString
getWindowTitle v1 = liftIO $ getWindowTitle' v1
{-# INLINE getWindowTitle #-}

hideWindow :: MonadIO m => Window -> m ()
hideWindow v1 = liftIO $ hideWindow' v1
{-# INLINE hideWindow #-}

isScreenSaverEnabled :: MonadIO m => m Bool
isScreenSaverEnabled = liftIO isScreenSaverEnabled'
{-# INLINE isScreenSaverEnabled #-}

maximizeWindow :: MonadIO m => Window -> m ()
maximizeWindow v1 = liftIO $ maximizeWindow' v1
{-# INLINE maximizeWindow #-}

minimizeWindow :: MonadIO m => Window -> m ()
minimizeWindow v1 = liftIO $ minimizeWindow' v1
{-# INLINE minimizeWindow #-}

raiseWindow :: MonadIO m => Window -> m ()
raiseWindow v1 = liftIO $ raiseWindow' v1
{-# INLINE raiseWindow #-}

restoreWindow :: MonadIO m => Window -> m ()
restoreWindow v1 = liftIO $ restoreWindow' v1
{-# INLINE restoreWindow #-}

setWindowBordered :: MonadIO m => Window -> Bool -> m ()
setWindowBordered v1 v2 = liftIO $ setWindowBordered' v1 v2
{-# INLINE setWindowBordered #-}

setWindowBrightness :: MonadIO m => Window -> CFloat -> m CInt
setWindowBrightness v1 v2 = liftIO $ setWindowBrightness' v1 v2
{-# INLINE setWindowBrightness #-}

setWindowData :: MonadIO m => Window -> CString -> Ptr () -> m (Ptr ())
setWindowData v1 v2 v3 = liftIO $ setWindowData' v1 v2 v3
{-# INLINE setWindowData #-}

setWindowDisplayMode :: MonadIO m => Window -> Ptr DisplayMode -> m CInt
setWindowDisplayMode v1 v2 = liftIO $ setWindowDisplayMode' v1 v2
{-# INLINE setWindowDisplayMode #-}

setWindowFullscreen :: MonadIO m => Window -> Word32 -> m CInt
setWindowFullscreen v1 v2 = liftIO $ setWindowFullscreen' v1 v2
{-# INLINE setWindowFullscreen #-}

setWindowGammaRamp :: MonadIO m => Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> m CInt
setWindowGammaRamp v1 v2 v3 v4 = liftIO $ setWindowGammaRamp' v1 v2 v3 v4
{-# INLINE setWindowGammaRamp #-}

setWindowGrab :: MonadIO m => Window -> Bool -> m ()
setWindowGrab v1 v2 = liftIO $ setWindowGrab' v1 v2
{-# INLINE setWindowGrab #-}

setWindowIcon :: MonadIO m => Window -> Ptr Surface -> m ()
setWindowIcon v1 v2 = liftIO $ setWindowIcon' v1 v2
{-# INLINE setWindowIcon #-}

setWindowMaximumSize :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowMaximumSize v1 v2 v3 = liftIO $ setWindowMaximumSize' v1 v2 v3
{-# INLINE setWindowMaximumSize #-}

setWindowMinimumSize :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowMinimumSize v1 v2 v3 = liftIO $ setWindowMinimumSize' v1 v2 v3
{-# INLINE setWindowMinimumSize #-}

setWindowPosition :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowPosition v1 v2 v3 = liftIO $ setWindowPosition' v1 v2 v3
{-# INLINE setWindowPosition #-}

setWindowSize :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowSize v1 v2 v3 = liftIO $ setWindowSize' v1 v2 v3
{-# INLINE setWindowSize #-}

setWindowTitle :: MonadIO m => Window -> CString -> m ()
setWindowTitle v1 v2 = liftIO $ setWindowTitle' v1 v2
{-# INLINE setWindowTitle #-}

showMessageBox :: MonadIO m => Ptr MessageBoxData -> Ptr CInt -> m CInt
showMessageBox v1 v2 = liftIO $ showMessageBox' v1 v2
{-# INLINE showMessageBox #-}

showSimpleMessageBox :: MonadIO m => Word32 -> CString -> CString -> Window -> m CInt
showSimpleMessageBox v1 v2 v3 v4 = liftIO $ showSimpleMessageBox' v1 v2 v3 v4
{-# INLINE showSimpleMessageBox #-}

showWindow :: MonadIO m => Window -> m ()
showWindow v1 = liftIO $ showWindow' v1
{-# INLINE showWindow #-}

updateWindowSurface :: MonadIO m => Window -> m CInt
updateWindowSurface v1 = liftIO $ updateWindowSurface' v1
{-# INLINE updateWindowSurface #-}

updateWindowSurfaceRects :: MonadIO m => Window -> Ptr Rect -> CInt -> m CInt
updateWindowSurfaceRects v1 v2 v3 = liftIO $ updateWindowSurfaceRects' v1 v2 v3
{-# INLINE updateWindowSurfaceRects #-}

videoInit :: MonadIO m => CString -> m CInt
videoInit v1 = liftIO $ videoInit' v1
{-# INLINE videoInit #-}

videoQuit :: MonadIO m => m ()
videoQuit = liftIO videoQuit'
{-# INLINE videoQuit #-}

createRenderer :: MonadIO m => Window -> CInt -> Word32 -> m Renderer
createRenderer v1 v2 v3 = liftIO $ createRenderer' v1 v2 v3
{-# INLINE createRenderer #-}

createSoftwareRenderer :: MonadIO m => Ptr Surface -> m Renderer
createSoftwareRenderer v1 = liftIO $ createSoftwareRenderer' v1
{-# INLINE createSoftwareRenderer #-}

createTexture :: MonadIO m => Renderer -> Word32 -> CInt -> CInt -> CInt -> m Texture
createTexture v1 v2 v3 v4 v5 = liftIO $ createTexture' v1 v2 v3 v4 v5
{-# INLINE createTexture #-}

createTextureFromSurface :: MonadIO m => Renderer -> Ptr Surface -> m Texture
createTextureFromSurface v1 v2 = liftIO $ createTextureFromSurface' v1 v2
{-# INLINE createTextureFromSurface #-}

destroyRenderer :: MonadIO m => Renderer -> m ()
destroyRenderer v1 = liftIO $ destroyRenderer' v1
{-# INLINE destroyRenderer #-}

destroyTexture :: MonadIO m => Texture -> m ()
destroyTexture v1 = liftIO $ destroyTexture' v1
{-# INLINE destroyTexture #-}

getNumRenderDrivers :: MonadIO m => m CInt
getNumRenderDrivers = liftIO getNumRenderDrivers'
{-# INLINE getNumRenderDrivers #-}

getRenderDrawBlendMode :: MonadIO m => Renderer -> Ptr BlendMode -> m Int
getRenderDrawBlendMode v1 v2 = liftIO $ getRenderDrawBlendMode' v1 v2
{-# INLINE getRenderDrawBlendMode #-}

getRenderDrawColor :: MonadIO m => Renderer -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m CInt
getRenderDrawColor v1 v2 v3 v4 v5 = liftIO $ getRenderDrawColor' v1 v2 v3 v4 v5
{-# INLINE getRenderDrawColor #-}

getRenderDriverInfo :: MonadIO m => CInt -> Ptr RendererInfo -> m CInt
getRenderDriverInfo v1 v2 = liftIO $ getRenderDriverInfo' v1 v2
{-# INLINE getRenderDriverInfo #-}

getRenderTarget :: MonadIO m => Renderer -> m Texture
getRenderTarget v1 = liftIO $ getRenderTarget' v1
{-# INLINE getRenderTarget #-}

getRenderer :: MonadIO m => Window -> m Renderer
getRenderer v1 = liftIO $ getRenderer' v1
{-# INLINE getRenderer #-}

getRendererInfo :: MonadIO m => Renderer -> Ptr RendererInfo -> m CInt
getRendererInfo v1 v2 = liftIO $ getRendererInfo' v1 v2
{-# INLINE getRendererInfo #-}

getRendererOutputSize :: MonadIO m => Renderer -> Ptr CInt -> Ptr CInt -> m CInt
getRendererOutputSize v1 v2 v3 = liftIO $ getRendererOutputSize' v1 v2 v3
{-# INLINE getRendererOutputSize #-}

getTextureAlphaMod :: MonadIO m => Texture -> Ptr Word8 -> m CInt
getTextureAlphaMod v1 v2 = liftIO $ getTextureAlphaMod' v1 v2
{-# INLINE getTextureAlphaMod #-}

getTextureBlendMode :: MonadIO m => Texture -> Ptr BlendMode -> m CInt
getTextureBlendMode v1 v2 = liftIO $ getTextureBlendMode' v1 v2
{-# INLINE getTextureBlendMode #-}

getTextureColorMod :: MonadIO m => Texture -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m CInt
getTextureColorMod v1 v2 v3 v4 = liftIO $ getTextureColorMod' v1 v2 v3 v4
{-# INLINE getTextureColorMod #-}

lockTexture :: MonadIO m => Texture -> Ptr Rect -> Ptr (Ptr ()) -> Ptr CInt -> m CInt
lockTexture v1 v2 v3 v4 = liftIO $ lockTexture' v1 v2 v3 v4
{-# INLINE lockTexture #-}

queryTexture :: MonadIO m => Texture -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> Ptr CInt -> m CInt
queryTexture v1 v2 v3 v4 v5 = liftIO $ queryTexture' v1 v2 v3 v4 v5
{-# INLINE queryTexture #-}

renderClear :: MonadIO m => Renderer -> m CInt
renderClear v1 = liftIO $ renderClear' v1
{-# INLINE renderClear #-}

renderCopy :: MonadIO m => Renderer -> Texture -> Ptr Rect -> Ptr Rect -> m CInt
renderCopy v1 v2 v3 v4 = liftIO $ renderCopy' v1 v2 v3 v4
{-# INLINE renderCopy #-}

renderCopyEx :: MonadIO m => Renderer -> Texture -> Ptr Rect -> Ptr Rect -> CDouble -> Ptr Point -> RendererFlip -> m CInt
renderCopyEx v1 v2 v3 v4 v5 v6 v7 = liftIO $ renderCopyEx' v1 v2 v3 v4 v5 v6 v7
{-# INLINE renderCopyEx #-}

renderDrawLine :: MonadIO m => Renderer -> CInt -> CInt -> CInt -> CInt -> m CInt
renderDrawLine v1 v2 v3 v4 v5 = liftIO $ renderDrawLine' v1 v2 v3 v4 v5
{-# INLINE renderDrawLine #-}

renderDrawLines :: MonadIO m => Renderer -> Ptr Point -> CInt -> m CInt
renderDrawLines v1 v2 v3 = liftIO $ renderDrawLines' v1 v2 v3
{-# INLINE renderDrawLines #-}

renderDrawPoint :: MonadIO m => Renderer -> CInt -> CInt -> m CInt
renderDrawPoint v1 v2 v3 = liftIO $ renderDrawPoint' v1 v2 v3
{-# INLINE renderDrawPoint #-}

renderDrawPoints :: MonadIO m => Renderer -> Ptr Point -> CInt -> m CInt
renderDrawPoints v1 v2 v3 = liftIO $ renderDrawPoints' v1 v2 v3
{-# INLINE renderDrawPoints #-}

renderDrawRect :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderDrawRect v1 v2 = liftIO $ renderDrawRect' v1 v2
{-# INLINE renderDrawRect #-}

renderDrawRects :: MonadIO m => Renderer -> Ptr Rect -> CInt -> m CInt
renderDrawRects v1 v2 v3 = liftIO $ renderDrawRects' v1 v2 v3
{-# INLINE renderDrawRects #-}

renderFillRect :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderFillRect v1 v2 = liftIO $ renderFillRect' v1 v2
{-# INLINE renderFillRect #-}

renderFillRects :: MonadIO m => Renderer -> Ptr Rect -> CInt -> m CInt
renderFillRects v1 v2 v3 = liftIO $ renderFillRects' v1 v2 v3
{-# INLINE renderFillRects #-}

renderGetClipRect :: MonadIO m => Renderer -> Ptr Rect -> m ()
renderGetClipRect v1 v2 = liftIO $ renderGetClipRect' v1 v2
{-# INLINE renderGetClipRect #-}

renderGetLogicalSize :: MonadIO m => Renderer -> Ptr CInt -> Ptr CInt -> m ()
renderGetLogicalSize v1 v2 v3 = liftIO $ renderGetLogicalSize' v1 v2 v3
{-# INLINE renderGetLogicalSize #-}

renderGetScale :: MonadIO m => Renderer -> Ptr CFloat -> Ptr CFloat -> m ()
renderGetScale v1 v2 v3 = liftIO $ renderGetScale' v1 v2 v3
{-# INLINE renderGetScale #-}

renderGetViewport :: MonadIO m => Renderer -> Ptr Rect -> m ()
renderGetViewport v1 v2 = liftIO $ renderGetViewport' v1 v2
{-# INLINE renderGetViewport #-}

renderPresent :: MonadIO m => Renderer -> m ()
renderPresent v1 = liftIO $ renderPresent' v1
{-# INLINE renderPresent #-}

renderReadPixels :: MonadIO m => Renderer -> Ptr Rect -> Word32 -> Ptr () -> CInt -> m CInt
renderReadPixels v1 v2 v3 v4 v5 = liftIO $ renderReadPixels' v1 v2 v3 v4 v5
{-# INLINE renderReadPixels #-}

renderSetClipRect :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderSetClipRect v1 v2 = liftIO $ renderSetClipRect' v1 v2
{-# INLINE renderSetClipRect #-}

renderSetLogicalSize :: MonadIO m => Renderer -> CInt -> CInt -> m CInt
renderSetLogicalSize v1 v2 v3 = liftIO $ renderSetLogicalSize' v1 v2 v3
{-# INLINE renderSetLogicalSize #-}

renderSetScale :: MonadIO m => Renderer -> CFloat -> CFloat -> m CInt
renderSetScale v1 v2 v3 = liftIO $ renderSetScale' v1 v2 v3
{-# INLINE renderSetScale #-}

renderSetViewport :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderSetViewport v1 v2 = liftIO $ renderSetViewport' v1 v2
{-# INLINE renderSetViewport #-}

renderTargetSupported :: MonadIO m => Renderer -> m Bool
renderTargetSupported v1 = liftIO $ renderTargetSupported' v1
{-# INLINE renderTargetSupported #-}

setRenderDrawBlendMode :: MonadIO m => Renderer -> BlendMode -> m CInt
setRenderDrawBlendMode v1 v2 = liftIO $ setRenderDrawBlendMode' v1 v2
{-# INLINE setRenderDrawBlendMode #-}

setRenderDrawColor :: MonadIO m => Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> m CInt
setRenderDrawColor v1 v2 v3 v4 v5 = liftIO $ setRenderDrawColor' v1 v2 v3 v4 v5
{-# INLINE setRenderDrawColor #-}

setRenderTarget :: MonadIO m => Renderer -> Texture -> m CInt
setRenderTarget v1 v2 = liftIO $ setRenderTarget' v1 v2
{-# INLINE setRenderTarget #-}

setTextureAlphaMod :: MonadIO m => Texture -> Word8 -> m CInt
setTextureAlphaMod v1 v2 = liftIO $ setTextureAlphaMod' v1 v2
{-# INLINE setTextureAlphaMod #-}

setTextureBlendMode :: MonadIO m => Texture -> BlendMode -> m CInt
setTextureBlendMode v1 v2 = liftIO $ setTextureBlendMode' v1 v2
{-# INLINE setTextureBlendMode #-}

setTextureColorMod :: MonadIO m => Texture -> Word8 -> Word8 -> Word8 -> m CInt
setTextureColorMod v1 v2 v3 v4 = liftIO $ setTextureColorMod' v1 v2 v3 v4
{-# INLINE setTextureColorMod #-}

unlockTexture :: MonadIO m => Texture -> m ()
unlockTexture v1 = liftIO $ unlockTexture' v1
{-# INLINE unlockTexture #-}

updateTexture :: MonadIO m => Texture -> Ptr Rect -> Ptr () -> CInt -> m CInt
updateTexture v1 v2 v3 v4 = liftIO $ updateTexture' v1 v2 v3 v4
{-# INLINE updateTexture #-}

updateYUVTexture :: MonadIO m => Texture -> Ptr Rect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> m CInt
updateYUVTexture v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ updateYUVTexture' v1 v2 v3 v4 v5 v6 v7 v8
{-# INLINE updateYUVTexture #-}

allocFormat :: MonadIO m => Word32 -> m (Ptr PixelFormat)
allocFormat v1 = liftIO $ allocFormat' v1
{-# INLINE allocFormat #-}

allocPalette :: MonadIO m => CInt -> m (Ptr Palette)
allocPalette v1 = liftIO $ allocPalette' v1
{-# INLINE allocPalette #-}

calculateGammaRamp :: MonadIO m => CFloat -> Ptr Word16 -> m ()
calculateGammaRamp v1 v2 = liftIO $ calculateGammaRamp' v1 v2
{-# INLINE calculateGammaRamp #-}

freeFormat :: MonadIO m => Ptr PixelFormat -> m ()
freeFormat v1 = liftIO $ freeFormat' v1
{-# INLINE freeFormat #-}

freePalette :: MonadIO m => Ptr Palette -> m ()
freePalette v1 = liftIO $ freePalette' v1
{-# INLINE freePalette #-}

getPixelFormatName :: MonadIO m => Word32 -> m CString
getPixelFormatName v1 = liftIO $ getPixelFormatName' v1
{-# INLINE getPixelFormatName #-}

getRGB :: MonadIO m => Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m ()
getRGB v1 v2 v3 v4 v5 = liftIO $ getRGB' v1 v2 v3 v4 v5
{-# INLINE getRGB #-}

getRGBA :: MonadIO m => Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m ()
getRGBA v1 v2 v3 v4 v5 v6 = liftIO $ getRGBA' v1 v2 v3 v4 v5 v6
{-# INLINE getRGBA #-}

mapRGB :: MonadIO m => Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> m Word32
mapRGB v1 v2 v3 v4 = liftIO $ mapRGB' v1 v2 v3 v4
{-# INLINE mapRGB #-}

mapRGBA :: MonadIO m => Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> m Word32
mapRGBA v1 v2 v3 v4 v5 = liftIO $ mapRGBA' v1 v2 v3 v4 v5
{-# INLINE mapRGBA #-}

masksToPixelFormatEnum :: MonadIO m => CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m Word32
masksToPixelFormatEnum v1 v2 v3 v4 v5 = liftIO $ masksToPixelFormatEnum' v1 v2 v3 v4 v5
{-# INLINE masksToPixelFormatEnum #-}

pixelFormatEnumToMasks :: MonadIO m => Word32 -> Ptr CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> m Bool
pixelFormatEnumToMasks v1 v2 v3 v4 v5 v6 = liftIO $ pixelFormatEnumToMasks' v1 v2 v3 v4 v5 v6
{-# INLINE pixelFormatEnumToMasks #-}

setPaletteColors :: MonadIO m => Ptr Palette -> Ptr Color -> CInt -> CInt -> m CInt
setPaletteColors v1 v2 v3 v4 = liftIO $ setPaletteColors' v1 v2 v3 v4
{-# INLINE setPaletteColors #-}

setPixelFormatPalette :: MonadIO m => Ptr PixelFormat -> Ptr Palette -> m CInt
setPixelFormatPalette v1 v2 = liftIO $ setPixelFormatPalette' v1 v2
{-# INLINE setPixelFormatPalette #-}

enclosePoints :: MonadIO m => Ptr Point -> CInt -> Ptr Rect -> Ptr Rect -> m Bool
enclosePoints v1 v2 v3 v4 = liftIO $ enclosePoints' v1 v2 v3 v4
{-# INLINE enclosePoints #-}

hasIntersection :: MonadIO m => Ptr Rect -> Ptr Rect -> m Bool
hasIntersection v1 v2 = liftIO $ hasIntersection' v1 v2
{-# INLINE hasIntersection #-}

intersectRect :: MonadIO m => Ptr Rect -> Ptr Rect -> Ptr Rect -> m Bool
intersectRect v1 v2 v3 = liftIO $ intersectRect' v1 v2 v3
{-# INLINE intersectRect #-}

intersectRectAndLine :: MonadIO m => Ptr Rect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> m Bool
intersectRectAndLine v1 v2 v3 v4 v5 = liftIO $ intersectRectAndLine' v1 v2 v3 v4 v5
{-# INLINE intersectRectAndLine #-}

unionRect :: MonadIO m => Ptr Rect -> Ptr Rect -> Ptr Rect -> m ()
unionRect v1 v2 v3 = liftIO $ unionRect' v1 v2 v3
{-# INLINE unionRect #-}

blitScaled :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
blitScaled v1 v2 v3 v4 = liftIO $ blitScaled' v1 v2 v3 v4
{-# INLINE blitScaled #-}

blitSurface :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
blitSurface v1 v2 v3 v4 = liftIO $ blitSurface' v1 v2 v3 v4
{-# INLINE blitSurface #-}

convertPixels :: MonadIO m => CInt -> CInt -> Word32 -> Ptr () -> CInt -> Word32 -> Ptr () -> CInt -> m CInt
convertPixels v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ convertPixels' v1 v2 v3 v4 v5 v6 v7 v8
{-# INLINE convertPixels #-}

convertSurface :: MonadIO m => Ptr Surface -> Ptr PixelFormat -> Word32 -> m (Ptr Surface)
convertSurface v1 v2 v3 = liftIO $ convertSurface' v1 v2 v3
{-# INLINE convertSurface #-}

convertSurfaceFormat :: MonadIO m => Ptr Surface -> Word32 -> Word32 -> m (Ptr Surface)
convertSurfaceFormat v1 v2 v3 = liftIO $ convertSurfaceFormat' v1 v2 v3
{-# INLINE convertSurfaceFormat #-}

createRGBSurface :: MonadIO m => Word32 -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m (Ptr Surface)
createRGBSurface v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ createRGBSurface' v1 v2 v3 v4 v5 v6 v7 v8
{-# INLINE createRGBSurface #-}

createRGBSurfaceFrom :: MonadIO m => Ptr () -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m (Ptr Surface)
createRGBSurfaceFrom v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ createRGBSurfaceFrom' v1 v2 v3 v4 v5 v6 v7 v8 v9
{-# INLINE createRGBSurfaceFrom #-}

fillRect :: MonadIO m => Ptr Surface -> Ptr Rect -> Word32 -> m CInt
fillRect v1 v2 v3 = liftIO $ fillRect' v1 v2 v3
{-# INLINE fillRect #-}

fillRects :: MonadIO m => Ptr Surface -> Ptr Rect -> CInt -> Word32 -> m CInt
fillRects v1 v2 v3 v4 = liftIO $ fillRects' v1 v2 v3 v4
{-# INLINE fillRects #-}

freeSurface :: MonadIO m => Ptr Surface -> m ()
freeSurface v1 = liftIO $ freeSurface' v1
{-# INLINE freeSurface #-}

getClipRect :: MonadIO m => Ptr Surface -> Ptr Rect -> m ()
getClipRect v1 v2 = liftIO $ getClipRect' v1 v2
{-# INLINE getClipRect #-}

getColorKey :: MonadIO m => Ptr Surface -> Ptr Word32 -> m CInt
getColorKey v1 v2 = liftIO $ getColorKey' v1 v2
{-# INLINE getColorKey #-}

getSurfaceAlphaMod :: MonadIO m => Ptr Surface -> Ptr Word8 -> m CInt
getSurfaceAlphaMod v1 v2 = liftIO $ getSurfaceAlphaMod' v1 v2
{-# INLINE getSurfaceAlphaMod #-}

getSurfaceBlendMode :: MonadIO m => Ptr Surface -> BlendMode -> m CInt
getSurfaceBlendMode v1 v2 = liftIO $ getSurfaceBlendMode' v1 v2
{-# INLINE getSurfaceBlendMode #-}

getSurfaceColorMod :: MonadIO m => Ptr Surface -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m CInt
getSurfaceColorMod v1 v2 v3 v4 = liftIO $ getSurfaceColorMod' v1 v2 v3 v4
{-# INLINE getSurfaceColorMod #-}

loadBMP :: MonadIO m => CString -> m (Ptr Surface)
loadBMP file = liftIO $ do
  rw <- withCString "rb" $ rwFromFile file
  loadBMP_RW rw 1
{-# INLINE loadBMP #-}

loadBMP_RW :: MonadIO m => Ptr RWops -> CInt -> m (Ptr Surface)
loadBMP_RW v1 v2 = liftIO $ loadBMP_RW' v1 v2
{-# INLINE loadBMP_RW #-}

lockSurface :: MonadIO m => Ptr Surface -> m CInt
lockSurface v1 = liftIO $ lockSurface' v1
{-# INLINE lockSurface #-}

lowerBlit :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
lowerBlit v1 v2 v3 v4 = liftIO $ lowerBlit' v1 v2 v3 v4
{-# INLINE lowerBlit #-}

lowerBlitScaled :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
lowerBlitScaled v1 v2 v3 v4 = liftIO $ lowerBlitScaled' v1 v2 v3 v4
{-# INLINE lowerBlitScaled #-}

saveBMP :: MonadIO m => Ptr Surface -> CString -> m CInt
saveBMP surface file = liftIO $ do
  rw <- withCString "wb" $ rwFromFile file
  saveBMP_RW surface rw 1
{-# INLINE saveBMP #-}

saveBMP_RW :: MonadIO m => Ptr Surface -> Ptr RWops -> CInt -> m CInt
saveBMP_RW v1 v2 v3 = liftIO $ saveBMP_RW' v1 v2 v3
{-# INLINE saveBMP_RW #-}

setClipRect :: MonadIO m => Ptr Surface -> Ptr Rect -> m Bool
setClipRect v1 v2 = liftIO $ setClipRect' v1 v2
{-# INLINE setClipRect #-}

setColorKey :: MonadIO m => Ptr Surface -> CInt -> Word32 -> m CInt
setColorKey v1 v2 v3 = liftIO $ setColorKey' v1 v2 v3
{-# INLINE setColorKey #-}

setSurfaceAlphaMod :: MonadIO m => Ptr Surface -> Word8 -> m CInt
setSurfaceAlphaMod v1 v2 = liftIO $ setSurfaceAlphaMod' v1 v2
{-# INLINE setSurfaceAlphaMod #-}

setSurfaceBlendMode :: MonadIO m => Ptr Surface -> BlendMode -> m CInt
setSurfaceBlendMode v1 v2 = liftIO $ setSurfaceBlendMode' v1 v2
{-# INLINE setSurfaceBlendMode #-}

setSurfaceColorMod :: MonadIO m => Ptr Surface -> Word8 -> Word8 -> Word8 -> m CInt
setSurfaceColorMod v1 v2 v3 v4 = liftIO $ setSurfaceColorMod' v1 v2 v3 v4
{-# INLINE setSurfaceColorMod #-}

setSurfacePalette :: MonadIO m => Ptr Surface -> Ptr Palette -> m CInt
setSurfacePalette v1 v2 = liftIO $ setSurfacePalette' v1 v2
{-# INLINE setSurfacePalette #-}

setSurfaceRLE :: MonadIO m => Ptr Surface -> CInt -> m CInt
setSurfaceRLE v1 v2 = liftIO $ setSurfaceRLE' v1 v2
{-# INLINE setSurfaceRLE #-}

unlockSurface :: MonadIO m => Ptr Surface -> m ()
unlockSurface v1 = liftIO $ unlockSurface' v1
{-# INLINE unlockSurface #-}

getWindowWMInfo :: MonadIO m => Window -> SysWMinfo -> m Bool
getWindowWMInfo v1 v2 = liftIO $ getWindowWMInfo' v1 v2
{-# INLINE getWindowWMInfo #-}

getClipboardText :: MonadIO m => m CString
getClipboardText = liftIO getClipboardText'
{-# INLINE getClipboardText #-}

hasClipboardText :: MonadIO m => m Bool
hasClipboardText = liftIO hasClipboardText'
{-# INLINE hasClipboardText #-}

setClipboardText :: MonadIO m => CString -> m CInt
setClipboardText v1 = liftIO $ setClipboardText' v1
{-# INLINE setClipboardText #-}
