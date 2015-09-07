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

foreign import ccall "SDL.h SDL_CreateWindow" createWindowFFI :: CString -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO Window
foreign import ccall "SDL.h SDL_CreateWindowAndRenderer" createWindowAndRendererFFI :: CInt -> CInt -> Word32 -> Ptr Window -> Ptr Renderer -> IO CInt
foreign import ccall "SDL.h SDL_CreateWindowFrom" createWindowFromFFI :: Ptr () -> IO Window
foreign import ccall "SDL.h SDL_DestroyWindow" destroyWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_DisableScreenSaver" disableScreenSaverFFI :: IO ()
foreign import ccall "SDL.h SDL_EnableScreenSaver" enableScreenSaverFFI :: IO ()
foreign import ccall "SDL.h SDL_GL_BindTexture" glBindTextureFFI :: Texture -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "SDL.h SDL_GL_CreateContext" glCreateContextFFI :: Window -> IO GLContext
foreign import ccall "SDL.h SDL_GL_DeleteContext" glDeleteContextFFI :: GLContext -> IO ()
foreign import ccall "SDL.h SDL_GL_ExtensionSupported" glExtensionSupportedFFI :: CString -> IO Bool
foreign import ccall "SDL.h SDL_GL_GetAttribute" glGetAttributeFFI :: GLattr -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_GetCurrentContext" glGetCurrentContextFFI :: IO GLContext
foreign import ccall "SDL.h SDL_GL_GetCurrentWindow" glGetCurrentWindowFFI :: IO Window
foreign import ccall "SDL.h SDL_GL_GetDrawableSize" glGetDrawableSizeFFI :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GL_GetProcAddress" glGetProcAddressFFI :: CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GL_GetSwapInterval" glGetSwapIntervalFFI :: IO CInt
foreign import ccall "SDL.h SDL_GL_LoadLibrary" glLoadLibraryFFI :: CString -> IO CInt
foreign import ccall "SDL.h SDL_GL_MakeCurrent" glMakeCurrentFFI :: Window -> GLContext -> IO CInt
foreign import ccall "SDL.h SDL_GL_ResetAttributes" glResetAttributesFFI :: IO ()
foreign import ccall "SDL.h SDL_GL_SetAttribute" glSetAttributeFFI :: GLattr -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_SetSwapInterval" glSetSwapIntervalFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_SwapWindow" glSwapWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_GL_UnbindTexture" glUnbindTextureFFI :: Texture -> IO CInt
foreign import ccall "SDL.h SDL_GL_UnloadLibrary" glUnloadLibraryFFI :: IO ()
foreign import ccall "SDL.h SDL_GetClosestDisplayMode" getClosestDisplayModeFFI :: CInt -> Ptr DisplayMode -> Ptr DisplayMode -> IO (Ptr DisplayMode)
foreign import ccall "SDL.h SDL_GetCurrentDisplayMode" getCurrentDisplayModeFFI :: CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetCurrentVideoDriver" getCurrentVideoDriverFFI :: IO CString
foreign import ccall "SDL.h SDL_GetDesktopDisplayMode" getDesktopDisplayModeFFI :: CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayBounds" getDisplayBoundsFFI :: CInt -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayMode" getDisplayModeFFI :: CInt -> CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayName" getDisplayNameFFI :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetNumDisplayModes" getNumDisplayModesFFI :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetNumVideoDisplays" getNumVideoDisplaysFFI :: IO CInt
foreign import ccall "SDL.h SDL_GetNumVideoDrivers" getNumVideoDriversFFI :: IO CInt
foreign import ccall "SDL.h SDL_GetVideoDriver" getVideoDriverFFI :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetWindowBrightness" getWindowBrightnessFFI :: Window -> IO CFloat
foreign import ccall "SDL.h SDL_GetWindowData" getWindowDataFFI :: Window -> CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GetWindowDisplayIndex" getWindowDisplayIndexFFI :: Window -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowDisplayMode" getWindowDisplayModeFFI :: Window -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowFlags" getWindowFlagsFFI :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowFromID" getWindowFromIDFFI :: Word32 -> IO Window
foreign import ccall "SDL.h SDL_GetWindowGammaRamp" getWindowGammaRampFFI :: Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowGrab" getWindowGrabFFI :: Window -> IO Bool
foreign import ccall "SDL.h SDL_GetWindowID" getWindowIDFFI :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowMaximumSize" getWindowMaximumSizeFFI :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowMinimumSize" getWindowMinimumSizeFFI :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowPixelFormat" getWindowPixelFormatFFI :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowPosition" getWindowPositionFFI :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowSize" getWindowSizeFFI :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowSurface" getWindowSurfaceFFI :: Window -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_GetWindowTitle" getWindowTitleFFI :: Window -> IO CString
foreign import ccall "SDL.h SDL_HideWindow" hideWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_IsScreenSaverEnabled" isScreenSaverEnabledFFI :: IO Bool
foreign import ccall "SDL.h SDL_MaximizeWindow" maximizeWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_MinimizeWindow" minimizeWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_RaiseWindow" raiseWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_RestoreWindow" restoreWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_SetWindowBordered" setWindowBorderedFFI :: Window -> Bool -> IO ()
foreign import ccall "SDL.h SDL_SetWindowBrightness" setWindowBrightnessFFI :: Window -> CFloat -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowData" setWindowDataFFI :: Window -> CString -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_SetWindowDisplayMode" setWindowDisplayModeFFI :: Window -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowFullscreen" setWindowFullscreenFFI :: Window -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowGammaRamp" setWindowGammaRampFFI :: Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowGrab" setWindowGrabFFI :: Window -> Bool -> IO ()
foreign import ccall "SDL.h SDL_SetWindowIcon" setWindowIconFFI :: Window -> Ptr Surface -> IO ()
foreign import ccall "SDL.h SDL_SetWindowMaximumSize" setWindowMaximumSizeFFI :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowMinimumSize" setWindowMinimumSizeFFI :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowPosition" setWindowPositionFFI :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowSize" setWindowSizeFFI :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowTitle" setWindowTitleFFI :: Window -> CString -> IO ()
foreign import ccall "SDL.h SDL_ShowMessageBox" showMessageBoxFFI :: Ptr MessageBoxData -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_ShowSimpleMessageBox" showSimpleMessageBoxFFI :: Word32 -> CString -> CString -> Window -> IO CInt
foreign import ccall "SDL.h SDL_ShowWindow" showWindowFFI :: Window -> IO ()
foreign import ccall "SDL.h SDL_UpdateWindowSurface" updateWindowSurfaceFFI :: Window -> IO CInt
foreign import ccall "SDL.h SDL_UpdateWindowSurfaceRects" updateWindowSurfaceRectsFFI :: Window -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_VideoInit" videoInitFFI :: CString -> IO CInt
foreign import ccall "SDL.h SDL_VideoQuit" videoQuitFFI :: IO ()

foreign import ccall "SDL.h SDL_CreateRenderer" createRendererFFI :: Window -> CInt -> Word32 -> IO Renderer
foreign import ccall "SDL.h SDL_CreateSoftwareRenderer" createSoftwareRendererFFI :: Ptr Surface -> IO Renderer
foreign import ccall "SDL.h SDL_CreateTexture" createTextureFFI :: Renderer -> Word32 -> CInt -> CInt -> CInt -> IO Texture
foreign import ccall "SDL.h SDL_CreateTextureFromSurface" createTextureFromSurfaceFFI :: Renderer -> Ptr Surface -> IO Texture
foreign import ccall "SDL.h SDL_DestroyRenderer" destroyRendererFFI :: Renderer -> IO ()
foreign import ccall "SDL.h SDL_DestroyTexture" destroyTextureFFI :: Texture -> IO ()
foreign import ccall "SDL.h SDL_GetNumRenderDrivers" getNumRenderDriversFFI :: IO CInt
foreign import ccall "SDL.h SDL_GetRenderDrawBlendMode" getRenderDrawBlendModeFFI :: Renderer -> Ptr BlendMode -> IO Int
foreign import ccall "SDL.h SDL_GetRenderDrawColor" getRenderDrawColorFFI :: Renderer -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetRenderDriverInfo" getRenderDriverInfoFFI :: CInt -> Ptr RendererInfo -> IO CInt
foreign import ccall "SDL.h SDL_GetRenderTarget" getRenderTargetFFI :: Renderer -> IO Texture
foreign import ccall "SDL.h SDL_GetRenderer" getRendererFFI :: Window -> IO Renderer
foreign import ccall "SDL.h SDL_GetRendererInfo" getRendererInfoFFI :: Renderer -> Ptr RendererInfo -> IO CInt
foreign import ccall "SDL.h SDL_GetRendererOutputSize" getRendererOutputSizeFFI :: Renderer -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureAlphaMod" getTextureAlphaModFFI :: Texture -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureBlendMode" getTextureBlendModeFFI :: Texture -> Ptr BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureColorMod" getTextureColorModFFI :: Texture -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_LockTexture" lockTextureFFI :: Texture -> Ptr Rect -> Ptr (Ptr ()) -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_QueryTexture" queryTextureFFI :: Texture -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderClear" renderClearFFI :: Renderer -> IO CInt
foreign import ccall "SDL.h SDL_RenderCopy" renderCopyFFI :: Renderer -> Texture -> Ptr Rect -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderCopyEx" renderCopyExFFI :: Renderer -> Texture -> Ptr Rect -> Ptr Rect -> CDouble -> Ptr Point -> RendererFlip -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawLine" renderDrawLineFFI :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawLines" renderDrawLinesFFI :: Renderer -> Ptr Point -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawPoint" renderDrawPointFFI :: Renderer -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawPoints" renderDrawPointsFFI :: Renderer -> Ptr Point -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawRect" renderDrawRectFFI :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawRects" renderDrawRectsFFI :: Renderer -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderFillRect" renderFillRectFFI :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderFillRects" renderFillRectsFFI :: Renderer -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderGetClipRect" renderGetClipRectFFI :: Renderer -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_RenderGetLogicalSize" renderGetLogicalSizeFFI :: Renderer -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_RenderGetScale" renderGetScaleFFI :: Renderer -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall "SDL.h SDL_RenderGetViewport" renderGetViewportFFI :: Renderer -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_RenderPresent" renderPresentFFI :: Renderer -> IO ()
foreign import ccall "SDL.h SDL_RenderReadPixels" renderReadPixelsFFI :: Renderer -> Ptr Rect -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetClipRect" renderSetClipRectFFI :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetLogicalSize" renderSetLogicalSizeFFI :: Renderer -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetScale" renderSetScaleFFI :: Renderer -> CFloat -> CFloat -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetViewport" renderSetViewportFFI :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderTargetSupported" renderTargetSupportedFFI :: Renderer -> IO Bool
foreign import ccall "SDL.h SDL_SetRenderDrawBlendMode" setRenderDrawBlendModeFFI :: Renderer -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetRenderDrawColor" setRenderDrawColorFFI :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetRenderTarget" setRenderTargetFFI :: Renderer -> Texture -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureAlphaMod" setTextureAlphaModFFI :: Texture -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureBlendMode" setTextureBlendModeFFI :: Texture -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureColorMod" setTextureColorModFFI :: Texture -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_UnlockTexture" unlockTextureFFI :: Texture -> IO ()
foreign import ccall "SDL.h SDL_UpdateTexture" updateTextureFFI :: Texture -> Ptr Rect -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_UpdateYUVTexture" updateYUVTextureFFI :: Texture -> Ptr Rect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "SDL.h SDL_AllocFormat" allocFormatFFI :: Word32 -> IO (Ptr PixelFormat)
foreign import ccall "SDL.h SDL_AllocPalette" allocPaletteFFI :: CInt -> IO (Ptr Palette)
foreign import ccall "SDL.h SDL_CalculateGammaRamp" calculateGammaRampFFI :: CFloat -> Ptr Word16 -> IO ()
foreign import ccall "SDL.h SDL_FreeFormat" freeFormatFFI :: Ptr PixelFormat -> IO ()
foreign import ccall "SDL.h SDL_FreePalette" freePaletteFFI :: Ptr Palette -> IO ()
foreign import ccall "SDL.h SDL_GetPixelFormatName" getPixelFormatNameFFI :: Word32 -> IO CString
foreign import ccall "SDL.h SDL_GetRGB" getRGBFFI :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetRGBA" getRGBAFFI :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_MapRGB" mapRGBFFI :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MapRGBA" mapRGBAFFI :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MasksToPixelFormatEnum" masksToPixelFormatEnumFFI :: CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO Word32
foreign import ccall "SDL.h SDL_PixelFormatEnumToMasks" pixelFormatEnumToMasksFFI :: Word32 -> Ptr CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Bool
foreign import ccall "SDL.h SDL_SetPaletteColors" setPaletteColorsFFI :: Ptr Palette -> Ptr Color -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_SetPixelFormatPalette" setPixelFormatPaletteFFI :: Ptr PixelFormat -> Ptr Palette -> IO CInt

foreign import ccall "SDL.h SDL_EnclosePoints" enclosePointsFFI :: Ptr Point -> CInt -> Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_HasIntersection" hasIntersectionFFI :: Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_IntersectRect" intersectRectFFI :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_IntersectRectAndLine" intersectRectAndLineFFI :: Ptr Rect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Bool
foreign import ccall "SDL.h SDL_UnionRect" unionRectFFI :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO ()

foreign import ccall "SDL.h SDL_UpperBlitScaled" blitScaledFFI :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_UpperBlit" blitSurfaceFFI :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_ConvertPixels" convertPixelsFFI :: CInt -> CInt -> Word32 -> Ptr () -> CInt -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_ConvertSurface" convertSurfaceFFI :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_ConvertSurfaceFormat" convertSurfaceFormatFFI :: Ptr Surface -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_CreateRGBSurface" createRGBSurfaceFFI :: Word32 -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_CreateRGBSurfaceFrom" createRGBSurfaceFromFFI :: Ptr () -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_FillRect" fillRectFFI :: Ptr Surface -> Ptr Rect -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_FillRects" fillRectsFFI :: Ptr Surface -> Ptr Rect -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_FreeSurface" freeSurfaceFFI :: Ptr Surface -> IO ()
foreign import ccall "SDL.h SDL_GetClipRect" getClipRectFFI :: Ptr Surface -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_GetColorKey" getColorKeyFFI :: Ptr Surface -> Ptr Word32 -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceAlphaMod" getSurfaceAlphaModFFI :: Ptr Surface -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceBlendMode" getSurfaceBlendModeFFI :: Ptr Surface -> Ptr BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceColorMod" getSurfaceColorModFFI :: Ptr Surface -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_LoadBMP_RW" loadBMP_RWFFI :: Ptr RWops -> CInt -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_LockSurface" lockSurfaceFFI :: Ptr Surface -> IO CInt
foreign import ccall "SDL.h SDL_LowerBlit" lowerBlitFFI :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_LowerBlitScaled" lowerBlitScaledFFI :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_SaveBMP_RW" saveBMP_RWFFI :: Ptr Surface -> Ptr RWops -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_SetClipRect" setClipRectFFI :: Ptr Surface -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_SetColorKey" setColorKeyFFI :: Ptr Surface -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceAlphaMod" setSurfaceAlphaModFFI :: Ptr Surface -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceBlendMode" setSurfaceBlendModeFFI :: Ptr Surface -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceColorMod" setSurfaceColorModFFI :: Ptr Surface -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfacePalette" setSurfacePaletteFFI :: Ptr Surface -> Ptr Palette -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceRLE" setSurfaceRLEFFI :: Ptr Surface -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_UnlockSurface" unlockSurfaceFFI :: Ptr Surface -> IO ()

foreign import ccall "SDL.h SDL_GetWindowWMInfo" getWindowWMInfoFFI :: Window -> SysWMinfo -> IO Bool

foreign import ccall "SDL.h SDL_GetClipboardText" getClipboardTextFFI :: IO CString
foreign import ccall "SDL.h SDL_HasClipboardText" hasClipboardTextFFI :: IO Bool
foreign import ccall "SDL.h SDL_SetClipboardText" setClipboardTextFFI :: CString -> IO CInt

createWindow :: MonadIO m => CString -> CInt -> CInt -> CInt -> CInt -> Word32 -> m Window
createWindow v1 v2 v3 v4 v5 v6 = liftIO $ createWindowFFI v1 v2 v3 v4 v5 v6
{-# INLINE createWindow #-}

createWindowAndRenderer :: MonadIO m => CInt -> CInt -> Word32 -> Ptr Window -> Ptr Renderer -> m CInt
createWindowAndRenderer v1 v2 v3 v4 v5 = liftIO $ createWindowAndRendererFFI v1 v2 v3 v4 v5
{-# INLINE createWindowAndRenderer #-}

createWindowFrom :: MonadIO m => Ptr () -> m Window
createWindowFrom v1 = liftIO $ createWindowFromFFI v1
{-# INLINE createWindowFrom #-}

destroyWindow :: MonadIO m => Window -> m ()
destroyWindow v1 = liftIO $ destroyWindowFFI v1
{-# INLINE destroyWindow #-}

disableScreenSaver :: MonadIO m => m ()
disableScreenSaver = liftIO disableScreenSaverFFI
{-# INLINE disableScreenSaver #-}

enableScreenSaver :: MonadIO m => m ()
enableScreenSaver = liftIO enableScreenSaverFFI
{-# INLINE enableScreenSaver #-}

glBindTexture :: MonadIO m => Texture -> Ptr CFloat -> Ptr CFloat -> m CInt
glBindTexture v1 v2 v3 = liftIO $ glBindTextureFFI v1 v2 v3
{-# INLINE glBindTexture #-}

glCreateContext :: MonadIO m => Window -> m GLContext
glCreateContext v1 = liftIO $ glCreateContextFFI v1
{-# INLINE glCreateContext #-}

glDeleteContext :: MonadIO m => GLContext -> m ()
glDeleteContext v1 = liftIO $ glDeleteContextFFI v1
{-# INLINE glDeleteContext #-}

glExtensionSupported :: MonadIO m => CString -> m Bool
glExtensionSupported v1 = liftIO $ glExtensionSupportedFFI v1
{-# INLINE glExtensionSupported #-}

glGetAttribute :: MonadIO m => GLattr -> Ptr CInt -> m CInt
glGetAttribute v1 v2 = liftIO $ glGetAttributeFFI v1 v2
{-# INLINE glGetAttribute #-}

glGetCurrentContext :: MonadIO m => m GLContext
glGetCurrentContext = liftIO glGetCurrentContextFFI
{-# INLINE glGetCurrentContext #-}

glGetCurrentWindow :: MonadIO m => m Window
glGetCurrentWindow = liftIO glGetCurrentWindowFFI
{-# INLINE glGetCurrentWindow #-}

glGetDrawableSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
glGetDrawableSize v1 v2 v3 = liftIO $ glGetDrawableSizeFFI v1 v2 v3
{-# INLINE glGetDrawableSize #-}

glGetProcAddress :: MonadIO m => CString -> m (Ptr ())
glGetProcAddress v1 = liftIO $ glGetProcAddressFFI v1
{-# INLINE glGetProcAddress #-}

glGetSwapInterval :: MonadIO m => m CInt
glGetSwapInterval = liftIO glGetSwapIntervalFFI
{-# INLINE glGetSwapInterval #-}

glLoadLibrary :: MonadIO m => CString -> m CInt
glLoadLibrary v1 = liftIO $ glLoadLibraryFFI v1
{-# INLINE glLoadLibrary #-}

glMakeCurrent :: MonadIO m => Window -> GLContext -> m CInt
glMakeCurrent v1 v2 = liftIO $ glMakeCurrentFFI v1 v2
{-# INLINE glMakeCurrent #-}

glResetAttributes :: MonadIO m => m ()
glResetAttributes = liftIO glResetAttributesFFI
{-# INLINE glResetAttributes #-}

glSetAttribute :: MonadIO m => GLattr -> CInt -> m CInt
glSetAttribute v1 v2 = liftIO $ glSetAttributeFFI v1 v2
{-# INLINE glSetAttribute #-}

glSetSwapInterval :: MonadIO m => CInt -> m CInt
glSetSwapInterval v1 = liftIO $ glSetSwapIntervalFFI v1
{-# INLINE glSetSwapInterval #-}

glSwapWindow :: MonadIO m => Window -> m ()
glSwapWindow v1 = liftIO $ glSwapWindowFFI v1
{-# INLINE glSwapWindow #-}

glUnbindTexture :: MonadIO m => Texture -> m CInt
glUnbindTexture v1 = liftIO $ glUnbindTextureFFI v1
{-# INLINE glUnbindTexture #-}

glUnloadLibrary :: MonadIO m => m ()
glUnloadLibrary = liftIO glUnloadLibraryFFI
{-# INLINE glUnloadLibrary #-}

getClosestDisplayMode :: MonadIO m => CInt -> Ptr DisplayMode -> Ptr DisplayMode -> m (Ptr DisplayMode)
getClosestDisplayMode v1 v2 v3 = liftIO $ getClosestDisplayModeFFI v1 v2 v3
{-# INLINE getClosestDisplayMode #-}

getCurrentDisplayMode :: MonadIO m => CInt -> Ptr DisplayMode -> m CInt
getCurrentDisplayMode v1 v2 = liftIO $ getCurrentDisplayModeFFI v1 v2
{-# INLINE getCurrentDisplayMode #-}

getCurrentVideoDriver :: MonadIO m => m CString
getCurrentVideoDriver = liftIO getCurrentVideoDriverFFI
{-# INLINE getCurrentVideoDriver #-}

getDesktopDisplayMode :: MonadIO m => CInt -> Ptr DisplayMode -> m CInt
getDesktopDisplayMode v1 v2 = liftIO $ getDesktopDisplayModeFFI v1 v2
{-# INLINE getDesktopDisplayMode #-}

getDisplayBounds :: MonadIO m => CInt -> Ptr Rect -> m CInt
getDisplayBounds v1 v2 = liftIO $ getDisplayBoundsFFI v1 v2
{-# INLINE getDisplayBounds #-}

getDisplayMode :: MonadIO m => CInt -> CInt -> Ptr DisplayMode -> m CInt
getDisplayMode v1 v2 v3 = liftIO $ getDisplayModeFFI v1 v2 v3
{-# INLINE getDisplayMode #-}

getDisplayName :: MonadIO m => CInt -> m CString
getDisplayName v1 = liftIO $ getDisplayNameFFI v1
{-# INLINE getDisplayName #-}

getNumDisplayModes :: MonadIO m => CInt -> m CInt
getNumDisplayModes v1 = liftIO $ getNumDisplayModesFFI v1
{-# INLINE getNumDisplayModes #-}

getNumVideoDisplays :: MonadIO m => m CInt
getNumVideoDisplays = liftIO getNumVideoDisplaysFFI
{-# INLINE getNumVideoDisplays #-}

getNumVideoDrivers :: MonadIO m => m CInt
getNumVideoDrivers = liftIO getNumVideoDriversFFI
{-# INLINE getNumVideoDrivers #-}

getVideoDriver :: MonadIO m => CInt -> m CString
getVideoDriver v1 = liftIO $ getVideoDriverFFI v1
{-# INLINE getVideoDriver #-}

getWindowBrightness :: MonadIO m => Window -> m CFloat
getWindowBrightness v1 = liftIO $ getWindowBrightnessFFI v1
{-# INLINE getWindowBrightness #-}

getWindowData :: MonadIO m => Window -> CString -> m (Ptr ())
getWindowData v1 v2 = liftIO $ getWindowDataFFI v1 v2
{-# INLINE getWindowData #-}

getWindowDisplayIndex :: MonadIO m => Window -> m CInt
getWindowDisplayIndex v1 = liftIO $ getWindowDisplayIndexFFI v1
{-# INLINE getWindowDisplayIndex #-}

getWindowDisplayMode :: MonadIO m => Window -> Ptr DisplayMode -> m CInt
getWindowDisplayMode v1 v2 = liftIO $ getWindowDisplayModeFFI v1 v2
{-# INLINE getWindowDisplayMode #-}

getWindowFlags :: MonadIO m => Window -> m Word32
getWindowFlags v1 = liftIO $ getWindowFlagsFFI v1
{-# INLINE getWindowFlags #-}

getWindowFromID :: MonadIO m => Word32 -> m Window
getWindowFromID v1 = liftIO $ getWindowFromIDFFI v1
{-# INLINE getWindowFromID #-}

getWindowGammaRamp :: MonadIO m => Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> m CInt
getWindowGammaRamp v1 v2 v3 v4 = liftIO $ getWindowGammaRampFFI v1 v2 v3 v4
{-# INLINE getWindowGammaRamp #-}

getWindowGrab :: MonadIO m => Window -> m Bool
getWindowGrab v1 = liftIO $ getWindowGrabFFI v1
{-# INLINE getWindowGrab #-}

getWindowID :: MonadIO m => Window -> m Word32
getWindowID v1 = liftIO $ getWindowIDFFI v1
{-# INLINE getWindowID #-}

getWindowMaximumSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowMaximumSize v1 v2 v3 = liftIO $ getWindowMaximumSizeFFI v1 v2 v3
{-# INLINE getWindowMaximumSize #-}

getWindowMinimumSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowMinimumSize v1 v2 v3 = liftIO $ getWindowMinimumSizeFFI v1 v2 v3
{-# INLINE getWindowMinimumSize #-}

getWindowPixelFormat :: MonadIO m => Window -> m Word32
getWindowPixelFormat v1 = liftIO $ getWindowPixelFormatFFI v1
{-# INLINE getWindowPixelFormat #-}

getWindowPosition :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowPosition v1 v2 v3 = liftIO $ getWindowPositionFFI v1 v2 v3
{-# INLINE getWindowPosition #-}

getWindowSize :: MonadIO m => Window -> Ptr CInt -> Ptr CInt -> m ()
getWindowSize v1 v2 v3 = liftIO $ getWindowSizeFFI v1 v2 v3
{-# INLINE getWindowSize #-}

getWindowSurface :: MonadIO m => Window -> m (Ptr Surface)
getWindowSurface v1 = liftIO $ getWindowSurfaceFFI v1
{-# INLINE getWindowSurface #-}

getWindowTitle :: MonadIO m => Window -> m CString
getWindowTitle v1 = liftIO $ getWindowTitleFFI v1
{-# INLINE getWindowTitle #-}

hideWindow :: MonadIO m => Window -> m ()
hideWindow v1 = liftIO $ hideWindowFFI v1
{-# INLINE hideWindow #-}

isScreenSaverEnabled :: MonadIO m => m Bool
isScreenSaverEnabled = liftIO isScreenSaverEnabledFFI
{-# INLINE isScreenSaverEnabled #-}

maximizeWindow :: MonadIO m => Window -> m ()
maximizeWindow v1 = liftIO $ maximizeWindowFFI v1
{-# INLINE maximizeWindow #-}

minimizeWindow :: MonadIO m => Window -> m ()
minimizeWindow v1 = liftIO $ minimizeWindowFFI v1
{-# INLINE minimizeWindow #-}

raiseWindow :: MonadIO m => Window -> m ()
raiseWindow v1 = liftIO $ raiseWindowFFI v1
{-# INLINE raiseWindow #-}

restoreWindow :: MonadIO m => Window -> m ()
restoreWindow v1 = liftIO $ restoreWindowFFI v1
{-# INLINE restoreWindow #-}

setWindowBordered :: MonadIO m => Window -> Bool -> m ()
setWindowBordered v1 v2 = liftIO $ setWindowBorderedFFI v1 v2
{-# INLINE setWindowBordered #-}

setWindowBrightness :: MonadIO m => Window -> CFloat -> m CInt
setWindowBrightness v1 v2 = liftIO $ setWindowBrightnessFFI v1 v2
{-# INLINE setWindowBrightness #-}

setWindowData :: MonadIO m => Window -> CString -> Ptr () -> m (Ptr ())
setWindowData v1 v2 v3 = liftIO $ setWindowDataFFI v1 v2 v3
{-# INLINE setWindowData #-}

setWindowDisplayMode :: MonadIO m => Window -> Ptr DisplayMode -> m CInt
setWindowDisplayMode v1 v2 = liftIO $ setWindowDisplayModeFFI v1 v2
{-# INLINE setWindowDisplayMode #-}

setWindowFullscreen :: MonadIO m => Window -> Word32 -> m CInt
setWindowFullscreen v1 v2 = liftIO $ setWindowFullscreenFFI v1 v2
{-# INLINE setWindowFullscreen #-}

setWindowGammaRamp :: MonadIO m => Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> m CInt
setWindowGammaRamp v1 v2 v3 v4 = liftIO $ setWindowGammaRampFFI v1 v2 v3 v4
{-# INLINE setWindowGammaRamp #-}

setWindowGrab :: MonadIO m => Window -> Bool -> m ()
setWindowGrab v1 v2 = liftIO $ setWindowGrabFFI v1 v2
{-# INLINE setWindowGrab #-}

setWindowIcon :: MonadIO m => Window -> Ptr Surface -> m ()
setWindowIcon v1 v2 = liftIO $ setWindowIconFFI v1 v2
{-# INLINE setWindowIcon #-}

setWindowMaximumSize :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowMaximumSize v1 v2 v3 = liftIO $ setWindowMaximumSizeFFI v1 v2 v3
{-# INLINE setWindowMaximumSize #-}

setWindowMinimumSize :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowMinimumSize v1 v2 v3 = liftIO $ setWindowMinimumSizeFFI v1 v2 v3
{-# INLINE setWindowMinimumSize #-}

setWindowPosition :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowPosition v1 v2 v3 = liftIO $ setWindowPositionFFI v1 v2 v3
{-# INLINE setWindowPosition #-}

setWindowSize :: MonadIO m => Window -> CInt -> CInt -> m ()
setWindowSize v1 v2 v3 = liftIO $ setWindowSizeFFI v1 v2 v3
{-# INLINE setWindowSize #-}

setWindowTitle :: MonadIO m => Window -> CString -> m ()
setWindowTitle v1 v2 = liftIO $ setWindowTitleFFI v1 v2
{-# INLINE setWindowTitle #-}

showMessageBox :: MonadIO m => Ptr MessageBoxData -> Ptr CInt -> m CInt
showMessageBox v1 v2 = liftIO $ showMessageBoxFFI v1 v2
{-# INLINE showMessageBox #-}

showSimpleMessageBox :: MonadIO m => Word32 -> CString -> CString -> Window -> m CInt
showSimpleMessageBox v1 v2 v3 v4 = liftIO $ showSimpleMessageBoxFFI v1 v2 v3 v4
{-# INLINE showSimpleMessageBox #-}

showWindow :: MonadIO m => Window -> m ()
showWindow v1 = liftIO $ showWindowFFI v1
{-# INLINE showWindow #-}

updateWindowSurface :: MonadIO m => Window -> m CInt
updateWindowSurface v1 = liftIO $ updateWindowSurfaceFFI v1
{-# INLINE updateWindowSurface #-}

updateWindowSurfaceRects :: MonadIO m => Window -> Ptr Rect -> CInt -> m CInt
updateWindowSurfaceRects v1 v2 v3 = liftIO $ updateWindowSurfaceRectsFFI v1 v2 v3
{-# INLINE updateWindowSurfaceRects #-}

videoInit :: MonadIO m => CString -> m CInt
videoInit v1 = liftIO $ videoInitFFI v1
{-# INLINE videoInit #-}

videoQuit :: MonadIO m => m ()
videoQuit = liftIO videoQuitFFI
{-# INLINE videoQuit #-}

createRenderer :: MonadIO m => Window -> CInt -> Word32 -> m Renderer
createRenderer v1 v2 v3 = liftIO $ createRendererFFI v1 v2 v3
{-# INLINE createRenderer #-}

createSoftwareRenderer :: MonadIO m => Ptr Surface -> m Renderer
createSoftwareRenderer v1 = liftIO $ createSoftwareRendererFFI v1
{-# INLINE createSoftwareRenderer #-}

createTexture :: MonadIO m => Renderer -> Word32 -> CInt -> CInt -> CInt -> m Texture
createTexture v1 v2 v3 v4 v5 = liftIO $ createTextureFFI v1 v2 v3 v4 v5
{-# INLINE createTexture #-}

createTextureFromSurface :: MonadIO m => Renderer -> Ptr Surface -> m Texture
createTextureFromSurface v1 v2 = liftIO $ createTextureFromSurfaceFFI v1 v2
{-# INLINE createTextureFromSurface #-}

destroyRenderer :: MonadIO m => Renderer -> m ()
destroyRenderer v1 = liftIO $ destroyRendererFFI v1
{-# INLINE destroyRenderer #-}

destroyTexture :: MonadIO m => Texture -> m ()
destroyTexture v1 = liftIO $ destroyTextureFFI v1
{-# INLINE destroyTexture #-}

getNumRenderDrivers :: MonadIO m => m CInt
getNumRenderDrivers = liftIO getNumRenderDriversFFI
{-# INLINE getNumRenderDrivers #-}

getRenderDrawBlendMode :: MonadIO m => Renderer -> Ptr BlendMode -> m Int
getRenderDrawBlendMode v1 v2 = liftIO $ getRenderDrawBlendModeFFI v1 v2
{-# INLINE getRenderDrawBlendMode #-}

getRenderDrawColor :: MonadIO m => Renderer -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m CInt
getRenderDrawColor v1 v2 v3 v4 v5 = liftIO $ getRenderDrawColorFFI v1 v2 v3 v4 v5
{-# INLINE getRenderDrawColor #-}

getRenderDriverInfo :: MonadIO m => CInt -> Ptr RendererInfo -> m CInt
getRenderDriverInfo v1 v2 = liftIO $ getRenderDriverInfoFFI v1 v2
{-# INLINE getRenderDriverInfo #-}

getRenderTarget :: MonadIO m => Renderer -> m Texture
getRenderTarget v1 = liftIO $ getRenderTargetFFI v1
{-# INLINE getRenderTarget #-}

getRenderer :: MonadIO m => Window -> m Renderer
getRenderer v1 = liftIO $ getRendererFFI v1
{-# INLINE getRenderer #-}

getRendererInfo :: MonadIO m => Renderer -> Ptr RendererInfo -> m CInt
getRendererInfo v1 v2 = liftIO $ getRendererInfoFFI v1 v2
{-# INLINE getRendererInfo #-}

getRendererOutputSize :: MonadIO m => Renderer -> Ptr CInt -> Ptr CInt -> m CInt
getRendererOutputSize v1 v2 v3 = liftIO $ getRendererOutputSizeFFI v1 v2 v3
{-# INLINE getRendererOutputSize #-}

getTextureAlphaMod :: MonadIO m => Texture -> Ptr Word8 -> m CInt
getTextureAlphaMod v1 v2 = liftIO $ getTextureAlphaModFFI v1 v2
{-# INLINE getTextureAlphaMod #-}

getTextureBlendMode :: MonadIO m => Texture -> Ptr BlendMode -> m CInt
getTextureBlendMode v1 v2 = liftIO $ getTextureBlendModeFFI v1 v2
{-# INLINE getTextureBlendMode #-}

getTextureColorMod :: MonadIO m => Texture -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m CInt
getTextureColorMod v1 v2 v3 v4 = liftIO $ getTextureColorModFFI v1 v2 v3 v4
{-# INLINE getTextureColorMod #-}

lockTexture :: MonadIO m => Texture -> Ptr Rect -> Ptr (Ptr ()) -> Ptr CInt -> m CInt
lockTexture v1 v2 v3 v4 = liftIO $ lockTextureFFI v1 v2 v3 v4
{-# INLINE lockTexture #-}

queryTexture :: MonadIO m => Texture -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> Ptr CInt -> m CInt
queryTexture v1 v2 v3 v4 v5 = liftIO $ queryTextureFFI v1 v2 v3 v4 v5
{-# INLINE queryTexture #-}

renderClear :: MonadIO m => Renderer -> m CInt
renderClear v1 = liftIO $ renderClearFFI v1
{-# INLINE renderClear #-}

renderCopy :: MonadIO m => Renderer -> Texture -> Ptr Rect -> Ptr Rect -> m CInt
renderCopy v1 v2 v3 v4 = liftIO $ renderCopyFFI v1 v2 v3 v4
{-# INLINE renderCopy #-}

renderCopyEx :: MonadIO m => Renderer -> Texture -> Ptr Rect -> Ptr Rect -> CDouble -> Ptr Point -> RendererFlip -> m CInt
renderCopyEx v1 v2 v3 v4 v5 v6 v7 = liftIO $ renderCopyExFFI v1 v2 v3 v4 v5 v6 v7
{-# INLINE renderCopyEx #-}

renderDrawLine :: MonadIO m => Renderer -> CInt -> CInt -> CInt -> CInt -> m CInt
renderDrawLine v1 v2 v3 v4 v5 = liftIO $ renderDrawLineFFI v1 v2 v3 v4 v5
{-# INLINE renderDrawLine #-}

renderDrawLines :: MonadIO m => Renderer -> Ptr Point -> CInt -> m CInt
renderDrawLines v1 v2 v3 = liftIO $ renderDrawLinesFFI v1 v2 v3
{-# INLINE renderDrawLines #-}

renderDrawPoint :: MonadIO m => Renderer -> CInt -> CInt -> m CInt
renderDrawPoint v1 v2 v3 = liftIO $ renderDrawPointFFI v1 v2 v3
{-# INLINE renderDrawPoint #-}

renderDrawPoints :: MonadIO m => Renderer -> Ptr Point -> CInt -> m CInt
renderDrawPoints v1 v2 v3 = liftIO $ renderDrawPointsFFI v1 v2 v3
{-# INLINE renderDrawPoints #-}

renderDrawRect :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderDrawRect v1 v2 = liftIO $ renderDrawRectFFI v1 v2
{-# INLINE renderDrawRect #-}

renderDrawRects :: MonadIO m => Renderer -> Ptr Rect -> CInt -> m CInt
renderDrawRects v1 v2 v3 = liftIO $ renderDrawRectsFFI v1 v2 v3
{-# INLINE renderDrawRects #-}

renderFillRect :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderFillRect v1 v2 = liftIO $ renderFillRectFFI v1 v2
{-# INLINE renderFillRect #-}

renderFillRects :: MonadIO m => Renderer -> Ptr Rect -> CInt -> m CInt
renderFillRects v1 v2 v3 = liftIO $ renderFillRectsFFI v1 v2 v3
{-# INLINE renderFillRects #-}

renderGetClipRect :: MonadIO m => Renderer -> Ptr Rect -> m ()
renderGetClipRect v1 v2 = liftIO $ renderGetClipRectFFI v1 v2
{-# INLINE renderGetClipRect #-}

renderGetLogicalSize :: MonadIO m => Renderer -> Ptr CInt -> Ptr CInt -> m ()
renderGetLogicalSize v1 v2 v3 = liftIO $ renderGetLogicalSizeFFI v1 v2 v3
{-# INLINE renderGetLogicalSize #-}

renderGetScale :: MonadIO m => Renderer -> Ptr CFloat -> Ptr CFloat -> m ()
renderGetScale v1 v2 v3 = liftIO $ renderGetScaleFFI v1 v2 v3
{-# INLINE renderGetScale #-}

renderGetViewport :: MonadIO m => Renderer -> Ptr Rect -> m ()
renderGetViewport v1 v2 = liftIO $ renderGetViewportFFI v1 v2
{-# INLINE renderGetViewport #-}

renderPresent :: MonadIO m => Renderer -> m ()
renderPresent v1 = liftIO $ renderPresentFFI v1
{-# INLINE renderPresent #-}

renderReadPixels :: MonadIO m => Renderer -> Ptr Rect -> Word32 -> Ptr () -> CInt -> m CInt
renderReadPixels v1 v2 v3 v4 v5 = liftIO $ renderReadPixelsFFI v1 v2 v3 v4 v5
{-# INLINE renderReadPixels #-}

renderSetClipRect :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderSetClipRect v1 v2 = liftIO $ renderSetClipRectFFI v1 v2
{-# INLINE renderSetClipRect #-}

renderSetLogicalSize :: MonadIO m => Renderer -> CInt -> CInt -> m CInt
renderSetLogicalSize v1 v2 v3 = liftIO $ renderSetLogicalSizeFFI v1 v2 v3
{-# INLINE renderSetLogicalSize #-}

renderSetScale :: MonadIO m => Renderer -> CFloat -> CFloat -> m CInt
renderSetScale v1 v2 v3 = liftIO $ renderSetScaleFFI v1 v2 v3
{-# INLINE renderSetScale #-}

renderSetViewport :: MonadIO m => Renderer -> Ptr Rect -> m CInt
renderSetViewport v1 v2 = liftIO $ renderSetViewportFFI v1 v2
{-# INLINE renderSetViewport #-}

renderTargetSupported :: MonadIO m => Renderer -> m Bool
renderTargetSupported v1 = liftIO $ renderTargetSupportedFFI v1
{-# INLINE renderTargetSupported #-}

setRenderDrawBlendMode :: MonadIO m => Renderer -> BlendMode -> m CInt
setRenderDrawBlendMode v1 v2 = liftIO $ setRenderDrawBlendModeFFI v1 v2
{-# INLINE setRenderDrawBlendMode #-}

setRenderDrawColor :: MonadIO m => Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> m CInt
setRenderDrawColor v1 v2 v3 v4 v5 = liftIO $ setRenderDrawColorFFI v1 v2 v3 v4 v5
{-# INLINE setRenderDrawColor #-}

setRenderTarget :: MonadIO m => Renderer -> Texture -> m CInt
setRenderTarget v1 v2 = liftIO $ setRenderTargetFFI v1 v2
{-# INLINE setRenderTarget #-}

setTextureAlphaMod :: MonadIO m => Texture -> Word8 -> m CInt
setTextureAlphaMod v1 v2 = liftIO $ setTextureAlphaModFFI v1 v2
{-# INLINE setTextureAlphaMod #-}

setTextureBlendMode :: MonadIO m => Texture -> BlendMode -> m CInt
setTextureBlendMode v1 v2 = liftIO $ setTextureBlendModeFFI v1 v2
{-# INLINE setTextureBlendMode #-}

setTextureColorMod :: MonadIO m => Texture -> Word8 -> Word8 -> Word8 -> m CInt
setTextureColorMod v1 v2 v3 v4 = liftIO $ setTextureColorModFFI v1 v2 v3 v4
{-# INLINE setTextureColorMod #-}

unlockTexture :: MonadIO m => Texture -> m ()
unlockTexture v1 = liftIO $ unlockTextureFFI v1
{-# INLINE unlockTexture #-}

updateTexture :: MonadIO m => Texture -> Ptr Rect -> Ptr () -> CInt -> m CInt
updateTexture v1 v2 v3 v4 = liftIO $ updateTextureFFI v1 v2 v3 v4
{-# INLINE updateTexture #-}

updateYUVTexture :: MonadIO m => Texture -> Ptr Rect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> m CInt
updateYUVTexture v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ updateYUVTextureFFI v1 v2 v3 v4 v5 v6 v7 v8
{-# INLINE updateYUVTexture #-}

allocFormat :: MonadIO m => Word32 -> m (Ptr PixelFormat)
allocFormat v1 = liftIO $ allocFormatFFI v1
{-# INLINE allocFormat #-}

allocPalette :: MonadIO m => CInt -> m (Ptr Palette)
allocPalette v1 = liftIO $ allocPaletteFFI v1
{-# INLINE allocPalette #-}

calculateGammaRamp :: MonadIO m => CFloat -> Ptr Word16 -> m ()
calculateGammaRamp v1 v2 = liftIO $ calculateGammaRampFFI v1 v2
{-# INLINE calculateGammaRamp #-}

freeFormat :: MonadIO m => Ptr PixelFormat -> m ()
freeFormat v1 = liftIO $ freeFormatFFI v1
{-# INLINE freeFormat #-}

freePalette :: MonadIO m => Ptr Palette -> m ()
freePalette v1 = liftIO $ freePaletteFFI v1
{-# INLINE freePalette #-}

getPixelFormatName :: MonadIO m => Word32 -> m CString
getPixelFormatName v1 = liftIO $ getPixelFormatNameFFI v1
{-# INLINE getPixelFormatName #-}

getRGB :: MonadIO m => Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m ()
getRGB v1 v2 v3 v4 v5 = liftIO $ getRGBFFI v1 v2 v3 v4 v5
{-# INLINE getRGB #-}

getRGBA :: MonadIO m => Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m ()
getRGBA v1 v2 v3 v4 v5 v6 = liftIO $ getRGBAFFI v1 v2 v3 v4 v5 v6
{-# INLINE getRGBA #-}

mapRGB :: MonadIO m => Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> m Word32
mapRGB v1 v2 v3 v4 = liftIO $ mapRGBFFI v1 v2 v3 v4
{-# INLINE mapRGB #-}

mapRGBA :: MonadIO m => Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> m Word32
mapRGBA v1 v2 v3 v4 v5 = liftIO $ mapRGBAFFI v1 v2 v3 v4 v5
{-# INLINE mapRGBA #-}

masksToPixelFormatEnum :: MonadIO m => CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m Word32
masksToPixelFormatEnum v1 v2 v3 v4 v5 = liftIO $ masksToPixelFormatEnumFFI v1 v2 v3 v4 v5
{-# INLINE masksToPixelFormatEnum #-}

pixelFormatEnumToMasks :: MonadIO m => Word32 -> Ptr CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> m Bool
pixelFormatEnumToMasks v1 v2 v3 v4 v5 v6 = liftIO $ pixelFormatEnumToMasksFFI v1 v2 v3 v4 v5 v6
{-# INLINE pixelFormatEnumToMasks #-}

setPaletteColors :: MonadIO m => Ptr Palette -> Ptr Color -> CInt -> CInt -> m CInt
setPaletteColors v1 v2 v3 v4 = liftIO $ setPaletteColorsFFI v1 v2 v3 v4
{-# INLINE setPaletteColors #-}

setPixelFormatPalette :: MonadIO m => Ptr PixelFormat -> Ptr Palette -> m CInt
setPixelFormatPalette v1 v2 = liftIO $ setPixelFormatPaletteFFI v1 v2
{-# INLINE setPixelFormatPalette #-}

enclosePoints :: MonadIO m => Ptr Point -> CInt -> Ptr Rect -> Ptr Rect -> m Bool
enclosePoints v1 v2 v3 v4 = liftIO $ enclosePointsFFI v1 v2 v3 v4
{-# INLINE enclosePoints #-}

hasIntersection :: MonadIO m => Ptr Rect -> Ptr Rect -> m Bool
hasIntersection v1 v2 = liftIO $ hasIntersectionFFI v1 v2
{-# INLINE hasIntersection #-}

intersectRect :: MonadIO m => Ptr Rect -> Ptr Rect -> Ptr Rect -> m Bool
intersectRect v1 v2 v3 = liftIO $ intersectRectFFI v1 v2 v3
{-# INLINE intersectRect #-}

intersectRectAndLine :: MonadIO m => Ptr Rect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> m Bool
intersectRectAndLine v1 v2 v3 v4 v5 = liftIO $ intersectRectAndLineFFI v1 v2 v3 v4 v5
{-# INLINE intersectRectAndLine #-}

unionRect :: MonadIO m => Ptr Rect -> Ptr Rect -> Ptr Rect -> m ()
unionRect v1 v2 v3 = liftIO $ unionRectFFI v1 v2 v3
{-# INLINE unionRect #-}

blitScaled :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
blitScaled v1 v2 v3 v4 = liftIO $ blitScaledFFI v1 v2 v3 v4
{-# INLINE blitScaled #-}

blitSurface :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
blitSurface v1 v2 v3 v4 = liftIO $ blitSurfaceFFI v1 v2 v3 v4
{-# INLINE blitSurface #-}

convertPixels :: MonadIO m => CInt -> CInt -> Word32 -> Ptr () -> CInt -> Word32 -> Ptr () -> CInt -> m CInt
convertPixels v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ convertPixelsFFI v1 v2 v3 v4 v5 v6 v7 v8
{-# INLINE convertPixels #-}

convertSurface :: MonadIO m => Ptr Surface -> Ptr PixelFormat -> Word32 -> m (Ptr Surface)
convertSurface v1 v2 v3 = liftIO $ convertSurfaceFFI v1 v2 v3
{-# INLINE convertSurface #-}

convertSurfaceFormat :: MonadIO m => Ptr Surface -> Word32 -> Word32 -> m (Ptr Surface)
convertSurfaceFormat v1 v2 v3 = liftIO $ convertSurfaceFormatFFI v1 v2 v3
{-# INLINE convertSurfaceFormat #-}

createRGBSurface :: MonadIO m => Word32 -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m (Ptr Surface)
createRGBSurface v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ createRGBSurfaceFFI v1 v2 v3 v4 v5 v6 v7 v8
{-# INLINE createRGBSurface #-}

createRGBSurfaceFrom :: MonadIO m => Ptr () -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> m (Ptr Surface)
createRGBSurfaceFrom v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ createRGBSurfaceFromFFI v1 v2 v3 v4 v5 v6 v7 v8 v9
{-# INLINE createRGBSurfaceFrom #-}

fillRect :: MonadIO m => Ptr Surface -> Ptr Rect -> Word32 -> m CInt
fillRect v1 v2 v3 = liftIO $ fillRectFFI v1 v2 v3
{-# INLINE fillRect #-}

fillRects :: MonadIO m => Ptr Surface -> Ptr Rect -> CInt -> Word32 -> m CInt
fillRects v1 v2 v3 v4 = liftIO $ fillRectsFFI v1 v2 v3 v4
{-# INLINE fillRects #-}

freeSurface :: MonadIO m => Ptr Surface -> m ()
freeSurface v1 = liftIO $ freeSurfaceFFI v1
{-# INLINE freeSurface #-}

getClipRect :: MonadIO m => Ptr Surface -> Ptr Rect -> m ()
getClipRect v1 v2 = liftIO $ getClipRectFFI v1 v2
{-# INLINE getClipRect #-}

getColorKey :: MonadIO m => Ptr Surface -> Ptr Word32 -> m CInt
getColorKey v1 v2 = liftIO $ getColorKeyFFI v1 v2
{-# INLINE getColorKey #-}

getSurfaceAlphaMod :: MonadIO m => Ptr Surface -> Ptr Word8 -> m CInt
getSurfaceAlphaMod v1 v2 = liftIO $ getSurfaceAlphaModFFI v1 v2
{-# INLINE getSurfaceAlphaMod #-}

getSurfaceBlendMode :: MonadIO m => Ptr Surface -> Ptr BlendMode -> m CInt
getSurfaceBlendMode v1 v2 = liftIO $ getSurfaceBlendModeFFI v1 v2
{-# INLINE getSurfaceBlendMode #-}

getSurfaceColorMod :: MonadIO m => Ptr Surface -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> m CInt
getSurfaceColorMod v1 v2 v3 v4 = liftIO $ getSurfaceColorModFFI v1 v2 v3 v4
{-# INLINE getSurfaceColorMod #-}

loadBMP :: MonadIO m => CString -> m (Ptr Surface)
loadBMP file = liftIO $ do
  rw <- withCString "rb" $ rwFromFile file
  loadBMP_RW rw 1
{-# INLINE loadBMP #-}

loadBMP_RW :: MonadIO m => Ptr RWops -> CInt -> m (Ptr Surface)
loadBMP_RW v1 v2 = liftIO $ loadBMP_RWFFI v1 v2
{-# INLINE loadBMP_RW #-}

lockSurface :: MonadIO m => Ptr Surface -> m CInt
lockSurface v1 = liftIO $ lockSurfaceFFI v1
{-# INLINE lockSurface #-}

lowerBlit :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
lowerBlit v1 v2 v3 v4 = liftIO $ lowerBlitFFI v1 v2 v3 v4
{-# INLINE lowerBlit #-}

lowerBlitScaled :: MonadIO m => Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> m CInt
lowerBlitScaled v1 v2 v3 v4 = liftIO $ lowerBlitScaledFFI v1 v2 v3 v4
{-# INLINE lowerBlitScaled #-}

saveBMP :: MonadIO m => Ptr Surface -> CString -> m CInt
saveBMP surface file = liftIO $ do
  rw <- withCString "wb" $ rwFromFile file
  saveBMP_RW surface rw 1
{-# INLINE saveBMP #-}

saveBMP_RW :: MonadIO m => Ptr Surface -> Ptr RWops -> CInt -> m CInt
saveBMP_RW v1 v2 v3 = liftIO $ saveBMP_RWFFI v1 v2 v3
{-# INLINE saveBMP_RW #-}

setClipRect :: MonadIO m => Ptr Surface -> Ptr Rect -> m Bool
setClipRect v1 v2 = liftIO $ setClipRectFFI v1 v2
{-# INLINE setClipRect #-}

setColorKey :: MonadIO m => Ptr Surface -> CInt -> Word32 -> m CInt
setColorKey v1 v2 v3 = liftIO $ setColorKeyFFI v1 v2 v3
{-# INLINE setColorKey #-}

setSurfaceAlphaMod :: MonadIO m => Ptr Surface -> Word8 -> m CInt
setSurfaceAlphaMod v1 v2 = liftIO $ setSurfaceAlphaModFFI v1 v2
{-# INLINE setSurfaceAlphaMod #-}

setSurfaceBlendMode :: MonadIO m => Ptr Surface -> BlendMode -> m CInt
setSurfaceBlendMode v1 v2 = liftIO $ setSurfaceBlendModeFFI v1 v2
{-# INLINE setSurfaceBlendMode #-}

setSurfaceColorMod :: MonadIO m => Ptr Surface -> Word8 -> Word8 -> Word8 -> m CInt
setSurfaceColorMod v1 v2 v3 v4 = liftIO $ setSurfaceColorModFFI v1 v2 v3 v4
{-# INLINE setSurfaceColorMod #-}

setSurfacePalette :: MonadIO m => Ptr Surface -> Ptr Palette -> m CInt
setSurfacePalette v1 v2 = liftIO $ setSurfacePaletteFFI v1 v2
{-# INLINE setSurfacePalette #-}

setSurfaceRLE :: MonadIO m => Ptr Surface -> CInt -> m CInt
setSurfaceRLE v1 v2 = liftIO $ setSurfaceRLEFFI v1 v2
{-# INLINE setSurfaceRLE #-}

unlockSurface :: MonadIO m => Ptr Surface -> m ()
unlockSurface v1 = liftIO $ unlockSurfaceFFI v1
{-# INLINE unlockSurface #-}

getWindowWMInfo :: MonadIO m => Window -> SysWMinfo -> m Bool
getWindowWMInfo v1 v2 = liftIO $ getWindowWMInfoFFI v1 v2
{-# INLINE getWindowWMInfo #-}

getClipboardText :: MonadIO m => m CString
getClipboardText = liftIO getClipboardTextFFI
{-# INLINE getClipboardText #-}

hasClipboardText :: MonadIO m => m Bool
hasClipboardText = liftIO hasClipboardTextFFI
{-# INLINE hasClipboardText #-}

setClipboardText :: MonadIO m => CString -> m CInt
setClipboardText v1 = liftIO $ setClipboardTextFFI v1
{-# INLINE setClipboardText #-}
