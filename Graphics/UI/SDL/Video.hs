module Graphics.UI.SDL.Video (
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

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Filesystem
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_CreateWindow" createWindow :: CString -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO Window
foreign import ccall "SDL.h SDL_CreateWindowAndRenderer" createWindowAndRenderer :: CInt -> CInt -> Word32 -> Ptr Window -> Ptr Renderer -> IO CInt
foreign import ccall "SDL.h SDL_CreateWindowFrom" createWindowFrom :: Ptr () -> IO Window
foreign import ccall "SDL.h SDL_DestroyWindow" destroyWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_DisableScreenSaver" disableScreenSaver :: IO ()
foreign import ccall "SDL.h SDL_EnableScreenSaver" enableScreenSaver :: IO ()
foreign import ccall "SDL.h SDL_GL_BindTexture" glBindTexture :: Texture -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "SDL.h SDL_GL_CreateContext" glCreateContext :: Window -> IO GLContext
foreign import ccall "SDL.h SDL_GL_DeleteContext" glDeleteContext :: GLContext -> IO ()
foreign import ccall "SDL.h SDL_GL_ExtensionSupported" glExtensionSupported :: CString -> IO Bool
foreign import ccall "SDL.h SDL_GL_GetAttribute" glGetAttribute :: GLattr -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_GetCurrentContext" glGetCurrentContext :: IO GLContext
foreign import ccall "SDL.h SDL_GL_GetCurrentWindow" glGetCurrentWindow :: IO Window
foreign import ccall "SDL.h SDL_GL_GetDrawableSize" glGetDrawableSize :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GL_GetProcAddress" glGetProcAddress :: CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GL_GetSwapInterval" glGetSwapInterval :: IO CInt
foreign import ccall "SDL.h SDL_GL_LoadLibrary" glLoadLibrary :: CString -> IO CInt
foreign import ccall "SDL.h SDL_GL_MakeCurrent" glMakeCurrent :: Window -> GLContext -> IO CInt
foreign import ccall "SDL.h SDL_GL_ResetAttributes" glResetAttributes :: IO ()
foreign import ccall "SDL.h SDL_GL_SetAttribute" glSetAttribute :: GLattr -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_SetSwapInterval" glSetSwapInterval :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GL_SwapWindow" glSwapWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_GL_UnbindTexture" glUnbindTexture :: Texture -> IO CInt
foreign import ccall "SDL.h SDL_GL_UnloadLibrary" glUnloadLibrary :: IO ()
foreign import ccall "SDL.h SDL_GetClosestDisplayMode" getClosestDisplayMode :: CInt -> Ptr DisplayMode -> Ptr DisplayMode -> IO (Ptr DisplayMode)
foreign import ccall "SDL.h SDL_GetCurrentDisplayMode" getCurrentDisplayMode :: CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetCurrentVideoDriver" getCurrentVideoDriver :: IO CString
foreign import ccall "SDL.h SDL_GetDesktopDisplayMode" getDesktopDisplayMode :: CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayBounds" getDisplayBounds :: CInt -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayMode" getDisplayMode :: CInt -> CInt -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetDisplayName" getDisplayName :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetNumDisplayModes" getNumDisplayModes :: CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetNumVideoDisplays" getNumVideoDisplays :: IO CInt
foreign import ccall "SDL.h SDL_GetNumVideoDrivers" getNumVideoDrivers :: IO CInt
foreign import ccall "SDL.h SDL_GetVideoDriver" getVideoDriver :: CInt -> IO CString
foreign import ccall "SDL.h SDL_GetWindowBrightness" getWindowBrightness :: Window -> IO CFloat
foreign import ccall "SDL.h SDL_GetWindowData" getWindowData :: Window -> CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_GetWindowDisplayIndex" getWindowDisplayIndex :: Window -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowDisplayMode" getWindowDisplayMode :: Window -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowFlags" getWindowFlags :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowFromID" getWindowFromID :: Word32 -> IO Window
foreign import ccall "SDL.h SDL_GetWindowGammaRamp" getWindowGammaRamp :: Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
foreign import ccall "SDL.h SDL_GetWindowGrab" getWindowGrab :: Window -> IO Bool
foreign import ccall "SDL.h SDL_GetWindowID" getWindowID :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowMaximumSize" getWindowMaximumSize :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowMinimumSize" getWindowMinimumSize :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowPixelFormat" getWindowPixelFormat :: Window -> IO Word32
foreign import ccall "SDL.h SDL_GetWindowPosition" getWindowPosition :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowSize" getWindowSize :: Window -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_GetWindowSurface" getWindowSurface :: Window -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_GetWindowTitle" getWindowTitle :: Window -> IO CString
foreign import ccall "SDL.h SDL_HideWindow" hideWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_IsScreenSaverEnabled" isScreenSaverEnabled :: IO Bool
foreign import ccall "SDL.h SDL_MaximizeWindow" maximizeWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_MinimizeWindow" minimizeWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_RaiseWindow" raiseWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_RestoreWindow" restoreWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_SetWindowBordered" setWindowBordered :: Window -> Bool -> IO ()
foreign import ccall "SDL.h SDL_SetWindowBrightness" setWindowBrightness :: Window -> CFloat -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowData" setWindowData :: Window -> CString -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_SetWindowDisplayMode" setWindowDisplayMode :: Window -> Ptr DisplayMode -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowFullscreen" setWindowFullscreen :: Window -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowGammaRamp" setWindowGammaRamp :: Window -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
foreign import ccall "SDL.h SDL_SetWindowGrab" setWindowGrab :: Window -> Bool -> IO ()
foreign import ccall "SDL.h SDL_SetWindowIcon" setWindowIcon :: Window -> Ptr Surface -> IO ()
foreign import ccall "SDL.h SDL_SetWindowMaximumSize" setWindowMaximumSize :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowMinimumSize" setWindowMinimumSize :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowPosition" setWindowPosition :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowSize" setWindowSize :: Window -> CInt -> CInt -> IO ()
foreign import ccall "SDL.h SDL_SetWindowTitle" setWindowTitle :: Window -> CString -> IO ()
foreign import ccall "SDL.h SDL_ShowMessageBox" showMessageBox :: Ptr MessageBoxData -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_ShowSimpleMessageBox" showSimpleMessageBox :: Word32 -> CString -> CString -> Window -> IO CInt
foreign import ccall "SDL.h SDL_ShowWindow" showWindow :: Window -> IO ()
foreign import ccall "SDL.h SDL_UpdateWindowSurface" updateWindowSurface :: Window -> IO CInt
foreign import ccall "SDL.h SDL_UpdateWindowSurfaceRects" updateWindowSurfaceRects :: Window -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_VideoInit" videoInit :: CString -> IO CInt
foreign import ccall "SDL.h SDL_VideoQuit" videoQuit :: IO ()

foreign import ccall "SDL.h SDL_CreateRenderer" createRenderer :: Window -> CInt -> Word32 -> IO Renderer
foreign import ccall "SDL.h SDL_CreateSoftwareRenderer" createSoftwareRenderer :: Ptr Surface -> IO Renderer
foreign import ccall "SDL.h SDL_CreateTexture" createTexture :: Renderer -> Word32 -> CInt -> CInt -> CInt -> IO Texture 
foreign import ccall "SDL.h SDL_CreateTextureFromSurface" createTextureFromSurface :: Renderer -> Ptr Surface -> IO Texture
foreign import ccall "SDL.h SDL_DestroyRenderer" destroyRenderer :: Renderer -> IO ()
foreign import ccall "SDL.h SDL_DestroyTexture" destroyTexture :: Texture -> IO ()
foreign import ccall "SDL.h SDL_GetNumRenderDrivers" getNumRenderDrivers :: IO CInt
foreign import ccall "SDL.h SDL_GetRenderDrawBlendMode" getRenderDrawBlendMode :: Renderer -> Ptr BlendMode -> IO Int
foreign import ccall "SDL.h SDL_GetRenderDrawColor" getRenderDrawColor :: Renderer -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetRenderDriverInfo" getRenderDriverInfo :: CInt -> Ptr RendererInfo -> IO CInt
foreign import ccall "SDL.h SDL_GetRenderTarget" getRenderTarget :: Renderer -> IO Texture
foreign import ccall "SDL.h SDL_GetRenderer" getRenderer :: Window -> IO Renderer
foreign import ccall "SDL.h SDL_GetRendererInfo" getRendererInfo :: Renderer -> Ptr RendererInfo -> IO CInt
foreign import ccall "SDL.h SDL_GetRendererOutputSize" getRendererOutputSize :: Renderer -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureAlphaMod" getTextureAlphaMod :: Texture -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureBlendMode" getTextureBlendMode :: Texture -> Ptr BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_GetTextureColorMod" getTextureColorMod :: Texture -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_LockTexture" lockTexture :: Texture -> Ptr Rect -> Ptr (Ptr ()) -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_QueryTexture" queryTexture :: Texture -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderClear" renderClear :: Renderer -> IO CInt
foreign import ccall "SDL.h SDL_RenderCopy" renderCopy :: Renderer -> Texture -> Ptr Rect -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderCopyEx" renderCopyEx :: Renderer -> Texture -> Ptr Rect -> Ptr Rect -> CDouble -> Ptr Point -> RendererFlip -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawLine" renderDrawLine :: Renderer -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawLines" renderDrawLines :: Renderer -> Ptr Point -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawPoint" renderDrawPoint :: Renderer -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawPoints" renderDrawPoints :: Renderer -> Ptr Point -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawRect" renderDrawRect :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderDrawRects" renderDrawRects :: Renderer -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderFillRect" renderFillRect :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderFillRects" renderFillRects :: Renderer -> Ptr Rect -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderGetClipRect" renderGetClipRect :: Renderer -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_RenderGetLogicalSize" renderGetLogicalSize :: Renderer -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "SDL.h SDL_RenderGetScale" renderGetScale :: Renderer -> Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall "SDL.h SDL_RenderGetViewport" renderGetViewport :: Renderer -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_RenderPresent" renderPresent :: Renderer -> IO ()
foreign import ccall "SDL.h SDL_RenderReadPixels" renderReadPixels :: Renderer -> Ptr Rect -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetClipRect" renderSetClipRect :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetLogicalSize" renderSetLogicalSize :: Renderer -> CInt -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetScale" renderSetScale :: Renderer -> CFloat -> CFloat -> IO CInt
foreign import ccall "SDL.h SDL_RenderSetViewport" renderSetViewport :: Renderer -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_RenderTargetSupported" renderTargetSupported :: Renderer -> IO Bool
foreign import ccall "SDL.h SDL_SetRenderDrawBlendMode" setRenderDrawBlendMode :: Renderer -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetRenderDrawColor" setRenderDrawColor :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetRenderTarget" setRenderTarget :: Renderer -> Texture -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureAlphaMod" setTextureAlphaMod :: Texture -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureBlendMode" setTextureBlendMode :: Texture -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetTextureColorMod" setTextureColorMod :: Texture -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_UnlockTexture" unlockTexture :: Texture -> IO ()
foreign import ccall "SDL.h SDL_UpdateTexture" updateTexture :: Texture -> Ptr Rect -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_UpdateYUVTexture" updateYUVTexture :: Texture -> Ptr Rect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "SDL.h SDL_AllocFormat" allocFormat :: Word32 -> IO (Ptr PixelFormat)
foreign import ccall "SDL.h SDL_AllocPalette" allocPalette :: CInt -> IO (Ptr Palette)
foreign import ccall "SDL.h SDL_CalculateGammaRamp" calculateGammaRamp :: CFloat -> Ptr Word16 -> IO ()
foreign import ccall "SDL.h SDL_FreeFormat" freeFormat :: Ptr PixelFormat -> IO ()
foreign import ccall "SDL.h SDL_FreePalette" freePalette :: Ptr Palette -> IO ()
foreign import ccall "SDL.h SDL_GetPixelFormatName" getPixelFormatName :: Word32 -> IO CString
foreign import ccall "SDL.h SDL_GetRGB" getRGB :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_GetRGBA" getRGBA :: Word32 -> Ptr PixelFormat -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "SDL.h SDL_MapRGB" mapRGB :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MapRGBA" mapRGBA :: Ptr PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
foreign import ccall "SDL.h SDL_MasksToPixelFormatEnum" masksToPixelFormatEnum :: CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO Word32
foreign import ccall "SDL.h SDL_PixelFormatEnumToMasks" pixelFormatEnumToMasks :: Word32 -> Ptr CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Bool
foreign import ccall "SDL.h SDL_SetPaletteColors" setPaletteColors :: Ptr Palette -> Ptr Color -> CInt -> CInt -> IO CInt 
foreign import ccall "SDL.h SDL_SetPixelFormatPalette" setPixelFormatPalette :: Ptr PixelFormat -> Ptr Palette -> IO CInt

foreign import ccall "SDL.h SDL_EnclosePoints" enclosePoints :: Ptr Point -> CInt -> Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_HasIntersection" hasIntersection :: Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_IntersectRect" intersectRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_IntersectRectAndLine" intersectRectAndLine :: Ptr Rect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Bool
foreign import ccall "SDL.h SDL_UnionRect" unionRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO ()

foreign import ccall "SDL.h SDL_UpperBlitScaled" blitScaled :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_UpperBlit" blitSurface :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_ConvertPixels" convertPixels :: CInt -> CInt -> Word32 -> Ptr () -> CInt -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_ConvertSurface" convertSurface :: Ptr Surface -> Ptr PixelFormat -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_ConvertSurfaceFormat" convertSurfaceFormat :: Ptr Surface -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_CreateRGBSurface" createRGBSurface :: Word32 -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_CreateRGBSurfaceFrom" createRGBSurfaceFrom :: Ptr () -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_FillRect" fillRect :: Ptr Surface -> Ptr Rect -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_FillRects" fillRects :: Ptr Surface -> Ptr Rect -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_FreeSurface" freeSurface :: Ptr Surface -> IO ()
foreign import ccall "SDL.h SDL_GetClipRect" getClipRect :: Ptr Surface -> Ptr Rect -> IO ()
foreign import ccall "SDL.h SDL_GetColorKey" getColorKey :: Ptr Surface -> Ptr Word32 -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceAlphaMod" getSurfaceAlphaMod :: Ptr Surface -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceBlendMode" getSurfaceBlendMode :: Ptr Surface -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_GetSurfaceColorMod" getSurfaceColorMod :: Ptr Surface -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt
foreign import ccall "SDL.h SDL_LoadBMP_RW" loadBMP_RW :: Ptr RWops -> CInt -> IO (Ptr Surface)
foreign import ccall "SDL.h SDL_LockSurface" lockSurface :: Ptr Surface -> IO CInt
foreign import ccall "SDL.h SDL_LowerBlit" lowerBlit :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_LowerBlitScaled" lowerBlitScaled :: Ptr Surface -> Ptr Rect -> Ptr Surface -> Ptr Rect -> IO CInt
foreign import ccall "SDL.h SDL_SaveBMP_RW" saveBMP_RW :: Ptr Surface -> Ptr RWops -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_SetClipRect" setClipRect :: Ptr Surface -> Ptr Rect -> IO Bool
foreign import ccall "SDL.h SDL_SetColorKey" setColorKey :: Ptr Surface -> CInt -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceAlphaMod" setSurfaceAlphaMod :: Ptr Surface -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceBlendMode" setSurfaceBlendMode :: Ptr Surface -> BlendMode -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceColorMod" setSurfaceColorMod :: Ptr Surface -> Word8 -> Word8 -> Word8 -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfacePalette" setSurfacePalette :: Ptr Surface -> Ptr Palette -> IO CInt
foreign import ccall "SDL.h SDL_SetSurfaceRLE" setSurfaceRLE :: Ptr Surface -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_UnlockSurface" unlockSurface :: Ptr Surface -> IO ()

foreign import ccall "SDL.h SDL_GetWindowWMInfo" getWindowWMInfo :: Window -> SysWMinfo -> IO Bool

foreign import ccall "SDL.h SDL_GetClipboardText" getClipboardText :: IO CString
foreign import ccall "SDL.h SDL_HasClipboardText" hasClipboardText :: IO Bool
foreign import ccall "SDL.h SDL_SetClipboardText" setClipboardText :: CString -> IO CInt

loadBMP :: CString -> IO (Ptr Surface)
loadBMP file = do
	rw <- withCString "rb" $ rwFromFile file
	loadBMP_RW rw 1

saveBMP :: Ptr Surface -> CString -> IO CInt
saveBMP surface file = do
	rw <- withCString "wb" $ rwFromFile file
	saveBMP_RW surface rw 1
