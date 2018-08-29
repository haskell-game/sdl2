{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SDL.Video.Vulkan (
  -- * Vulkan types
  VkInstance, VkSurfaceKHR, VkGetInstanceProcAddrFunc,
  -- * Vulkan loader
  vkLoadLibrary, vkUnloadLibrary, vkGetVkGetInstanceProcAddr,
  -- * Vulkan surface
  vkGetInstanceExtensions, vkCreateSurface,
  -- * Querying for the drawable size
  vkGetDrawableSize
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign hiding (throwIf_, throwIfNeg_)
import Foreign.C.Types (CInt)
import Foreign.C.String (CString, withCString)
import SDL.Vect (V2 (V2))
import SDL.Internal.Exception (throwIf_, throwIfNeg_)
import SDL.Internal.Types (Window (Window))
import SDL.Raw.Types (VkInstance, VkSurfaceKHR, VkGetInstanceProcAddrFunc)
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Dynamically load a Vulkan loader library.
--
-- If a filePath is 'Nothing', SDL will use the value of the environment variable
-- SDL_VULKAN_LIBRARY, if set, otherwise it loads the default Vulkan
-- loader library.
--
-- This function should be called after initializing the video driver
-- (i.e. 'SDL.Init.initialize' ['SDL.Init.InitVideo']), but before
-- creating any windows with 'SDL.Video.windowGraphicsContext' = 'SDL.Video.VulkanContext'.
--
-- If no Vulkan loader library is loaded, analogue of 'vkLoadLibrary' 'Nothing'
-- will be automatically called by SDL C library upon creation of the first Vulkan window.
--
-- Throws 'SDL.Exception.SDLException' if there are no working Vulkan drivers installed.
vkLoadLibrary :: MonadIO m => Maybe FilePath -> m ()
vkLoadLibrary = \case
    Nothing       -> liftIO . testNeg $ Raw.vkLoadLibrary nullPtr
    Just filePath -> liftIO . withCString filePath $ testNeg . Raw.vkLoadLibrary
  where
    testNeg = throwIfNeg_ "SDL.Video.Vulkan.vkLoadLibrary" "SDL_Vulkan_LoadLibrary"

-- | Unload the Vulkan loader library previously loaded by 'vkLoadLibrary'.
--
-- Analogue of this function will be automatically called by SDL C library
-- after destruction of the last window with
-- 'SDL.Video.windowGraphicsContext' = 'SDL.Video.VulkanContext'.
vkUnloadLibrary :: MonadIO m => m ()
vkUnloadLibrary = Raw.vkUnloadLibrary

foreign import ccall "dynamic" mkVkGetInstanceProcAddrFunc ::
  FunPtr VkGetInstanceProcAddrFunc -> VkGetInstanceProcAddrFunc

-- | Get the vkGetInstanceProcAddr function, which can be used to obtain another Vulkan functions
-- (see <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetInstanceProcAddr.html>).
--
-- The 'vkGetVkGetInstanceProcAddr' function should be called after either calling 'vkLoadLibrary'
-- function or creating first Vulkan window.
vkGetVkGetInstanceProcAddr :: (Functor m, MonadIO m) => m VkGetInstanceProcAddrFunc
vkGetVkGetInstanceProcAddr = mkVkGetInstanceProcAddrFunc <$> Raw.vkGetVkGetInstanceProcAddr

-- | Get the names of the Vulkan instance extensions needed to create
-- a surface with 'vkCreateSurface'.
--
-- The extension names queried here must be enabled when calling vkCreateInstance
-- (see <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateInstance.html>),
-- otherwise 'vkCreateSurface' will fail.
--
-- Window should have been created with 'SDL.Video.windowGraphicsContext' = 'SDL.Video.VulkanContext'.
--
-- Throws 'SDL.Exception.SDLException' on failure.
vkGetInstanceExtensions :: MonadIO m => Window -> m [CString]
vkGetInstanceExtensions (Window w) = liftIO . alloca $ \countPtr -> do
  throwIf_ not "SDL.Video.Vulkan.vkGetInstanceExtensions (1)" "SDL_Vulkan_GetInstanceExtensions" $
    Raw.vkGetInstanceExtensions w countPtr nullPtr
  count <- fromIntegral <$> peek countPtr
  allocaArray count $ \sPtr ->
    throwIf_ not "SDL.Video.Vulkan.vkGetInstanceExtensions (2)" "SDL_Vulkan_GetInstanceExtensions"
      (Raw.vkGetInstanceExtensions w countPtr sPtr) >> peekArray count sPtr

-- | Create a Vulkan rendering surface for a window.
--
-- Window should have been created with 'SDL.Video.windowGraphicsContext' = 'SDL.Video.VulkanContext'.
--
-- Instance should have been created with the extensions returned
-- by 'vkGetInstanceExtensions' enabled.
--
-- Throws 'SDL.Exception.SDLException' on failure.
vkCreateSurface :: MonadIO m => Window -> VkInstance -> m VkSurfaceKHR
vkCreateSurface (Window w) vkInstance = liftIO . alloca $ \vkSurfacePtr ->
  throwIf_ not "SDL.Video.Vulkan.vkCreateSurface" "SDL_Vulkan_CreateSurface"
    (Raw.vkCreateSurface w vkInstance vkSurfacePtr) >> peek vkSurfacePtr

-- | Get the size of a window's underlying drawable area in pixels (for use
-- with setting viewport, scissor & etc).
--
-- It may differ from 'SDL.Video.windowSize' if window was created with 'SDL.Video.windowHighDPI' flag.
vkGetDrawableSize :: MonadIO m => Window -> m (V2 CInt)
vkGetDrawableSize (Window w) = liftIO . alloca $ \wptr ->
  alloca $ \hptr -> do
    Raw.vkGetDrawableSize w wptr hptr
    V2 <$> peek wptr <*> peek hptr
