{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Video.Renderer
  ( Renderer

  -- * Drawing Primitives
  , blitScaled
  , blitSurface
  , createTexture
  , createTextureFromSurface
  , convertSurface
  , destroyTexture
  , createRGBSurface
  , createRGBSurfaceFrom
  , lockTexture
  , unlockTexture
  , lockSurface
  , unlockSurface
  , fillRect
  , fillRects
  , freeSurface
  , glBindTexture
  , glUnbindTexture
  , loadBMP
  , mapRGB
  , renderTargetSupported
  , formatPalette
  , setPaletteColors
  , getWindowSurface
  , colorKey
  , renderDrawBlendMode
  , renderDrawColor
  , renderTarget
  , textureAlphaMod
  , textureBlendMode
  , surfaceBlendMode
  , textureColorMod

  , surfaceDimensions
  , surfacePixels
  , surfaceFormat
  , updateWindowSurface
  , queryTexture
  , BlendMode(..)
  , Rectangle(..)
  , Surface(..)
  , SurfacePixelFormat
  , Texture
  , TextureInfo(..)
  , TextureAccess(..)
  , PixelFormat(..)
  , Palette

  -- * Drawing Primitives
  , renderClear
  , renderCopy
  , renderCopyEx
  , renderDrawLine
  , renderDrawLines
  , renderDrawPoint
  , renderDrawPoints
  , renderDrawRect
  , renderDrawRects
  , renderFillRect
  , renderFillRects
  , renderPresent
  , renderClipRect
  , renderLogicalSize
  , renderScale
  , renderViewport

  -- * Utilities
  , RendererConfig(..)
  , defaultRenderer
  , RendererInfo(..)
  , getRendererInfo
  , getRenderDriverInfo
  ) where

import Prelude hiding (foldr)

import Data.StateVar
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Data (Data)
import Data.Foldable
import Data.Text (Text)
import Data.Traversable
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import Linear
import Linear.Affine (Point(P))
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types

import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified SDL.Raw as Raw

-- | Perform a fast surface copy to a destination surface.
--
-- See @<https://wiki.libsdl.org/SDL_BlitSurface SDL_BlitSurface>@ for C documentation.
blitSurface :: MonadIO m
            => Surface -- ^ The 'Surface' to be copied from
            -> Maybe (Rectangle CInt) -- ^ The rectangle to be copied, or 'Nothing' to copy the entire surface
            -> Surface -- ^ The 'Surface' that is the blit target
            -> Maybe (Point V2 CInt) -- ^ The position to blit to
            -> m ()
blitSurface (Surface src _) srcRect (Surface dst _) dstLoc = liftIO $
  throwIfNeg_ "SDL.Video.blitSurface" "SDL_BlitSurface" $
  maybeWith with srcRect $ \srcPtr ->
  maybeWith with (fmap (flip Rectangle 0) dstLoc) $ \dstPtr ->
  Raw.blitSurface src (castPtr srcPtr) dst (castPtr dstPtr)

-- | Create a texture for a rendering context.
--
-- See @<https://wiki.libsdl.org/SDL_CreateTexture SDL_CreateTexture>@ for C documentation.
createTexture :: (Functor m,MonadIO m)
              => Renderer -- ^ The rendering context.
              -> PixelFormat
              -> TextureAccess
              -> V2 CInt -- ^ The size of the texture.
              -> m Texture
createTexture (Renderer r) fmt access (V2 w h) =
  fmap Texture $
  throwIfNull "SDL.Video.Renderer.createTexture" "SDL_CreateTexture" $
  Raw.createTexture r (toNumber fmt) (toNumber access) w h

-- | Create a texture from an existing surface.
--
-- See @<https://wiki.libsdl.org/SDL_CreateTextureFromSurface SDL_CreateTextureFromSurface>@ for C documentation.
createTextureFromSurface :: (Functor m,MonadIO m)
                         => Renderer -- ^ The rendering context
                         -> Surface -- ^ The surface containing pixel data used to fill the texture
                         -> m Texture
createTextureFromSurface (Renderer r) (Surface s _) =
  fmap Texture $
  throwIfNull "SDL.Video.createTextureFromSurface" "SDL_CreateTextureFromSurface" $
  Raw.createTextureFromSurface r s

-- | Bind an OpenGL\/ES\/ES2 texture to the current context for use with when rendering OpenGL primitives directly.
--
-- See @<https://wiki.libsdl.org/SDL_GL_BindTexture SDL_GL_BindTexture>@ for C documentation.
glBindTexture :: (Functor m,MonadIO m)
              => Texture -- ^ The texture to bind to the current OpenGL\/ES\/ES2 context
              -> m ()
glBindTexture (Texture t) =
  throwIfNeg_ "SDL.Video.Renderer.glBindTexture" "SDL_GL_BindTexture" $
  Raw.glBindTexture t nullPtr nullPtr

-- | Unbind an OpenGL\/ES\/ES2 texture from the current context.
--
-- See @<https://wiki.libsdl.org/SDL_GL_UnbindTexture SDL_GL_UnbindTexture>@ for C documentation.
glUnbindTexture :: (Functor m,MonadIO m)
                => Texture -- ^ The texture to unbind from the current OpenGL\/ES\/ES2 context
                -> m ()
glUnbindTexture (Texture t) =
  throwIfNeg_ "SDL.Video.Renderer.glUnindTexture" "SDL_GL_UnbindTexture" $
  Raw.glUnbindTexture t

-- | Destroy the specified texture.
--
-- See @<https://wiki.libsdl.org/SDL_DestroyTexture SDL_DestroyTexture>@ for the C documentation.
destroyTexture :: MonadIO m => Texture -> m ()
destroyTexture (Texture t) = Raw.destroyTexture t

-- | Lock a portion of the texture for *write-only* pixel access.
--
-- See @<https://wiki.libsdl.org/SDL_LockTexture SDL_LockTexture>@ for C documentation.
lockTexture :: MonadIO m
            => Texture -- ^ The 'Texture' to lock for access, which must have been created with 'TextureAccessStreaming'
            -> Maybe (Rectangle CInt) -- ^ The area to lock for access; 'Nothing' to lock the entire texture
            -> m (Ptr (),CInt) -- ^ A pointer to the locked pixels, appropriately offset by the locked area, and the pitch of the locked pixels (the pitch is the length of one row in bytes).
lockTexture (Texture t) rect = liftIO $
  alloca $ \pixelsPtr ->
  alloca $ \pitchPtr ->
  maybeWith with rect $ \rectPtr -> do
    throwIfNeg_ "lockTexture" "SDL_LockTexture" $
      Raw.lockTexture t (castPtr rectPtr) pixelsPtr pitchPtr
    pixels <- peek pixelsPtr
    pitch <- peek pitchPtr
    return (pixels, pitch)

-- | Unlock a texture, uploading the changes to video memory, if needed.
--
-- /Warning/: See <https://bugzilla.libsdl.org/show_bug.cgi?id=1586 Bug No. 1586> before using this function!
--
-- See @<https://wiki.libsdl.org/SDL_UnlockTexture SDL_UnlockTexture>@ for C documentation.
unlockTexture :: MonadIO m => Texture -> m ()
unlockTexture (Texture t) = liftIO $ Raw.unlockTexture t

-- | Set up a surface for directly accessing the pixels.
--
-- See @<https://wiki.libsdl.org/SDL_LockSurface SDL_LockSurface>@ for C documentation.
lockSurface :: MonadIO m => Surface -> m ()
lockSurface (Surface s _) = liftIO $
  throwIfNeg_ "lockSurface" "SDL_LockSurface" $
    Raw.lockSurface s

-- | Release a surface after directly accessing the pixels.
--
-- See @<https://wiki.libsdl.org/SDL_UnlockSurface SDL_UnlockSurface>@ for C documentation.
unlockSurface :: MonadIO m => Surface -> m ()
unlockSurface (Surface s _) = Raw.unlockSurface s

-- | Information to the GPU about how you will use a texture.
data TextureAccess
  = TextureAccessStatic
    -- ^ Changes rarely, cannot be locked
  | TextureAccessStreaming
    -- ^ changes frequently, can be locked
  | TextureAccessTarget
    -- ^ Can be used as a render target
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber TextureAccess CInt where
  fromNumber n' = case n' of
    Raw.SDL_TEXTUREACCESS_STATIC -> TextureAccessStatic
    Raw.SDL_TEXTUREACCESS_STREAMING -> TextureAccessStreaming
    Raw.SDL_TEXTUREACCESS_TARGET -> TextureAccessTarget
    _ -> error "Unknown value"

instance ToNumber TextureAccess CInt where
  toNumber t = case t of
    TextureAccessStatic -> Raw.SDL_TEXTUREACCESS_STATIC
    TextureAccessStreaming -> Raw.SDL_TEXTUREACCESS_STREAMING
    TextureAccessTarget -> Raw.SDL_TEXTUREACCESS_TARGET

data TextureInfo = TextureInfo
  { texturePixelFormat :: PixelFormat
    -- ^ Raw format of the texture; the actual format may differ, but pixel transfers will use this format
  , textureAccess      :: TextureAccess
    -- ^ The access available to the texture
  , textureWidth       :: CInt
    -- ^ The width of the texture
  , textureHeight      :: CInt
    -- ^ The height of the texture
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

-- | Query the attributes of a texture.
--
-- See @<https://wiki.libsdl.org/SDL_QueryTexture SDL_QueryTexture>@ for C documentation.
queryTexture :: MonadIO m => Texture -> m TextureInfo
queryTexture (Texture tex) = liftIO $
  alloca $ \pfPtr ->
  alloca $ \acPtr ->
  alloca $ \wPtr ->
  alloca $ \hPtr -> do
    throwIfNeg_ "SDL.Video.queryTexture" "SDL_QueryTexture" $
      Raw.queryTexture tex pfPtr acPtr wPtr hPtr
    TextureInfo <$>
      fmap fromNumber (peek pfPtr) <*>
      fmap fromNumber (peek acPtr) <*>
      peek wPtr <*>
      peek hPtr

-- | Allocate a new RGB surface.
--
-- See @<https://wiki.libsdl.org/SDL_CreateRGBSurface SDL_CreateRGBSurface>@ for C documentation.
createRGBSurface :: (Functor m,MonadIO m)
                 => V2 CInt -- ^ The size of the surface
                 -> CInt -- ^ The bit-depth of the surface
                 -> V4 Word32 -- ^ The red, green, blue and alpha mask for the pixels
                 -> m Surface
createRGBSurface (V2 w h) d (V4 r g b a) =
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.createRGBSurface" "SDL_CreateRGBSurface" $
  Raw.createRGBSurface 0 w h d r g b a

-- | Allocate a new RGB surface with existing pixel data.
--
-- See @<https://wiki.libsdl.org/SDL_CreateRGBSurfaceFrom SDL_CreateRGBSurfaceFrom>@ for C documentation.
createRGBSurfaceFrom :: (Functor m,MonadIO m)
                     => MSV.IOVector Word8 -- ^ The existing pixel data
                     -> V2 CInt -- ^ The size of the surface
                     -> CInt -- ^ The bit-depth of the surface
                     -> CInt -- ^ The pitch - the length of a row of pixels in bytes
                     -> V4 Word32 -- ^ The red, green blue and alpha mask for the pixels
                     -> m Surface
createRGBSurfaceFrom pixels (V2 w h) d p (V4 r g b a) = liftIO $
  fmap (managedSurface pixels) $
  throwIfNull "SDL.Video.createRGBSurfaceFrom" "SDL_CreateRGBSurfaceFrom" $
    MSV.unsafeWith pixels $ \pixelPtr ->
      Raw.createRGBSurfaceFrom (castPtr pixelPtr) w h d p r g b a

-- | Perform a fast fill of a rectangle with a specific color.
--
-- If there is a clip rectangle set on the destination (set via 'clipRect'), then this function will fill based on the intersection of the clip rectangle and the given 'Rectangle'.
--
-- See @<https://wiki.libsdl.org/SDL_FillRect SDL_FillRect>@ for C documentation.
fillRect :: MonadIO m
         => Surface -- ^ The 'Surface' that is the drawing target.
         -> Maybe (Rectangle CInt) -- ^ The rectangle to fill, or 'Nothing' to fill the entire surface.
         -> Word32 -- ^ The color to fill with. This should be a pixel of the format used by the surface, and can be generated by 'mapRGB' or 'mapRGBA'. If the color value contains an alpha component then the destination is simply filled with that alpha information, no blending takes place.
         -> m ()
fillRect (Surface s _) rect col = liftIO $
  throwIfNeg_ "SDL.Video.fillRect" "SDL_FillRect" $
  maybeWith with rect $ \rectPtr ->
  Raw.fillRect s (castPtr rectPtr) col

-- | Perform a fast fill of a set of rectangles with a specific color.
--
-- If there is a clip rectangle set on any of the destinations (set via 'clipRect'), then this function will fill based on the intersection of the clip rectangle and the given 'Rectangle's.
--
-- See @<https://wiki.libsdl.org/SDL_FillRect SDL_FillRects>@ for C documentation.
fillRects :: MonadIO m
          => Surface -- ^ The 'Surface' that is the drawing target.
          -> SV.Vector (Rectangle CInt) -- ^ A 'SV.Vector' of 'Rectangle's to be filled.
          -> Word32 -- ^ The color to fill with. This should be a pixel of the format used by the surface, and can be generated by 'mapRGB' or 'mapRGBA'. If the color value contains an alpha component then the destination is simply filled with that alpha information, no blending takes place.
          -> m ()
fillRects (Surface s _) rects col = liftIO $ do
  throwIfNeg_ "SDL.Video.fillRects" "SDL_FillRects" $
    SV.unsafeWith rects $ \rp ->
      Raw.fillRects s
                    (castPtr rp)
                    (fromIntegral (SV.length rects))
                    col

-- | Free an RGB surface.
--
-- If the surface was created using 'createRGBSurfaceFrom' then the pixel data is not freed.
--
-- See @<https://wiki.libsdl.org/SDL_FreeSurface SDL_FreeSurface>@ for the C documentation.
freeSurface :: MonadIO m => Surface -> m ()
freeSurface (Surface s _) = Raw.freeSurface s

-- | Load a surface from a BMP file.
--
-- See @<https://wiki.libsdl.org/SDL_LoadBMP SDL_LoadBMP>@ for C documentation.
loadBMP :: MonadIO m => FilePath -> m Surface
loadBMP filePath = liftIO $
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.loadBMP" "SDL_LoadBMP" $
  withCString filePath $ Raw.loadBMP

newtype SurfacePixelFormat = SurfacePixelFormat (Ptr Raw.PixelFormat)
  deriving (Eq, Typeable)

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. De need to guarantee that pointers aren't reused?
-- | Map an RGB triple to an opaque pixel value for a given pixel format.
--
-- This function maps the RGB color value to the specified pixel format and returns the pixel value best approximating the given RGB color value for the given pixel format.
--
-- If the format has a palette (8-bit) the index of the closest matching color in the palette will be returned.
--
-- If the specified pixel format has an alpha component it will be returned as all 1 bits (fully opaque).
--
-- If the pixel format bpp (color depth) is less than 32-bpp then the unused upper bits of the return value can safely be ignored (e.g., with a 16-bpp format the return value can be assigned to a 'Word16', and similarly a 'Word8' for an 8-bpp format).
--
-- See @<https://wiki.libsdl.org/SDL_MapRGB SDL_MapRGB>@ for C documentation.
mapRGB :: MonadIO m
       => SurfacePixelFormat -- ^ The format of the pixel
       -> V3 Word8 -- ^ The color to map
       -> m Word32
mapRGB (SurfacePixelFormat fmt) (V3 r g b) = Raw.mapRGB fmt r g b

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. surface->{w,h} are immutable, but do we need to guarantee that pointers
-- aren't reused by *different* surfaces.
surfaceDimensions :: MonadIO m => Surface -> m (V2 CInt)
surfaceDimensions (Surface s _) = liftIO $ (V2 <$> Raw.surfaceW <*> Raw.surfaceH) <$> peek s

surfacePixels :: MonadIO m => Surface -> m (Ptr ())
surfacePixels (Surface s _) = liftIO $ Raw.surfacePixels <$> peek s

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. surface->format is immutable, but do we need to guarantee that pointers
-- aren't reused by *different* surfaces?
surfaceFormat :: MonadIO m => Surface -> m SurfacePixelFormat
surfaceFormat (Surface s _) = liftIO $ SurfacePixelFormat . Raw.surfaceFormat <$> peek s

newtype Palette = Palette (Ptr Raw.Palette)
  deriving (Eq, Typeable)

formatPalette :: MonadIO m => SurfacePixelFormat -> m (Maybe Palette)
formatPalette (SurfacePixelFormat f) = liftIO $ wrap . Raw.pixelFormatPalette <$> peek f
  where wrap p
          | p == nullPtr = Nothing
          | otherwise = Just (Palette p)

-- | Set a range of colors in a palette.
--
-- See @<https://wiki.libsdl.org/SDL_SetPaletteColors SDL_SetPaletteColors>@ for C documentation.
setPaletteColors :: MonadIO m
                 => Palette -- ^ The 'Palette' to modify
                 -> (SV.Vector (V4 Word8)) -- ^ A 'SV.Vector' of colours to copy into the palette
                 -> CInt -- ^ The index of the first palette entry to modify
                 -> m ()
setPaletteColors (Palette p) colors first = liftIO $
  throwIfNeg_ "SDL.Video.setPaletteColors" "SDL_SetPaletteColors" $
  SV.unsafeWith colors $ \cp ->
    Raw.setPaletteColors p (castPtr cp) first n
  where
    n = fromIntegral $ SV.length colors

-- | Get the SDL surface associated with the window.
--
-- See @<https://wiki.libsdl.org/SDL_GetWindowSurface SDL_GetWindowSurface>@ for C documentation.
getWindowSurface :: (Functor m, MonadIO m) => Window -> m Surface
getWindowSurface (Window w) =
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.getWindowSurface" "SDL_GetWindowSurface" $
  Raw.getWindowSurface w

-- | Get or set the blend mode used for drawing operations (fill and line).
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetRenderDrawBlendMode SDL_SetRenderDrawBlendMode>@ and @<https://wiki.libsdl.org/SDL_GetRenderDrawBlendMode SDL_GetRenderDrawBlendMode>@ for C documentation.
renderDrawBlendMode :: Renderer -> StateVar BlendMode
renderDrawBlendMode (Renderer r) = makeStateVar getRenderDrawBlendMode setRenderDrawBlendMode
  where
  getRenderDrawBlendMode = liftIO $
    alloca $ \bmPtr -> do
      throwIfNeg_ "SDL.Video.Renderer.getRenderDrawBlendMode" "SDL_GetRenderDrawBlendMode" $
        Raw.getRenderDrawBlendMode r bmPtr
      fromNumber <$> peek bmPtr

  setRenderDrawBlendMode bm =
    throwIfNeg_ "SDL.Video.Renderer.setRenderDrawBlendMode" "SDL_SetRenderDrawBlendMode" $
    Raw.setRenderDrawBlendMode r (toNumber bm)

-- | Get or set the color used for drawing operations (rect, line and clear).
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetRenderDrawColor SDL_SetRenderDrawColor>@ and @<https://wiki.libsdl.org/SDL_GetRenderDrawColor SDL_GetRenderDrawColor>@ for C documentation.
renderDrawColor :: Renderer -> StateVar (V4 Word8)
renderDrawColor (Renderer re) = makeStateVar getRenderDrawColor setRenderDrawColor
  where
  getRenderDrawColor = liftIO $
    alloca $ \r ->
    alloca $ \g ->
    alloca $ \b ->
    alloca $ \a -> do
      throwIfNeg_ "SDL.Video.Renderer.getRenderDrawColor" "SDL_GetRenderDrawColor" $
        Raw.getRenderDrawColor re r g b a
      V4 <$> peek r <*> peek g <*> peek b <*> peek a

  setRenderDrawColor (V4 r g b a) =
    throwIfNeg_ "SDL.Video.setRenderDrawColor" "SDL_SetRenderDrawColor" $
    Raw.setRenderDrawColor re r g b a

-- | Copy the window surface to the screen.
--
-- This is the function you use to reflect any changes to the surface on the screen.
--
-- See @<https://wiki.libsdl.org/SDL_UpdateWindowSurface SDL_UpdateWindowSurface>@ for C documentation.
updateWindowSurface :: (Functor m, MonadIO m) => Window -> m ()
updateWindowSurface (Window w) =
  throwIfNeg_ "SDL.Video.updateWindowSurface" "SDL_UpdateWindowSurface" $
    Raw.updateWindowSurface w

-- | Blend modes used in 'renderCopy' and drawing operations.
data BlendMode
  = BlendNone
    -- ^ No blending
  | BlendAlphaBlend
    -- ^ Alpha blending.
    --
    -- @
    -- dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA))
    -- dstA = srcA + (dstA * (1-srcA))
    -- @
  | BlendAdditive
    -- ^ Additive blending
    --
    -- @
    -- dstRGB = (srcRGB * srcA) + dstRGB
    -- dstA = dstA
    -- @
  | BlendMod
    -- ^ Color modulate
    --
    -- @
    -- dstRGB = srcRGB * dstRGB
    -- dstA = dstA
    --
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber BlendMode Word32 where
  fromNumber n = case n of
    Raw.SDL_BLENDMODE_ADD -> BlendAdditive
    Raw.SDL_BLENDMODE_BLEND -> BlendAlphaBlend
    Raw.SDL_BLENDMODE_ADD -> BlendAdditive
    Raw.SDL_BLENDMODE_MOD -> BlendMod
    _ -> error $ "fromNumber<BlendMode>: unknown blend mode: " ++ show n

instance ToNumber BlendMode Word32 where
  toNumber BlendNone = Raw.SDL_BLENDMODE_NONE
  toNumber BlendAlphaBlend = Raw.SDL_BLENDMODE_BLEND
  toNumber BlendAdditive = Raw.SDL_BLENDMODE_ADD
  toNumber BlendMod = Raw.SDL_BLENDMODE_MOD

data Rectangle a = Rectangle (Point V2 a) (V2 a)
  deriving (Eq, Functor, Generic, Ord, Read, Show, Typeable)

instance Storable a => Storable (Rectangle a) where
  sizeOf ~(Rectangle o s) = sizeOf o + sizeOf s
  alignment _ = 0
  peek ptr = do
    o <- peek (castPtr ptr)
    s <- peek (castPtr (ptr `plusPtr` sizeOf o))
    return (Rectangle o s)
  poke ptr (Rectangle o s) = do
    poke (castPtr ptr) o
    poke (castPtr (ptr `plusPtr` sizeOf o)) s

data Surface = Surface (Ptr Raw.Surface) (Maybe (MSV.IOVector Word8))
  deriving (Typeable)

unmanagedSurface :: Ptr Raw.Surface -> Surface
unmanagedSurface s = Surface s Nothing

managedSurface :: MSV.IOVector Word8 -> Ptr Raw.Surface -> Surface
managedSurface p s = Surface s (Just p)

newtype Texture = Texture Raw.Texture
  deriving (Eq, Typeable)

-- | Draw a rectangle outline on the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderDrawRect SDL_RenderDrawRect>@ for C documentation.
renderDrawRect :: MonadIO m
               => Renderer
               -> Maybe (Rectangle CInt) -- ^ The rectangle outline to draw. 'Nothing' for the entire rendering context.
               -> m ()
renderDrawRect (Renderer r) rect = liftIO $
  throwIfNeg_ "SDL.Video.renderDrawRect" "SDL_RenderDrawRect" $
  maybeWith with rect (Raw.renderDrawRect r . castPtr)

-- | Draw some number of rectangles on the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderDrawRects SDL_RenderDrawRects>@ for C documentation.
renderDrawRects :: MonadIO m => Renderer -> SV.Vector (Rectangle CInt) -> m ()
renderDrawRects (Renderer r) rects = liftIO $
  throwIfNeg_ "SDL.Video.renderDrawRects" "SDL_RenderDrawRects" $
  SV.unsafeWith rects $ \rp ->
    Raw.renderDrawRects r
                        (castPtr rp)
                        (fromIntegral (SV.length rects))

-- | Fill a rectangle on the current rendering target with the drawing color.
--
-- See @<https://wiki.libsdl.org/SDL_RenderFillRect SDL_RenderFillRect>@ for C documentation.
renderFillRect :: MonadIO m
               => Renderer
               -> Maybe (Rectangle CInt) -- ^ The rectangle to fill. 'Nothing' for the entire rendering context.
               -> m ()
renderFillRect (Renderer r) rect = liftIO $ do
  throwIfNeg_ "SDL.Video.renderFillRect" "SDL_RenderFillRect" $
    maybeWith with rect $ \rPtr ->
      Raw.renderFillRect r
                         (castPtr rPtr)

-- | Fill some number of rectangles on the current rendering target with the drawing color.
--
-- See @<https://wiki.libsdl.org/SDL_RenderFillRects SDL_RenderFillRects>@ for C documentation.
renderFillRects :: MonadIO m => Renderer -> SV.Vector (Rectangle CInt) -> m ()
renderFillRects (Renderer r) rects = liftIO $
  throwIfNeg_ "SDL.Video.renderFillRects" "SDL_RenderFillRects" $
    SV.unsafeWith rects $ \rp ->
      Raw.renderFillRects r
                          (castPtr rp)
                          (fromIntegral (SV.length rects))

-- | Clear the current rendering target with the drawing color.
--
-- See @<https://wiki.libsdl.org/SDL_RenderClear SDL_RenderClear>@ for C documentation.
renderClear :: (Functor m, MonadIO m) => Renderer -> m ()
renderClear (Renderer r) =
  throwIfNeg_ "SDL.Video.renderClear" "SDL_RenderClear" $
  Raw.renderClear r

-- | Get or set the drawing scale for rendering on the current target.
--
-- The drawing coordinates are scaled by the x\/y scaling factors before they are used by the renderer. This allows resolution independent drawing with a single coordinate system.
--
-- If this results in scaling or subpixel drawing by the rendering backend, it will be handled using the appropriate quality hints. For best results use integer scaling factors.
--
-- See @<https://wiki.libsdl.org/SDL_RenderSetScale SDL_RenderSetScale>@ and @<https://wiki.libsdl.org/SDL_RenderGetScale SDL_RenderGetScale>@ for C documentation.
renderScale :: Renderer -> StateVar (V2 CFloat)
renderScale (Renderer r) = makeStateVar renderGetScale renderSetScale
  where
  renderSetScale (V2 x y) =
    throwIfNeg_ "SDL.Video.renderSetScale" "SDL_RenderSetScale" $
    Raw.renderSetScale r x y

  renderGetScale = liftIO $
    alloca $ \w ->
    alloca $ \h -> do
      Raw.renderGetScale r w h
      V2 <$> peek w <*> peek h

-- | Get or set the clip rectangle for rendering on the specified target.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_RenderSetClipRect SDL_RenderSetClipRect>@ and @<https://wiki.libsdl.org/SDL_RenderGetClipRect SDL_RenderGetClipRect>@ for C documentation.
renderClipRect :: Renderer -> StateVar (Maybe (Rectangle CInt))
renderClipRect (Renderer r) = makeStateVar renderGetClipRect renderSetClipRect
  where
  renderGetClipRect = liftIO $
    alloca $ \rPtr -> do
      Raw.renderGetClipRect r rPtr
      maybePeek peek (castPtr rPtr)
  renderSetClipRect rect =
    liftIO $
    throwIfNeg_ "SDL.Video.renderSetClipRect" "SDL_RenderSetClipRect" $
    maybeWith with rect $ Raw.renderSetClipRect r . castPtr

-- | Get or set the drawing area for rendering on the current target.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_RenderSetViewport SDL_RenderSetViewport>@ and @<https://wiki.libsdl.org/SDL_RenderGetViewport SDL_RenderGetViewport>@ for C documentation.
renderViewport :: Renderer -> StateVar (Maybe (Rectangle CInt))
renderViewport (Renderer r) = makeStateVar renderGetViewport renderSetViewport
  where
  renderGetViewport = liftIO $
    alloca $ \rect -> do
      Raw.renderGetViewport r rect
      maybePeek peek (castPtr rect)

  renderSetViewport rect =
    liftIO $
    throwIfNeg_ "SDL.Video.renderSetViewport" "SDL_RenderSetViewport" $
    maybeWith with rect $ Raw.renderSetViewport r . castPtr

-- | Update the screen with any rendering performed since the previous call.
--
-- SDL\'s rendering functions operate on a backbuffer; that is, calling a rendering function such as 'renderDrawLine' does not directly put a line on the screen, but rather updates the backbuffer. As such, you compose your entire scene and present the composed backbuffer to the screen as a complete picture.
--
-- Therefore, when using SDL's rendering API, one does all drawing intended for the frame, and then calls this function once per frame to present the final drawing to the user.
--
-- The backbuffer should be considered invalidated after each present; do not assume that previous contents will exist between frames. You are strongly encouraged to call 'renderClear' to initialize the backbuffer before starting each new frame's drawing, even if you plan to overwrite every pixel.
--
-- See @<https://wiki.libsdl.org/SDL_RenderPresent SDL_RenderPresent>@ for C documentation.
renderPresent :: MonadIO m => Renderer -> m ()
renderPresent (Renderer r) = Raw.renderPresent r

-- | Copy a portion of the texture to the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderCopy SDL_RenderCopy>@ for C documentation.
renderCopy :: MonadIO m
           => Renderer -- ^ The rendering context
           -> Texture -- ^ The source texture
           -> Maybe (Rectangle CInt) -- ^ The source rectangle to copy, or 'Nothing' for the whole texture
           -> Maybe (Rectangle CInt) -- ^ The destination rectangle to copy to, or 'Nothing' for the whole rendering target. The texture will be stretched to fill the given rectangle.
           -> m ()
renderCopy (Renderer r) (Texture t) srcRect dstRect =
  liftIO $
  throwIfNeg_ "SDL.Video.renderCopy" "SDL_RenderCopy" $
  maybeWith with srcRect $ \src ->
  maybeWith with dstRect $ \dst ->
  Raw.renderCopy r t (castPtr src) (castPtr dst)

-- | Copy a portion of the texture to the current rendering target, optionally rotating it by angle around the given center and also flipping it top-bottom and/or left-right.
--
-- See @<https://wiki.libsdl.org/SDL_RenderCopy SDL_RenderCopyEx>@ for C documentation.
renderCopyEx :: MonadIO m
             => Renderer -- ^ The rendering context
             -> Texture -- ^ The source texture
             -> Maybe (Rectangle CInt) -- ^ The source rectangle to copy, or 'Nothing' for the whole texture
             -> Maybe (Rectangle CInt) -- ^ The destination rectangle to copy to, or 'Nothing' for the whole rendering target. The texture will be stretched to fill the given rectangle.
             -> CDouble -- ^ An angle in degrees that indicates the point around which the destination rectangle will be rotated.
             -> Maybe (Point V2 CInt) -- ^ The point of rotation
             -> V2 Bool -- ^ Whether to flip in the X or Y axis. -- ^ The point of rotation
             -> m () -- ^ Whether to flip in the X or Y axis.
renderCopyEx (Renderer r) (Texture t) srcRect dstRect theta center flips =
  liftIO $
  throwIfNeg_ "SDL.Video.renderCopyEx" "SDL_RenderCopyEx" $
  maybeWith with srcRect $ \src ->
  maybeWith with dstRect $ \dst ->
  maybeWith with center $ \c ->
  Raw.renderCopyEx r t (castPtr src) (castPtr dst) theta (castPtr c)
                   (case flips of
                      V2 x y -> (if x then Raw.SDL_FLIP_HORIZONTAL else 0) .|.
                               (if y then Raw.SDL_FLIP_VERTICAL else 0))

-- | Draw a line on the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderDrawLine SDL_RenderDrawLine>@ for C documentation.
renderDrawLine :: (Functor m,MonadIO m)
               => Renderer
               -> Point V2 CInt -- ^ The start point of the line
               -> Point V2 CInt -- ^ The end point of the line
               -> m ()
renderDrawLine (Renderer r) (P (V2 x y)) (P (V2 x' y')) =
  throwIfNeg_ "SDL.Video.renderDrawLine" "SDL_RenderDrawLine" $
  Raw.renderDrawLine r x y x' y'

-- | Draw a series of connected lines on the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderDrawLines SDL_RenderDrawLines>@ for C documentation.
renderDrawLines :: MonadIO m
                => Renderer
                -> SV.Vector (Point V2 CInt) -- ^ A 'SV.Vector' of points along the line. SDL will draw lines between these points.
                -> m ()
renderDrawLines (Renderer r) points =
  liftIO $
  throwIfNeg_ "SDL.Video.renderDrawLines" "SDL_RenderDrawLines" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawLines r
                        (castPtr cp)
                        (fromIntegral (SV.length points))

-- | Draw a point on the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderDrawPoint SDL_RenderDrawPoint>@ for C documentation.
renderDrawPoint :: (Functor m, MonadIO m) => Renderer -> Point V2 CInt -> m ()
renderDrawPoint (Renderer r) (P (V2 x y)) =
  throwIfNeg_ "SDL.Video.renderDrawPoint" "SDL_RenderDrawPoint" $
  Raw.renderDrawPoint r x y

-- | Draw multiple points on the current rendering target.
--
-- See @<https://wiki.libsdl.org/SDL_RenderDrawPoints SDL_RenderDrawPoints>@ for C documentation.
renderDrawPoints :: MonadIO m => Renderer -> SV.Vector (Point V2 CInt) -> m ()
renderDrawPoints (Renderer r) points =
  liftIO $
  throwIfNeg_ "SDL.Video.renderDrawPoints" "SDL_RenderDrawPoints" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawPoints r
                         (castPtr cp)
                         (fromIntegral (SV.length points))

-- | Copy an existing surface into a new one that is optimized for blitting to a surface of a specified pixel format.
--
-- This function is used to optimize images for faster repeat blitting. This is accomplished by converting the original and storing the result as a new surface. The new, optimized surface can then be used as the source for future blits, making them faster.
--
-- See @<https://wiki.libsdl.org/SDL_ConvertSurface SDL_ConvertSurface>@ for C documentation.
convertSurface :: (Functor m,MonadIO m)
               => Surface -- ^ The 'Surface' to convert
               -> SurfacePixelFormat -- ^ The pixel format that the new surface is optimized for
               -> m Surface
convertSurface (Surface s _) (SurfacePixelFormat fmt) =
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.Renderer.convertSurface" "SDL_ConvertSurface" $
  Raw.convertSurface s fmt 0

-- | Perform a scaled surface copy to a destination surface.
--
-- See @<https://wiki.libsdl.org/SDL_BlitScaled SDL_BlitScaled>@ for C documentation.
blitScaled :: MonadIO m
           => Surface -- ^ The 'Surface' to be copied from
           -> Maybe (Rectangle CInt) -- ^ The rectangle to be copied, or 'Nothing' to copy the entire surface
           -> Surface -- ^ The 'Surface' that is the blit target
           -> Maybe (Rectangle CInt) -- ^ The rectangle that is copied into, or 'Nothing' to copy into the entire surface
           -> m ()
blitScaled (Surface src _) srcRect (Surface dst _) dstRect =
  liftIO $
  throwIfNeg_ "SDL.Video.blitSurface" "SDL_BlitSurface" $
  maybeWith with srcRect $ \srcPtr ->
  maybeWith with dstRect $ \dstPtr ->
  Raw.blitScaled src (castPtr srcPtr) dst (castPtr dstPtr)

-- | Get or set the color key (transparent pixel color) for a surface.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetColorKey SDL_SetColorKey>@ and @<https://wiki.libsdl.org/SDL_GetColorKey SDL_GetColorKey>@ for C documentation.
colorKey :: Surface -> StateVar (Maybe Word32)
colorKey (Surface s _) = makeStateVar getColorKey setColorKey
  where
  getColorKey =
    liftIO $
    alloca $ \keyPtr -> do
      ret <- Raw.getColorKey s keyPtr
      if ret == -1 then return Nothing else fmap Just (peek keyPtr)
  setColorKey key =
    liftIO $
    throwIfNeg_ "SDL.Video.Renderer.setColorKey" "SDL_SetColorKey" $
    case key of
      Nothing ->
        alloca $ \keyPtr -> do
          -- TODO Error checking?
          ret <- Raw.getColorKey s keyPtr
          if ret == -1
            -- if ret == -1 then there is no key enabled, so we have nothing to
            -- do.
            then return 0
            else do key' <- peek keyPtr
                    Raw.setColorKey s 0 key'

      Just key' -> do
        Raw.setColorKey s 1 key'

-- | Get or set the additional color value multiplied into render copy operations.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetTextureColorMod SDL_SetTextureColorMod>@ and @<https://wiki.libsdl.org/SDL_GetTextureColorMod SDL_GetTextureColorMod>@ for C documentation.
textureColorMod :: Texture -> StateVar (V3 Word8)
textureColorMod (Texture t) = makeStateVar getTextureColorMod setTextureColorMod
  where
  getTextureColorMod = liftIO $
    alloca $ \r ->
    alloca $ \g ->
    alloca $ \b -> do
      throwIfNeg_ "SDL.Video.Renderer.getTextureColorMod" "SDL_GetTextureColorMod" $
        Raw.getTextureColorMod t r g b
      V3 <$> peek r <*> peek g <*> peek b

  setTextureColorMod (V3 r g b) =
    throwIfNeg_ "SDL.Video.Renderer.setTextureColorMod" "SDL_SetTextureColorMod" $
    Raw.setTextureColorMod t r g b

data PixelFormat
  = Unknown
  | Index1LSB
  | Index1MSB
  | Index4LSB
  | Index4MSB
  | Index8
  | RGB332
  | RGB444
  | RGB555
  | BGR555
  | ARGB4444
  | RGBA4444
  | ABGR4444
  | BGRA4444
  | ARGB1555
  | RGBA5551
  | ABGR1555
  | BGRA5551
  | RGB565
  | BGR565
  | RGB24
  | BGR24
  | RGB888
  | RGBX8888
  | BGR888
  | BGRX8888
  | ARGB8888
  | RGBA8888
  | ABGR8888
  | BGRA8888
  | ARGB2101010
  | YV12
  | IYUV
  | YUY2
  | UYVY
  | YVYU
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber PixelFormat Word32 where
  fromNumber n' = case n' of
    Raw.SDL_PIXELFORMAT_UNKNOWN -> Unknown
    Raw.SDL_PIXELFORMAT_INDEX1LSB -> Index1LSB
    Raw.SDL_PIXELFORMAT_INDEX1MSB -> Index1MSB
    Raw.SDL_PIXELFORMAT_INDEX4LSB -> Index4LSB
    Raw.SDL_PIXELFORMAT_INDEX4MSB -> Index4MSB
    Raw.SDL_PIXELFORMAT_INDEX8 -> Index8
    Raw.SDL_PIXELFORMAT_RGB332 -> RGB332
    Raw.SDL_PIXELFORMAT_RGB444 -> RGB444
    Raw.SDL_PIXELFORMAT_RGB555 -> RGB555
    Raw.SDL_PIXELFORMAT_BGR555 -> BGR555
    Raw.SDL_PIXELFORMAT_ARGB4444 -> ARGB4444
    Raw.SDL_PIXELFORMAT_RGBA4444 -> RGBA4444
    Raw.SDL_PIXELFORMAT_ABGR4444 -> ABGR4444
    Raw.SDL_PIXELFORMAT_BGRA4444 -> BGRA4444
    Raw.SDL_PIXELFORMAT_ARGB1555 -> ARGB1555
    Raw.SDL_PIXELFORMAT_RGBA5551 -> RGBA5551
    Raw.SDL_PIXELFORMAT_ABGR1555 -> ABGR1555
    Raw.SDL_PIXELFORMAT_BGRA5551 -> BGRA5551
    Raw.SDL_PIXELFORMAT_RGB565 -> RGB565
    Raw.SDL_PIXELFORMAT_BGR565 -> BGR565
    Raw.SDL_PIXELFORMAT_RGB24 -> RGB24
    Raw.SDL_PIXELFORMAT_BGR24 -> BGR24
    Raw.SDL_PIXELFORMAT_RGB888 -> RGB888
    Raw.SDL_PIXELFORMAT_RGBX8888 -> RGBX8888
    Raw.SDL_PIXELFORMAT_BGR888 -> BGR888
    Raw.SDL_PIXELFORMAT_BGRX8888 -> BGRX8888
    Raw.SDL_PIXELFORMAT_ARGB8888 -> ARGB8888
    Raw.SDL_PIXELFORMAT_RGBA8888 -> RGBA8888
    Raw.SDL_PIXELFORMAT_ABGR8888 -> ABGR8888
    Raw.SDL_PIXELFORMAT_BGRA8888 -> BGRA8888
    Raw.SDL_PIXELFORMAT_ARGB2101010 -> ARGB2101010
    Raw.SDL_PIXELFORMAT_YV12 -> YV12
    Raw.SDL_PIXELFORMAT_IYUV -> IYUV
    Raw.SDL_PIXELFORMAT_YUY2 -> YUY2
    Raw.SDL_PIXELFORMAT_UYVY -> UYVY
    Raw.SDL_PIXELFORMAT_YVYU -> YVYU
    _ -> error "fromNumber: not numbered"

instance ToNumber PixelFormat Word32 where
  toNumber pf = case pf of
    Unknown -> Raw.SDL_PIXELFORMAT_UNKNOWN
    Index1LSB -> Raw.SDL_PIXELFORMAT_INDEX1LSB
    Index1MSB -> Raw.SDL_PIXELFORMAT_INDEX1MSB
    Index4LSB -> Raw.SDL_PIXELFORMAT_INDEX4LSB
    Index4MSB -> Raw.SDL_PIXELFORMAT_INDEX4MSB
    Index8 -> Raw.SDL_PIXELFORMAT_INDEX8
    RGB332 -> Raw.SDL_PIXELFORMAT_RGB332
    RGB444 -> Raw.SDL_PIXELFORMAT_RGB444
    RGB555 -> Raw.SDL_PIXELFORMAT_RGB555
    BGR555 -> Raw.SDL_PIXELFORMAT_BGR555
    ARGB4444 -> Raw.SDL_PIXELFORMAT_ARGB4444
    RGBA4444 -> Raw.SDL_PIXELFORMAT_RGBA4444
    ABGR4444 -> Raw.SDL_PIXELFORMAT_ABGR4444
    BGRA4444 -> Raw.SDL_PIXELFORMAT_BGRA4444
    ARGB1555 -> Raw.SDL_PIXELFORMAT_ARGB1555
    RGBA5551 -> Raw.SDL_PIXELFORMAT_RGBA5551
    ABGR1555 -> Raw.SDL_PIXELFORMAT_ABGR1555
    BGRA5551 -> Raw.SDL_PIXELFORMAT_BGRA5551
    RGB565 -> Raw.SDL_PIXELFORMAT_RGB565
    BGR565 -> Raw.SDL_PIXELFORMAT_BGR565
    RGB24 -> Raw.SDL_PIXELFORMAT_RGB24
    BGR24 -> Raw.SDL_PIXELFORMAT_BGR24
    RGB888 -> Raw.SDL_PIXELFORMAT_RGB888
    RGBX8888 -> Raw.SDL_PIXELFORMAT_RGBX8888
    BGR888 -> Raw.SDL_PIXELFORMAT_BGR888
    BGRX8888 -> Raw.SDL_PIXELFORMAT_BGRX8888
    ARGB8888 -> Raw.SDL_PIXELFORMAT_ARGB8888
    RGBA8888 -> Raw.SDL_PIXELFORMAT_RGBA8888
    ABGR8888 -> Raw.SDL_PIXELFORMAT_ABGR8888
    BGRA8888 -> Raw.SDL_PIXELFORMAT_BGRA8888
    ARGB2101010 -> Raw.SDL_PIXELFORMAT_ARGB2101010
    YV12 -> Raw.SDL_PIXELFORMAT_YV12
    IYUV -> Raw.SDL_PIXELFORMAT_IYUV
    YUY2 -> Raw.SDL_PIXELFORMAT_YUY2
    UYVY -> Raw.SDL_PIXELFORMAT_UYVY
    YVYU -> Raw.SDL_PIXELFORMAT_YVYU

-- | The configuration data used when creating windows.
data RendererConfig = RendererConfig
  { rendererSoftware      :: Bool
    -- ^ The renderer is a software fallback
  , rendererAccelerated   :: Bool
    -- ^ The renderer uses hardware acceleration
  , rendererPresentVSync  :: Bool
    -- ^ Present is synchronized with the refresh rate
  , rendererTargetTexture :: Bool
    -- ^ The renderer supports rendering to texture
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

instance FromNumber RendererConfig Word32 where
  fromNumber n = RendererConfig
    { rendererSoftware      = n .&. Raw.SDL_RENDERER_SOFTWARE /= 0
    , rendererAccelerated   = n .&. Raw.SDL_RENDERER_ACCELERATED /= 0
    , rendererPresentVSync  = n .&. Raw.SDL_RENDERER_PRESENTVSYNC /= 0
    , rendererTargetTexture = n .&. Raw.SDL_RENDERER_TARGETTEXTURE /= 0
    }

instance ToNumber RendererConfig Word32 where
  toNumber config = foldr (.|.) 0
    [ if rendererSoftware config then Raw.SDL_RENDERER_SOFTWARE else 0
    , if rendererAccelerated config then Raw.SDL_RENDERER_ACCELERATED else 0
    , if rendererPresentVSync config then Raw.SDL_RENDERER_PRESENTVSYNC else 0
    , if rendererTargetTexture config then Raw.SDL_RENDERER_TARGETTEXTURE else 0
    ]

-- | Default options for 'RendererConfig'.
--
-- @
-- 'defaultRenderer' = 'RendererConfig'
--   { 'rendererSoftware'      = False
--   , 'rendererAccelerated'   = True
--   , 'rendererPresentVSync'  = False
--   , 'rendererTargetTexture' = False
--   }
-- @
defaultRenderer :: RendererConfig
defaultRenderer = RendererConfig
  { rendererSoftware      = False
  , rendererAccelerated   = True
  , rendererPresentVSync  = False
  , rendererTargetTexture = False
  }

-- | Information about an instantiated 'Renderer'.
data RendererInfo = RendererInfo
  { rendererInfoName              :: Text
    -- ^ The name of the renderer
  , rendererInfoFlags             :: RendererConfig
    -- ^ Supported renderer features
  , rendererInfoNumTextureFormats :: Word32
    -- ^ The number of available texture formats
  , rendererInfoTextureFormats    :: [PixelFormat]
    -- ^ The available texture formats
  , rendererInfoMaxTextureWidth   :: CInt
    -- ^ The maximum texture width
  , rendererInfoMaxTextureHeight  :: CInt
    -- ^ The maximum texture height
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

fromRawRendererInfo :: MonadIO m => Raw.RendererInfo -> m RendererInfo
fromRawRendererInfo (Raw.RendererInfo name flgs ntf tfs mtw mth) = liftIO $ do
    name' <- Text.decodeUtf8 <$> BS.packCString name
    return $ RendererInfo name' (fromNumber flgs) ntf (fmap fromNumber tfs) mtw mth

-- | Get information about a rendering context.
--
-- See @<https://wiki.libsdl.org/SDL_GetRendererInfo SDL_GetRendererInfo>@ for C documentation.
getRendererInfo :: MonadIO m => Renderer -> m RendererInfo
getRendererInfo (Renderer renderer) = liftIO $
  alloca $ \rptr -> do
    throwIfNeg_ "getRendererInfo" "SDL_GetRendererInfo" $
      Raw.getRendererInfo renderer rptr
    peek rptr >>= fromRawRendererInfo

-- | Enumerate all known render drivers on the system, and determine their supported features.
--
-- See @<https://wiki.libsdl.org/SDL_GetRenderDriverInfo SDL_GetRenderDriverInfo>@ for C documentation.
getRenderDriverInfo :: MonadIO m => m [RendererInfo]
getRenderDriverInfo = liftIO $ do
  count <- Raw.getNumRenderDrivers
  traverse go [0..count-1]
  where
    go idx = alloca $ \rptr -> do
               throwIfNeg_ "getRenderDriverInfo" "SDL_GetRenderDriverInfo" $
                 Raw.getRenderDriverInfo idx rptr
               peek rptr >>= fromRawRendererInfo

-- | Get or set the additional alpha value multiplied into render copy operations.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetTextureAlphaMod SDL_SetTextureAlphaMod>@ and @<https://wiki.libsdl.org/SDL_GetTextureAlphaMod SDL_GetTextureAlphaMod>@ for C documentation.
textureAlphaMod :: Texture -> StateVar Word8
textureAlphaMod (Texture t) = makeStateVar getTextureAlphaMod setTextureAlphaMod
  where
  getTextureAlphaMod = liftIO $
    alloca $ \x -> do
      throwIfNeg_ "SDL.Video.Renderer.getTextureAlphaMod" "SDL_GetTextureAlphaMod" $
        Raw.getTextureAlphaMod t x
      peek x

  setTextureAlphaMod alpha =
    throwIfNeg_ "SDL.Video.Renderer.setTextureAlphaMod" "SDL_SetTextureAlphaMod" $
    Raw.setTextureAlphaMod t alpha

-- | Get or set the blend mode used for texture copy operations.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetTextureBlendMode SDL_SetTextureBlendMode>@ and @<https://wiki.libsdl.org/SDL_GetTextureBlendMode SDL_GetTextureBlendMode>@ for C documentation.
textureBlendMode :: Texture -> StateVar BlendMode
textureBlendMode (Texture t) = makeStateVar getTextureBlendMode setTextureBlendMode
  where
  getTextureBlendMode = liftIO $
    alloca $ \x -> do
      throwIfNeg_ "SDL.Video.Renderer.getTextureBlendMode" "SDL_GetTextureBlendMode" $
        Raw.getTextureBlendMode t x
      fromNumber <$> peek x

  setTextureBlendMode bm =
    throwIfNeg_ "SDL.Video.Renderer.setTextureBlendMode" "SDL_SetTextureBlendMode" $
    Raw.setTextureBlendMode t (toNumber bm)

-- | Get or set the blend mode used for blit operations.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetSurfaceBlendMode SDL_SetSurfaceBlendMode>@ and @<https://wiki.libsdl.org/SDL_GetSurfaceBlendMode SDL_GetSurfaceBlendMode>@ for C documentation.
surfaceBlendMode :: Surface -> StateVar BlendMode
surfaceBlendMode (Surface s _) = makeStateVar getSurfaceBlendMode setSurfaceBlendMode
  where
  getSurfaceBlendMode = liftIO $
    alloca $ \x -> do
      throwIfNeg_ "SDL.Video.Renderer.getSurfaceBlendMode" "SDL_GetSurfaceBlendMode" $
        Raw.getSurfaceBlendMode s x
      fromNumber <$> peek x

  setSurfaceBlendMode bm =
    throwIfNeg_ "SDL.Video.Renderer.setSurfaceBlendMode" "SDL_SetSurfaceBlendMode" $
    Raw.setSurfaceBlendMode s (toNumber bm)

-- | Get or set the current render target. 'Nothing' corresponds to the default render target.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_SetRenderTarget SDL_SetRenderTarget>@ and @<https://wiki.libsdl.org/SDL_GetRenderTarget SDL_GetRenderTarget>@ for C documentation.
renderTarget :: Renderer -> StateVar (Maybe Texture)
renderTarget (Renderer r) = makeStateVar getRenderTarget setRenderTarget
  where
  getRenderTarget = do
    t <- Raw.getRenderTarget r
    return $
      if t == nullPtr
        then Nothing
        else Just (Texture t)

  setRenderTarget texture =
    throwIfNeg_ "SDL.Video.Renderer.setRenderTarget" "SDL_SetRenderTarget" $
    case texture of
      Nothing -> Raw.setRenderTarget r nullPtr
      Just (Texture t) -> Raw.setRenderTarget r t

-- | Get or set the device independent resolution for rendering.
--
-- This 'StateVar' can be modified using '$=' and the current value retrieved with 'get'.
--
-- See @<https://wiki.libsdl.org/SDL_RenderSetLogicalSize SDL_RenderSetLogicalSize>@ and @<https://wiki.libsdl.org/SDL_RenderGetLogicalSize SDL_RenderGetLogicalSize>@ for C documentation.
renderLogicalSize :: Renderer -> StateVar (Maybe (V2 CInt))
renderLogicalSize (Renderer r) = makeStateVar renderGetLogicalSize renderSetLogicalSize
  where
  renderGetLogicalSize = liftIO $
    alloca $ \w -> do
    alloca $ \h -> do
      Raw.renderGetLogicalSize r w h
      v <- V2 <$> peek w <*> peek h
      return $ if v == 0 then Nothing else Just v

  renderSetLogicalSize v =
    throwIfNeg_ "SDL.Video.renderSetLogicalSize" "SDL_RenderSetLogicalSize" $ do
      let (x,y) = case v of Just (V2 x y) -> (x, y)
                            Nothing -> (0,0)
      Raw.renderSetLogicalSize r x y

-- | Determine whether a window supports the use of render targets.
--
-- See @<https://wiki.libsdl.org/SDL_RenderTargetSupported SDL_RenderTargetSupported>@ for C documentation.
renderTargetSupported :: (MonadIO m) => Renderer -> m Bool
renderTargetSupported (Renderer r) = Raw.renderTargetSupported r
