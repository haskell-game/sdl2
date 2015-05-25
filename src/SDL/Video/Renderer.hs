{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , setColorKey

  , getRenderDrawBlendMode
  , setRenderDrawBlendMode

  , getRenderDrawColor
  , setRenderDrawColor

  , getRenderTarget
  , setRenderTarget

  , getTextureAlphaMod
  , setTextureAlphaMod

  , getTextureBlendMode
  , setTextureBlendMode

  , getSurfaceBlendMode
  , setSurfaceBlendMode

  , getTextureColorMod
  , setTextureColorMod

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

  , renderGetClipRect
  , renderSetClipRect

  , renderGetLogicalSize
  , renderSetLogicalSize

  , renderGetScale
  , renderSetScale

  , renderGetViewport
  , renderSetViewport

  -- * Utilities
  , RendererConfig(..)
  , defaultRenderer
  , RendererInfo(..)
  , getRendererInfo
  , getRenderDriverInfo
  ) where

import Prelude hiding (foldr)

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

blitSurface :: MonadIO m => Surface -> Maybe (Rectangle CInt) -> Surface -> Maybe (Rectangle CInt) -> m ()
blitSurface (Surface src _) srcRect (Surface dst _) dstRect = liftIO $
  throwIfNeg_ "SDL.Video.blitSurface" "SDL_BlitSurface" $
  maybeWith with srcRect $ \srcPtr ->
  maybeWith with dstRect $ \dstPtr ->
  Raw.blitSurface src (castPtr srcPtr) dst (castPtr dstPtr)

createTexture :: (Functor m, MonadIO m) => Renderer -> PixelFormat -> TextureAccess -> V2 CInt -> m Texture
createTexture (Renderer r) fmt access (V2 w h) =
  fmap Texture $
  throwIfNull "SDL.Video.Renderer.createTexture" "SDL_CreateTexture" $
  Raw.createTexture r (toNumber fmt) (toNumber access) w h

createTextureFromSurface :: (Functor m, MonadIO m) => Renderer -> Surface -> m Texture
createTextureFromSurface (Renderer r) (Surface s _) =
  fmap Texture $
  throwIfNull "SDL.Video.createTextureFromSurface" "SDL_CreateTextureFromSurface" $
  Raw.createTextureFromSurface r s

glBindTexture :: (Functor m, MonadIO m) => Texture -> m ()
glBindTexture (Texture t) =
  throwIfNeg_ "SDL.Video.Renderer.glBindTexture" "SDL_GL_BindTexture" $
  Raw.glBindTexture t nullPtr nullPtr

glUnbindTexture :: (Functor m, MonadIO m) => Texture -> m ()
glUnbindTexture (Texture t) =
  throwIfNeg_ "SDL.Video.Renderer.glUnindTexture" "SDL_GL_UnbindTexture" $
  Raw.glUnbindTexture t

destroyTexture :: MonadIO m => Texture -> m ()
destroyTexture (Texture t) = Raw.destroyTexture t

lockTexture :: MonadIO m => Texture -> Maybe (Rectangle CInt) -> m (Ptr (), CInt)
lockTexture (Texture t) rect = liftIO $
  alloca $ \pixelsPtr ->
  alloca $ \pitchPtr ->
  maybeWith with rect $ \rectPtr -> do
    throwIfNeg_ "lockTexture" "SDL_LockTexture" $
      Raw.lockTexture t (castPtr rectPtr) pixelsPtr pitchPtr
    pixels <- peek pixelsPtr
    pitch <- peek pitchPtr
    return (pixels, pitch)

unlockTexture :: MonadIO m => Texture -> m ()
unlockTexture (Texture t) = liftIO $ Raw.unlockTexture t

lockSurface :: MonadIO m => Surface -> m ()
lockSurface (Surface s _) = liftIO $
  throwIfNeg_ "lockSurface" "SDL_LockSurface" $
    Raw.lockSurface s

unlockSurface :: MonadIO m => Surface -> m ()
unlockSurface (Surface s _) = Raw.unlockSurface s

data TextureAccess
  = TextureAccessStatic
  | TextureAccessStreaming
  | TextureAccessTarget
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
  , textureAccess      :: TextureAccess
  , textureWidth       :: CInt
  , textureHeight      :: CInt
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

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

createRGBSurface :: (Functor m, MonadIO m) => V2 CInt -> CInt -> V4 Word32 -> m Surface
createRGBSurface (V2 w h) d (V4 r g b a) =
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.createRGBSurface" "SDL_CreateRGBSurface" $
  Raw.createRGBSurface 0 w h d r g b a

createRGBSurfaceFrom :: (Functor m, MonadIO m) => MSV.IOVector Word8 -> V2 CInt -> CInt -> CInt -> V4 Word32 -> m Surface
createRGBSurfaceFrom pixels (V2 w h) d p (V4 r g b a) = liftIO $
  fmap (managedSurface pixels) $
  throwIfNull "SDL.Video.createRGBSurfaceFrom" "SDL_CreateRGBSurfaceFrom" $
    MSV.unsafeWith pixels $ \pixelPtr ->
      Raw.createRGBSurfaceFrom (castPtr pixelPtr) w h d p r g b a

fillRect :: MonadIO m => Surface -> Maybe (Rectangle CInt) -> Word32 -> m ()
fillRect (Surface s _) rect col = liftIO $
  throwIfNeg_ "SDL.Video.fillRect" "SDL_FillRect" $
  maybeWith with rect $ \rectPtr ->
  Raw.fillRect s (castPtr rectPtr) col

fillRects :: MonadIO m => Surface -> SV.Vector (Rectangle CInt) -> Word32 -> m ()
fillRects (Surface s _) rects col = liftIO $ do
  throwIfNeg_ "SDL.Video.fillRects" "SDL_FillRects" $
    SV.unsafeWith rects $ \rp ->
      Raw.fillRects s
                    (castPtr rp)
                    (fromIntegral (SV.length rects))
                    col

freeSurface :: MonadIO m => Surface -> m ()
freeSurface (Surface s _) = Raw.freeSurface s

loadBMP :: MonadIO m => FilePath -> m Surface
loadBMP filePath = liftIO $
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.loadBMP" "SDL_LoadBMP" $
  withCString filePath $ Raw.loadBMP

newtype SurfacePixelFormat = SurfacePixelFormat (Ptr Raw.PixelFormat)
  deriving (Eq, Typeable)

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. De need to guarantee that pointers aren't reused?
mapRGB :: MonadIO m => SurfacePixelFormat -> V3 Word8 -> m Word32
mapRGB (SurfacePixelFormat fmt) (V3 r g b) = Raw.mapRGB fmt r g b

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. surface->{w,h} are immutable, but do we need to guarantee that pointers
-- aren't reused by *different* surfaces?
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

setPaletteColors :: MonadIO m => Palette -> (SV.Vector (V4 Word8)) -> CInt -> m ()
setPaletteColors (Palette p) colors first = liftIO $
  throwIfNeg_ "SDL.Video.setPaletteColors" "SDL_SetPaletteColors" $
  SV.unsafeWith colors $ \cp ->
    Raw.setPaletteColors p (castPtr cp) first n
  where
    n = fromIntegral $ SV.length colors

getWindowSurface :: (Functor m, MonadIO m) => Window -> m Surface
getWindowSurface (Window w) =
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.getWindowSurface" "SDL_GetWindowSurface" $
  Raw.getWindowSurface w

getRenderDrawBlendMode :: (Functor m, MonadIO m) => Renderer -> m BlendMode
getRenderDrawBlendMode (Renderer r) = liftIO $
  alloca $ \bmPtr -> do
    throwIfNeg_ "SDL.Video.Renderer.getRenderDrawBlendMode" "SDL_GetRenderDrawBlendMode" $
      Raw.getRenderDrawBlendMode r bmPtr
    fromNumber <$> peek bmPtr

setRenderDrawBlendMode :: (Functor m, MonadIO m) => Renderer -> BlendMode -> m ()
setRenderDrawBlendMode (Renderer r) bm =
  throwIfNeg_ "SDL.Video.Renderer.setRenderDrawBlendMode" "SDL_SetRenderDrawBlendMode" $
  Raw.setRenderDrawBlendMode r (toNumber bm)

getRenderDrawColor :: (MonadIO m) => Renderer -> m (V4 Word8)
getRenderDrawColor (Renderer re) = liftIO $
  alloca $ \r ->
  alloca $ \g ->
  alloca $ \b ->
  alloca $ \a -> do
    throwIfNeg_ "SDL.Video.Renderer.getRenderDrawColor" "SDL_GetRenderDrawColor" $
      Raw.getRenderDrawColor re r g b a
    V4 <$> peek r <*> peek g <*> peek b <*> peek a

setRenderDrawColor :: (Functor m, MonadIO m) => Renderer -> V4 Word8 -> m ()
setRenderDrawColor (Renderer re) (V4 r g b a) =
  throwIfNeg_ "SDL.Video.setRenderDrawColor" "SDL_SetRenderDrawColor" $
  Raw.setRenderDrawColor re r g b a

updateWindowSurface :: (Functor m, MonadIO m) => Window -> m ()
updateWindowSurface (Window w) =
  throwIfNeg_ "SDL.Video.updateWindowSurface" "SDL_UpdateWindowSurface" $
    Raw.updateWindowSurface w

data BlendMode
  = BlendNone
  | BlendAlphaBlend
  | BlendAdditive
  | BlendMod
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

renderDrawRect :: MonadIO m => Renderer -> Rectangle CInt -> m ()
renderDrawRect (Renderer r) rect = liftIO $
  throwIfNeg_ "SDL.Video.renderDrawRect" "SDL_RenderDrawRect" $
  with rect (Raw.renderDrawRect r . castPtr)

renderDrawRects :: MonadIO m => Renderer -> SV.Vector (Rectangle CInt) -> m ()
renderDrawRects (Renderer r) rects = liftIO $
  throwIfNeg_ "SDL.Video.renderDrawRects" "SDL_RenderDrawRects" $
  SV.unsafeWith rects $ \rp ->
    Raw.renderDrawRects r
                        (castPtr rp)
                        (fromIntegral (SV.length rects))

renderFillRect :: MonadIO m => Renderer -> Maybe (Rectangle CInt) -> m ()
renderFillRect (Renderer r) rect = liftIO $ do
  throwIfNeg_ "SDL.Video.renderFillRect" "SDL_RenderFillRect" $
    maybeWith with rect $ \rPtr ->
      Raw.renderFillRect r
                         (castPtr rPtr)

renderFillRects :: MonadIO m => Renderer -> SV.Vector (Rectangle CInt) -> m ()
renderFillRects (Renderer r) rects = liftIO $
  throwIfNeg_ "SDL.Video.renderFillRects" "SDL_RenderFillRects" $
    SV.unsafeWith rects $ \rp ->
      Raw.renderFillRects r
                          (castPtr rp)
                          (fromIntegral (SV.length rects))

renderClear :: (Functor m, MonadIO m) => Renderer -> m ()
renderClear (Renderer r) =
  throwIfNeg_ "SDL.Video.renderClear" "SDL_RenderClear" $
  Raw.renderClear r

renderSetScale :: (Functor m, MonadIO m) => Renderer -> V2 CFloat -> m ()
renderSetScale (Renderer r) (V2 x y) =
  throwIfNeg_ "SDL.Video.renderSetScale" "SDL_RenderSetScale" $
  Raw.renderSetScale r x y

renderGetScale :: (MonadIO m) => Renderer -> m (V2 CFloat)
renderGetScale (Renderer r) = liftIO $
  alloca $ \w ->
  alloca $ \h -> do
    Raw.renderGetScale r w h
    V2 <$> peek w <*> peek h

renderSetLogicalSize :: (Functor m, MonadIO m) => Renderer -> V2 CInt -> m ()
renderSetLogicalSize (Renderer r) (V2 x y) =
  throwIfNeg_ "SDL.Video.renderSetLogicalSize" "SDL_RenderSetLogicalSize" $
  Raw.renderSetLogicalSize r x y

renderSetClipRect :: MonadIO m => Renderer -> Maybe (Rectangle CInt) -> m ()
renderSetClipRect (Renderer r) rect =
  liftIO $
  throwIfNeg_ "SDL.Video.renderSetClipRect" "SDL_RenderSetClipRect" $
  maybeWith with rect $ Raw.renderSetClipRect r . castPtr

renderGetViewport :: MonadIO m => Renderer -> m (Rectangle CInt)
renderGetViewport (Renderer r) = liftIO $
  alloca $ \rect -> do
    Raw.renderGetViewport r rect
    peek (castPtr rect)

renderSetViewport :: MonadIO m => Renderer -> Maybe (Rectangle CInt) -> m ()
renderSetViewport (Renderer r) rect =
  liftIO $
  throwIfNeg_ "SDL.Video.renderSetViewport" "SDL_RenderSetViewport" $
  maybeWith with rect $ Raw.renderSetViewport r . castPtr

renderPresent :: MonadIO m => Renderer -> m ()
renderPresent (Renderer r) = Raw.renderPresent r

renderCopy :: MonadIO m => Renderer -> Texture -> Maybe (Rectangle CInt) -> Maybe (Rectangle CInt) -> m ()
renderCopy (Renderer r) (Texture t) srcRect dstRect =
  liftIO $
  throwIfNeg_ "SDL.Video.renderCopy" "SDL_RenderCopy" $
  maybeWith with srcRect $ \src ->
  maybeWith with dstRect $ \dst ->
  Raw.renderCopy r t (castPtr src) (castPtr dst)

renderCopyEx :: MonadIO m => Renderer -> Texture -> Maybe (Rectangle CInt) -> Maybe (Rectangle CInt) -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
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

renderDrawLine :: (Functor m, MonadIO m) => Renderer -> Point V2 CInt -> Point V2 CInt -> m ()
renderDrawLine (Renderer r) (P (V2 x y)) (P (V2 x' y')) =
  throwIfNeg_ "SDL.Video.renderDrawLine" "SDL_RenderDrawLine" $
  Raw.renderDrawLine r x y x' y'

renderDrawLines :: MonadIO m => Renderer -> SV.Vector (Point V2 CInt) -> m ()
renderDrawLines (Renderer r) points =
  liftIO $
  throwIfNeg_ "SDL.Video.renderDrawLines" "SDL_RenderDrawLines" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawLines r
                        (castPtr cp)
                        (fromIntegral (SV.length points))

renderDrawPoint :: (Functor m, MonadIO m) => Renderer -> Point V2 CInt -> m ()
renderDrawPoint (Renderer r) (P (V2 x y)) =
  throwIfNeg_ "SDL.Video.renderDrawPoint" "SDL_RenderDrawPoint" $
  Raw.renderDrawPoint r x y

renderDrawPoints :: MonadIO m => Renderer -> SV.Vector (Point V2 CInt) -> m ()
renderDrawPoints (Renderer r) points =
  liftIO $
  throwIfNeg_ "SDL.Video.renderDrawPoints" "SDL_RenderDrawPoints" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawPoints r
                         (castPtr cp)
                         (fromIntegral (SV.length points))

convertSurface :: (Functor m, MonadIO m) => Surface -> SurfacePixelFormat -> m Surface
convertSurface (Surface s _) (SurfacePixelFormat fmt) =
  fmap unmanagedSurface $
  throwIfNull "SDL.Video.Renderer.convertSurface" "SDL_ConvertSurface" $
  Raw.convertSurface s fmt 0

blitScaled :: MonadIO m => Surface -> Maybe (Rectangle CInt) -> Surface -> Maybe (Rectangle CInt) -> m ()
blitScaled (Surface src _) srcRect (Surface dst _) dstRect =
  liftIO $
  throwIfNeg_ "SDL.Video.blitSurface" "SDL_BlitSurface" $
  maybeWith with srcRect $ \srcPtr ->
  maybeWith with dstRect $ \dstPtr ->
  Raw.blitScaled src (castPtr srcPtr) dst (castPtr dstPtr)

setColorKey :: MonadIO m => Surface -> Maybe Word32 -> m ()
setColorKey (Surface s _) key =
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

getTextureColorMod :: (MonadIO m) => Texture -> m (V3 Word8)
getTextureColorMod (Texture t) = liftIO $
  alloca $ \r ->
  alloca $ \g ->
  alloca $ \b -> do
    throwIfNeg_ "SDL.Video.Renderer.getTextureColorMod" "SDL_GetTextureColorMod" $
      Raw.getTextureColorMod t r g b
    V3 <$> peek r <*> peek g <*> peek b

setTextureColorMod :: (Functor m, MonadIO m) => Texture -> V3 Word8 -> m ()
setTextureColorMod (Texture t) (V3 r g b) =
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

data RendererConfig = RendererConfig
  { rendererSoftware      :: Bool
  , rendererAccelerated   :: Bool
  , rendererPresentVSync  :: Bool
  , rendererTargetTexture :: Bool
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

defaultRenderer :: RendererConfig
defaultRenderer = RendererConfig
  { rendererSoftware      = False
  , rendererAccelerated   = True
  , rendererPresentVSync  = False
  , rendererTargetTexture = False
  }

data RendererInfo = RendererInfo
  { rendererInfoName              :: Text
  , rendererInfoFlags             :: RendererConfig
  , rendererInfoNumTextureFormats :: Word32
  , rendererInfoTextureFormats    :: [PixelFormat]
  , rendererInfoMaxTextureWidth   :: CInt
  , rendererInfoMaxTextureHeight  :: CInt
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

fromRawRendererInfo :: MonadIO m => Raw.RendererInfo -> m RendererInfo
fromRawRendererInfo (Raw.RendererInfo name flgs ntf tfs mtw mth) = liftIO $ do
    name' <- Text.decodeUtf8 <$> BS.packCString name
    return $ RendererInfo name' (fromNumber flgs) ntf (fmap fromNumber tfs) mtw mth

getRendererInfo :: MonadIO m => Renderer -> m RendererInfo
getRendererInfo (Renderer renderer) = liftIO $
  alloca $ \rptr -> do
    throwIfNeg_ "getRendererInfo" "SDL_GetRendererInfo" $
      Raw.getRendererInfo renderer rptr
    peek rptr >>= fromRawRendererInfo

getRenderDriverInfo :: MonadIO m => m [RendererInfo]
getRenderDriverInfo = liftIO $ do
  count <- Raw.getNumRenderDrivers
  traverse go [0..count-1]
  where
    go idx = alloca $ \rptr -> do
               throwIfNeg_ "getRenderDriverInfo" "SDL_GetRenderDriverInfo" $
                 Raw.getRenderDriverInfo idx rptr
               peek rptr >>= fromRawRendererInfo

getTextureAlphaMod :: (MonadIO m) => Texture -> m Word8
getTextureAlphaMod (Texture t) = liftIO $
  alloca $ \x -> do
    throwIfNeg_ "SDL.Video.Renderer.getTextureAlphaMod" "SDL_GetTextureAlphaMod" $
      Raw.getTextureAlphaMod t x
    peek x

setTextureAlphaMod :: (Functor m, MonadIO m) => Texture -> Word8 -> m ()
setTextureAlphaMod (Texture t) alpha =
  throwIfNeg_ "SDL.Video.Renderer.setTextureAlphaMod" "SDL_SetTextureAlphaMod" $
  Raw.setTextureAlphaMod t alpha

getTextureBlendMode :: (MonadIO m) => Texture -> m BlendMode
getTextureBlendMode (Texture t) = liftIO $
  alloca $ \x -> do
    throwIfNeg_ "SDL.Video.Renderer.getTextureBlendMode" "SDL_GetTextureBlendMode" $
      Raw.getTextureBlendMode t x
    fromNumber <$> peek x

setTextureBlendMode :: (Functor m, MonadIO m) => Texture -> BlendMode -> m ()
setTextureBlendMode (Texture t) bm =
  throwIfNeg_ "SDL.Video.Renderer.setTextureBlendMode" "SDL_SetTextureBlendMode" $
  Raw.setTextureBlendMode t (toNumber bm)

getSurfaceBlendMode :: (MonadIO m) => Surface -> m BlendMode
getSurfaceBlendMode (Surface s _) = liftIO $
  alloca $ \x -> do
    throwIfNeg_ "SDL.Video.Renderer.getSurfaceBlendMode" "SDL_GetSurfaceBlendMode" $
      Raw.getSurfaceBlendMode s x
    fromNumber <$> peek x

setSurfaceBlendMode :: (Functor m, MonadIO m) => Surface -> BlendMode -> m ()
setSurfaceBlendMode (Surface s _) bm =
  throwIfNeg_ "SDL.Video.Renderer.setSurfaceBlendMode" "SDL_SetSurfaceBlendMode" $
  Raw.setSurfaceBlendMode s (toNumber bm)

getRenderTarget :: (MonadIO m) => Renderer -> m (Maybe Texture)
getRenderTarget (Renderer r) = do
  t <- Raw.getRenderTarget r
  return $
    if t == nullPtr
      then Nothing
      else Just (Texture t)

setRenderTarget :: (Functor m, MonadIO m) => Renderer -> Maybe Texture -> m ()
setRenderTarget (Renderer r) texture =
  throwIfNeg_ "SDL.Video.Renderer.setRenderTarget" "SDL_SetRenderTarget" $
  case texture of
    Nothing -> Raw.setRenderTarget r nullPtr
    Just (Texture t) -> Raw.setRenderTarget r t

renderGetClipRect :: (MonadIO m) => Renderer -> m (Maybe (Rectangle CInt))
renderGetClipRect (Renderer r) = liftIO $
  alloca $ \rPtr -> do
    Raw.renderGetClipRect r rPtr
    maybePeek peek (castPtr rPtr)

renderGetLogicalSize :: (MonadIO m) => Renderer -> m (Maybe (V2 CInt))
renderGetLogicalSize (Renderer r) = liftIO $
  alloca $ \w -> do
  alloca $ \h -> do
    Raw.renderGetLogicalSize r w h
    v <- V2 <$> peek w <*> peek h
    return $ if v == 0 then Nothing else Just v

renderTargetSupported :: (MonadIO m) => Renderer -> m Bool
renderTargetSupported (Renderer r) = Raw.renderTargetSupported r
