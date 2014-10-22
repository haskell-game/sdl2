{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Video.Renderer
  ( Renderer

  -- * Drawing Primitives
  , blitScaled
  , blitSurface
  , createTextureFromSurface
  , convertSurface
  , destroyTexture
  , fillRect
  , freeSurface
  , loadBMP
  , mapRGB
  , getWindowSurface
  , setColorKey
  , setRenderDrawBlendMode
  , setRenderDrawColor
  , surfaceDimensions
  , surfaceFormat
  , updateWindowSurface
  , BlendMode(..)
  , Rectangle(..)
  , Surface
  , SurfacePixelFormat
  , Texture
  , PixelFormat(..)

  -- * Drawing Primitives
  , renderClear
  , renderCopy
  , renderDrawLine
  , renderDrawLines
  , renderDrawPoint
  , renderDrawPoints
  , renderDrawRect
  , renderDrawRects
  , renderFillRect
  , renderFillRects
  , renderPresent
  , renderSetClipRect
  , renderSetLogicalSize
  , renderSetScale
  , renderSetViewport

  -- * Utilities
  , RendererConfig(..)
  , defaultRenderer
  , RendererInfo(..)
  , getRendererInfo
  , getNumRenderDrivers
  , getRenderDriverInfo
  ) where

import Prelude hiding (foldr)

import Control.Applicative
import Data.Bits
import Data.Foldable
import Data.Text (Text)
import Data.Traversable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Linear
import Linear.Affine (Point(P))
import SDL.Exception
import SDL.Internal.Numbered
import SDL.Internal.Types

import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as SV
import qualified SDL.Raw as Raw

blitSurface :: Surface -> Maybe (Rectangle CInt) -> Surface -> Maybe (Rectangle CInt) -> IO ()
blitSurface (Surface src) srcRect (Surface dst) dstRect =
  throwIfNeg_ "SDL.Video.blitSurface" "SDL_BlitSurface" $
  maybeWith with srcRect $ \srcPtr ->
  maybeWith with dstRect $ \dstPtr ->
  Raw.blitSurface src (castPtr srcPtr) dst (castPtr dstPtr)

createTextureFromSurface :: Renderer -> Surface -> IO Texture
createTextureFromSurface (Renderer r) (Surface s) =
  fmap Texture $
  throwIfNull "SDL.Video.createTextureFromSurface" "SDL_CreateTextureFromSurface" $
  Raw.createTextureFromSurface r s

destroyTexture :: Texture -> IO ()
destroyTexture (Texture t) = Raw.destroyTexture t

fillRect :: Surface -> Maybe (Rectangle CInt) -> Word32 -> IO ()
fillRect (Surface s) rect col =
  throwIfNeg_ "SDL.Video.fillRect" "SDL_FillRect" $
  maybeWith with rect $ \rectPtr ->
  Raw.fillRect s (castPtr rectPtr) col

freeSurface :: Surface -> IO ()
freeSurface (Surface s) = Raw.freeSurface s

loadBMP :: FilePath -> IO Surface
loadBMP filePath =
  fmap Surface $
  throwIfNull "SDL.Video.loadBMP" "SDL_LoadBMP" $
  withCString filePath $ Raw.loadBMP

newtype SurfacePixelFormat = SurfacePixelFormat (Ptr Raw.PixelFormat)

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. De need to guarantee that pointers aren't reused?
mapRGB :: SurfacePixelFormat -> V3 Word8 -> IO Word32
mapRGB (SurfacePixelFormat fmt) (V3 r g b) = Raw.mapRGB fmt r g b

-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. surface->{w,h} are immutable, but do we need to guarantee that pointers
-- aren't reused by *different* surfaces?
surfaceDimensions :: Surface -> IO (V2 CInt)
surfaceDimensions (Surface s) = (V2 <$> Raw.surfaceW <*> Raw.surfaceH) <$> peek s


-- It's possible we could use unsafePerformIO here, but I'm not
-- sure. surface->format is immutable, but do we need to guarantee that pointers
-- aren't reused by *different* surfaces?
surfaceFormat :: Surface -> IO SurfacePixelFormat
surfaceFormat (Surface s) = SurfacePixelFormat . Raw.surfaceFormat <$> peek s

getWindowSurface :: Window -> IO Surface
getWindowSurface (Window w) =
  fmap Surface $
  throwIfNull "SDL.Video.getWindowSurface" "SDL_GetWindowSurface" $
  Raw.getWindowSurface w

setRenderDrawBlendMode :: Renderer -> BlendMode -> IO ()
setRenderDrawBlendMode (Renderer r) bm =
  throwIfNeg_ "SDL.Video.setRenderDrawBlendMode" "SDL_RenderDrawBlendMode" $
  Raw.setRenderDrawBlendMode r (toNumber bm)

setRenderDrawColor :: Renderer -> V4 Word8 -> IO ()
setRenderDrawColor (Renderer re) (V4 r g b a) =
  throwIfNeg_ "SDL.Video.setRenderDrawColor" "SDL_SetRenderDrawColor" $
  Raw.setRenderDrawColor re r g b a

updateWindowSurface :: Window -> IO ()
updateWindowSurface (Window w) =
  throwIfNeg_ "SDL.Video.updateWindowSurface" "SDL_UpdateWindowSurface" $
    Raw.updateWindowSurface w

data BlendMode = BlendNone | BlendAlphaBlend | BlendAdditive | BlendMod
  deriving (Eq,Show)

instance ToNumber BlendMode Word32 where
  toNumber BlendNone = Raw.blendModeNone
  toNumber BlendAlphaBlend = Raw.blendModeBlend
  toNumber BlendAdditive = Raw.blendModeAdd
  toNumber BlendMod = Raw.blendModeMod

data Rectangle a = Rectangle (Point V2 a) (V2 a)

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

newtype Surface = Surface (Ptr Raw.Surface)

newtype Texture = Texture Raw.Texture

renderDrawRect :: Renderer -> Rectangle CInt -> IO ()
renderDrawRect (Renderer r) rect =
  throwIfNeg_ "SDL.Video.renderDrawRect" "SDL_RenderDrawRect" $
  with rect (Raw.renderDrawRect r . castPtr)

renderDrawRects :: Renderer -> SV.Vector (Rectangle CInt) -> IO ()
renderDrawRects (Renderer r) rects =
  throwIfNeg_ "SDL.Video.renderDrawRects" "SDL_RenderDrawRects" $
  SV.unsafeWith rects $ \rp ->
    Raw.renderDrawRects r
                        (castPtr rp)
                        (fromIntegral (SV.length rects))

renderFillRect :: Renderer -> Maybe (Rectangle CInt) -> IO ()
renderFillRect (Renderer r) rect = do
  throwIfNeg_ "SDL.Video.renderFillRect" "SDL_RenderFillRect" $
    maybeWith with rect $ \rPtr ->
      Raw.renderFillRect r
                         (castPtr rPtr)

renderFillRects :: Renderer -> SV.Vector (Rectangle CInt) -> IO ()
renderFillRects (Renderer r) rects = do
  throwIfNeg_ "SDL.Video.renderFillRects" "SDL_RenderFillRects" $
    SV.unsafeWith rects $ \rp ->
      Raw.renderFillRects r
                          (castPtr rp)
                          (fromIntegral (SV.length rects))

renderClear :: Renderer -> IO ()
renderClear (Renderer r) =
  throwIfNeg_ "SDL.Video.renderClear" "SDL_RenderClear" $
  Raw.renderClear r

renderSetScale :: Renderer -> V2 CFloat -> IO ()
renderSetScale (Renderer r) (V2 x y) =
  throwIfNeg_ "SDL.Video.renderSetScale" "SDL_RenderSetScale" $
  Raw.renderSetScale r x y

renderSetLogicalSize :: Renderer -> V2 CInt -> IO ()
renderSetLogicalSize (Renderer r) (V2 x y) =
  throwIfNeg_ "SDL.Video.renderSetLogicalSize" "SDL_RenderSetLogicalSize" $
  Raw.renderSetLogicalSize r x y

renderSetClipRect :: Renderer -> Maybe (Rectangle CInt) -> IO ()
renderSetClipRect (Renderer r) rect =
  throwIfNeg_ "SDL.Video.renderSetClipRect" "SDL_RenderSetClipRect" $
  maybeWith with rect $ Raw.renderSetClipRect r . castPtr

renderSetViewport :: Renderer -> Maybe (Rectangle CInt) -> IO ()
renderSetViewport (Renderer r) rect =
  throwIfNeg_ "SDL.Video.renderSetViewport" "SDL_RenderSetViewport" $
  maybeWith with rect $ Raw.renderSetViewport r . castPtr

renderPresent :: Renderer -> IO ()
renderPresent (Renderer r) = Raw.renderPresent r

renderCopy :: Renderer -> Texture -> Maybe (Rectangle CInt) -> Maybe (Rectangle CInt) -> IO ()
renderCopy (Renderer r) (Texture t) srcRect dstRect =
  throwIfNeg_ "SDL.Video.renderCopy" "SDL_RenderCopy" $
  maybeWith with srcRect $ \src ->
  maybeWith with dstRect $ \dst ->
  Raw.renderCopy r t (castPtr src) (castPtr dst)

renderDrawLine :: Renderer -> Point V2 CInt -> Point V2 CInt -> IO ()
renderDrawLine (Renderer r) (P (V2 x y)) (P (V2 x' y')) =
  throwIfNeg_ "SDL.Video.renderDrawLine" "SDL_RenderDrawLine" $
  Raw.renderDrawLine r x y x' y'

renderDrawLines :: Renderer -> SV.Vector (Point V2 CInt) -> IO ()
renderDrawLines (Renderer r) points =
  throwIfNeg_ "SDL.Video.renderDrawLines" "SDL_RenderDrawLines" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawLines r
                        (castPtr cp)
                        (fromIntegral (SV.length points))

renderDrawPoint :: Renderer -> Point V2 CInt -> IO ()
renderDrawPoint (Renderer r) (P (V2 x y)) =
  throwIfNeg_ "SDL.Video.renderDrawPoint" "SDL_RenderDrawPoint" $
  Raw.renderDrawPoint r x y

renderDrawPoints :: Renderer -> SV.Vector (Point V2 CInt) -> IO ()
renderDrawPoints (Renderer r) points =
  throwIfNeg_ "SDL.Video.renderDrawPoints" "SDL_RenderDrawPoints" $
  SV.unsafeWith points $ \cp ->
    Raw.renderDrawPoints r
                         (castPtr cp)
                         (fromIntegral (SV.length points))

convertSurface :: Surface -> SurfacePixelFormat -> IO Surface
convertSurface (Surface s) (SurfacePixelFormat fmt) =
  fmap Surface $
  throwIfNull "SDL.Video.Renderer.convertSurface" "SDL_ConvertSurface" $
  Raw.convertSurface s fmt 0

blitScaled :: Surface -> Maybe (Rectangle CInt) -> Surface -> Maybe (Rectangle CInt) -> IO ()
blitScaled (Surface src) srcRect (Surface dst) dstRect =
  throwIfNeg_ "SDL.Video.blitSurface" "SDL_BlitSurface" $
  maybeWith with srcRect $ \srcPtr ->
  maybeWith with dstRect $ \dstPtr ->
  Raw.blitScaled src (castPtr srcPtr) dst (castPtr dstPtr)

setColorKey :: Surface -> Maybe Word32 -> IO ()
setColorKey (Surface s) key =
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

data PixelFormat = Unknown
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
                 deriving (Eq, Show)

instance FromNumber PixelFormat Word32 where
  fromNumber n' = case n' of
    n | n == Raw.pixelFormatUnknown -> Unknown
    n | n == Raw.pixelFormatIndex1LSB -> Index1LSB
    n | n == Raw.pixelFormatIndex1MSB -> Index1MSB
    n | n == Raw.pixelFormatIndex4LSB -> Index4LSB
    n | n == Raw.pixelFormatIndex4MSB -> Index4MSB
    n | n == Raw.pixelFormatIndex8 -> Index8
    n | n == Raw.pixelFormatRGB332 -> RGB332
    n | n == Raw.pixelFormatRGB444 -> RGB444
    n | n == Raw.pixelFormatRGB555 -> RGB555
    n | n == Raw.pixelFormatBGR555 -> BGR555
    n | n == Raw.pixelFormatARGB4444 -> ARGB4444
    n | n == Raw.pixelFormatRGBA4444 -> RGBA4444
    n | n == Raw.pixelFormatABGR4444 -> ABGR4444
    n | n == Raw.pixelFormatBGRA4444 -> BGRA4444
    n | n == Raw.pixelFormatARGB1555 -> ARGB1555
    n | n == Raw.pixelFormatRGBA5551 -> RGBA5551
    n | n == Raw.pixelFormatABGR1555 -> ABGR1555
    n | n == Raw.pixelFormatBGRA5551 -> BGRA5551
    n | n == Raw.pixelFormatRGB565 -> RGB565
    n | n == Raw.pixelFormatBGR565 -> BGR565
    n | n == Raw.pixelFormatRGB24 -> RGB24
    n | n == Raw.pixelFormatBGR24 -> BGR24
    n | n == Raw.pixelFormatRGB888 -> RGB888
    n | n == Raw.pixelFormatRGBX8888 -> RGBX8888
    n | n == Raw.pixelFormatBGR888 -> BGR888
    n | n == Raw.pixelFormatBGRX8888 -> BGRX8888
    n | n == Raw.pixelFormatARGB8888 -> ARGB8888
    n | n == Raw.pixelFormatRGBA8888 -> RGBA8888
    n | n == Raw.pixelFormatABGR8888 -> ABGR8888
    n | n == Raw.pixelFormatBGRA8888 -> BGRA8888
    n | n == Raw.pixelFormatARGB2101010 -> ARGB2101010
    n | n == Raw.pixelFormatYV12 -> YV12
    n | n == Raw.pixelFormatIYUV -> IYUV
    n | n == Raw.pixelFormatYUY2 -> YUY2
    n | n == Raw.pixelFormatUYVY -> UYVY
    n | n == Raw.pixelFormatYVYU -> YVYU
    _ -> error "fromNumber: not numbered"

data RendererConfig = RendererConfig
  { rendererSoftware      :: Bool
  , rendererAccelerated   :: Bool
  , rendererPresentVSync  :: Bool
  , rendererTargetTexture :: Bool
  } deriving (Eq, Show)

instance FromNumber RendererConfig Word32 where
  fromNumber n = RendererConfig
    { rendererSoftware      = n .&. Raw.rendererFlagSoftware /= 0
    , rendererAccelerated   = n .&. Raw.rendererFlagAccelerated /= 0
    , rendererPresentVSync  = n .&. Raw.rendererFlagPresentVSync /= 0
    , rendererTargetTexture = n .&. Raw.rendererFlagTargetTexture /= 0
    }

instance ToNumber RendererConfig Word32 where
  toNumber config = foldr (.|.) 0
    [ if rendererSoftware config then Raw.rendererFlagSoftware else 0
    , if rendererAccelerated config then Raw.rendererFlagAccelerated else 0
    , if rendererPresentVSync config then Raw.rendererFlagPresentVSync else 0
    , if rendererTargetTexture config then Raw.rendererFlagTargetTexture else 0
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
  } deriving (Eq, Show)

fromRawRendererInfo :: Raw.RendererInfo -> IO RendererInfo
fromRawRendererInfo (Raw.RendererInfo name flgs ntf tfs mtw mth) = do
    name' <- Text.decodeUtf8 <$> BS.packCString name
    return $ RendererInfo name' (fromNumber flgs) ntf (fmap fromNumber tfs) mtw mth

getRendererInfo :: Renderer -> IO RendererInfo
getRendererInfo (Renderer renderer) =
  alloca $ \rptr -> do
    throwIfNeg_ "getRendererInfo" "SDL_GetRendererInfo" $
      Raw.getRendererInfo renderer rptr
    peek rptr >>= fromRawRendererInfo

getNumRenderDrivers :: IO CInt
getNumRenderDrivers = Raw.getNumRenderDrivers

getRenderDriverInfo :: IO [RendererInfo]
getRenderDriverInfo = do
  count <- Raw.getNumRenderDrivers
  traverse go [0..count-1]
  where
    go idx = alloca $ \rptr -> do
               throwIfNeg_ "getRenderDriverInfo" "SDL_GetRenderDriverInfo" $
                 Raw.getRenderDriverInfo idx rptr
               peek rptr >>= fromRawRendererInfo
