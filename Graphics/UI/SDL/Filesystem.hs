module Graphics.UI.SDL.Filesystem (
  -- * Filesystem Paths
  getBasePath,
  getPrefPath,

  -- * File I/O Abstraction
  allocRW,
  freeRW,
  rwFromConstMem,
  rwFromFP,
  rwFromFile,
  rwFromMem,
  rwClose,
  rwRead,
  rwSeek,
  rwTell,
  rwWrite,
  readBE16,
  readBE32,
  readBE64,
  readLE16,
  readLE32,
  readLE64,
  writeBE16,
  writeBE32,
  writeBE64,
  writeLE16,
  writeLE32,
  writeLE64
) where

import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_GetBasePath" getBasePath' :: IO CString
foreign import ccall "SDL.h SDL_GetPrefPath" getPrefPath' :: CString -> CString -> IO CString

foreign import ccall "SDL.h SDL_AllocRW" allocRW' :: IO (Ptr RWops)
foreign import ccall "SDL.h SDL_FreeRW" freeRW' :: Ptr RWops -> IO ()
foreign import ccall "SDL.h SDL_RWFromConstMem" rwFromConstMem' :: Ptr () -> CInt -> IO (Ptr RWops)
foreign import ccall "SDL.h SDL_RWFromFP" rwFromFP' :: Ptr () -> Bool -> IO (Ptr RWops)
foreign import ccall "SDL.h SDL_RWFromFile" rwFromFile' :: CString -> CString -> IO (Ptr RWops)
foreign import ccall "SDL.h SDL_RWFromMem" rwFromMem' :: Ptr () -> CInt -> IO (Ptr RWops)
foreign import ccall "sdlhelper.h SDLHelper_RWclose" rwClose' :: Ptr RWops -> IO CInt
foreign import ccall "sdlhelper.h SDLHelper_RWread" rwRead' :: Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize
foreign import ccall "sdlhelper.h SDLHelper_RWseek" rwSeek' :: Ptr RWops -> Int64 -> CInt -> IO Int64
foreign import ccall "sdlhelper.h SDLHelper_RWtell" rwTell' :: Ptr RWops -> IO Int64
foreign import ccall "sdlhelper.h SDLHelper_RWwrite" rwWrite' :: Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize
foreign import ccall "SDL.h SDL_ReadBE16" readBE16' :: Ptr RWops -> IO Word16
foreign import ccall "SDL.h SDL_ReadBE32" readBE32' :: Ptr RWops -> IO Word32
foreign import ccall "SDL.h SDL_ReadBE64" readBE64' :: Ptr RWops -> IO Word64
foreign import ccall "SDL.h SDL_ReadLE16" readLE16' :: Ptr RWops -> IO Word16
foreign import ccall "SDL.h SDL_ReadLE32" readLE32' :: Ptr RWops -> IO Word32
foreign import ccall "SDL.h SDL_ReadLE64" readLE64' :: Ptr RWops -> IO Word64
foreign import ccall "SDL.h SDL_WriteBE16" writeBE16' :: Ptr RWops -> Word16 -> IO CSize
foreign import ccall "SDL.h SDL_WriteBE32" writeBE32' :: Ptr RWops -> Word32 -> IO CSize
foreign import ccall "SDL.h SDL_WriteBE64" writeBE64' :: Ptr RWops -> Word64 -> IO CSize
foreign import ccall "SDL.h SDL_WriteLE16" writeLE16' :: Ptr RWops -> Word16 -> IO CSize
foreign import ccall "SDL.h SDL_WriteLE32" writeLE32' :: Ptr RWops -> Word32 -> IO CSize
foreign import ccall "SDL.h SDL_WriteLE64" writeLE64' :: Ptr RWops -> Word64 -> IO CSize

getBasePath :: MonadIO m => m CString
getBasePath = liftIO getBasePath'
{-# INLINE getBasePath #-}

getPrefPath :: MonadIO m => CString -> CString -> m CString
getPrefPath v1 v2 = liftIO $ getPrefPath' v1 v2
{-# INLINE getPrefPath #-}

allocRW :: MonadIO m => m (Ptr RWops)
allocRW = liftIO allocRW'
{-# INLINE allocRW #-}

freeRW :: MonadIO m => Ptr RWops -> m ()
freeRW v1 = liftIO $ freeRW' v1
{-# INLINE freeRW #-}

rwFromConstMem :: MonadIO m => Ptr () -> CInt -> m (Ptr RWops)
rwFromConstMem v1 v2 = liftIO $ rwFromConstMem' v1 v2
{-# INLINE rwFromConstMem #-}

rwFromFP :: MonadIO m => Ptr () -> Bool -> m (Ptr RWops)
rwFromFP v1 v2 = liftIO $ rwFromFP' v1 v2
{-# INLINE rwFromFP #-}

rwFromFile :: MonadIO m => CString -> CString -> m (Ptr RWops)
rwFromFile v1 v2 = liftIO $ rwFromFile' v1 v2
{-# INLINE rwFromFile #-}

rwFromMem :: MonadIO m => Ptr () -> CInt -> m (Ptr RWops)
rwFromMem v1 v2 = liftIO $ rwFromMem' v1 v2
{-# INLINE rwFromMem #-}

rwClose :: MonadIO m => Ptr RWops -> m CInt
rwClose v1 = liftIO $ rwClose' v1
{-# INLINE rwClose #-}

rwRead :: MonadIO m => Ptr RWops -> Ptr () -> CSize -> CSize -> m CSize
rwRead v1 v2 v3 v4 = liftIO $ rwRead' v1 v2 v3 v4
{-# INLINE rwRead #-}

rwSeek :: MonadIO m => Ptr RWops -> Int64 -> CInt -> m Int64
rwSeek v1 v2 v3 = liftIO $ rwSeek' v1 v2 v3
{-# INLINE rwSeek #-}

rwTell :: MonadIO m => Ptr RWops -> m Int64
rwTell v1 = liftIO $ rwTell' v1
{-# INLINE rwTell #-}

rwWrite :: MonadIO m => Ptr RWops -> Ptr () -> CSize -> CSize -> m CSize
rwWrite v1 v2 v3 v4 = liftIO $ rwWrite' v1 v2 v3 v4
{-# INLINE rwWrite #-}

readBE16 :: MonadIO m => Ptr RWops -> m Word16
readBE16 v1 = liftIO $ readBE16' v1
{-# INLINE readBE16 #-}

readBE32 :: MonadIO m => Ptr RWops -> m Word32
readBE32 v1 = liftIO $ readBE32' v1
{-# INLINE readBE32 #-}

readBE64 :: MonadIO m => Ptr RWops -> m Word64
readBE64 v1 = liftIO $ readBE64' v1
{-# INLINE readBE64 #-}

readLE16 :: MonadIO m => Ptr RWops -> m Word16
readLE16 v1 = liftIO $ readLE16' v1
{-# INLINE readLE16 #-}

readLE32 :: MonadIO m => Ptr RWops -> m Word32
readLE32 v1 = liftIO $ readLE32' v1
{-# INLINE readLE32 #-}

readLE64 :: MonadIO m => Ptr RWops -> m Word64
readLE64 v1 = liftIO $ readLE64' v1
{-# INLINE readLE64 #-}

writeBE16 :: MonadIO m => Ptr RWops -> Word16 -> m CSize
writeBE16 v1 v2 = liftIO $ writeBE16' v1 v2
{-# INLINE writeBE16 #-}

writeBE32 :: MonadIO m => Ptr RWops -> Word32 -> m CSize
writeBE32 v1 v2 = liftIO $ writeBE32' v1 v2
{-# INLINE writeBE32 #-}

writeBE64 :: MonadIO m => Ptr RWops -> Word64 -> m CSize
writeBE64 v1 v2 = liftIO $ writeBE64' v1 v2
{-# INLINE writeBE64 #-}

writeLE16 :: MonadIO m => Ptr RWops -> Word16 -> m CSize
writeLE16 v1 v2 = liftIO $ writeLE16' v1 v2
{-# INLINE writeLE16 #-}

writeLE32 :: MonadIO m => Ptr RWops -> Word32 -> m CSize
writeLE32 v1 v2 = liftIO $ writeLE32' v1 v2
{-# INLINE writeLE32 #-}

writeLE64 :: MonadIO m => Ptr RWops -> Word64 -> m CSize
writeLE64 v1 v2 = liftIO $ writeLE64' v1 v2
{-# INLINE writeLE64 #-}
