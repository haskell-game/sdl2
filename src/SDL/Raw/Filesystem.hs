module SDL.Raw.Filesystem (
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

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_GetBasePath" getBasePath :: IO CString
foreign import ccall "SDL.h SDL_GetPrefPath" getPrefPath :: CString -> CString -> IO CString

foreign import ccall "SDL.h SDL_AllocRW" allocRW :: IO (Ptr RWops)
foreign import ccall "SDL.h SDL_FreeRW" freeRW :: Ptr RWops -> IO ()
foreign import ccall "SDL.h SDL_RWFromConstMem" rwFromConstMem :: Ptr () -> CInt -> IO (Ptr RWops)
foreign import ccall "SDL.h SDL_RWFromFP" rwFromFP :: Ptr () -> Bool -> IO (Ptr RWops)
foreign import ccall "SDL.h SDL_RWFromFile" rwFromFile :: CString -> CString -> IO (Ptr RWops)
foreign import ccall "SDL.h SDL_RWFromMem" rwFromMem :: Ptr () -> CInt -> IO (Ptr RWops)
foreign import ccall "sdlhelper.h SDLHelper_RWclose" rwClose :: Ptr RWops -> IO CInt
foreign import ccall "sdlhelper.h SDLHelper_RWread" rwRead :: Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize
foreign import ccall "sdlhelper.h SDLHelper_RWseek" rwSeek :: Ptr RWops -> Int64 -> CInt -> IO Int64
foreign import ccall "sdlhelper.h SDLHelper_RWtell" rwTell :: Ptr RWops -> IO Int64
foreign import ccall "sdlhelper.h SDLHelper_RWwrite" rwWrite :: Ptr RWops -> Ptr () -> CSize -> CSize -> IO CSize
foreign import ccall "SDL.h SDL_ReadBE16" readBE16 :: Ptr RWops -> IO Word16
foreign import ccall "SDL.h SDL_ReadBE32" readBE32 :: Ptr RWops -> IO Word32
foreign import ccall "SDL.h SDL_ReadBE64" readBE64 :: Ptr RWops -> IO Word64
foreign import ccall "SDL.h SDL_ReadLE16" readLE16 :: Ptr RWops -> IO Word16
foreign import ccall "SDL.h SDL_ReadLE32" readLE32 :: Ptr RWops -> IO Word32
foreign import ccall "SDL.h SDL_ReadLE64" readLE64 :: Ptr RWops -> IO Word64
foreign import ccall "SDL.h SDL_WriteBE16" writeBE16 :: Ptr RWops -> Word16 -> IO CSize
foreign import ccall "SDL.h SDL_WriteBE32" writeBE32 :: Ptr RWops -> Word32 -> IO CSize
foreign import ccall "SDL.h SDL_WriteBE64" writeBE64 :: Ptr RWops -> Word64 -> IO CSize
foreign import ccall "SDL.h SDL_WriteLE16" writeLE16 :: Ptr RWops -> Word16 -> IO CSize
foreign import ccall "SDL.h SDL_WriteLE32" writeLE32 :: Ptr RWops -> Word32 -> IO CSize
foreign import ccall "SDL.h SDL_WriteLE64" writeLE64 :: Ptr RWops -> Word64 -> IO CSize
