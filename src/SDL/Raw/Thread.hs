module SDL.Raw.Thread (
  -- * Thread Management
  createThread,
  detachThread,
  getThreadID,
  getThreadName,
  setThreadPriority,
  tlsCreate,
  tlsGet,
  tlsSet,
  threadID,
  waitThread,

  -- * Thread Synchronization Primitives
  condBroadcast,
  condSignal,
  condWait,
  condWaitTimeout,
  createCond,
  createMutex,
  createSemaphore,
  destroyCond,
  destroyMutex,
  destroySemaphore,
  lockMutex,
  semPost,
  semTryWait,
  semValue,
  semWait,
  semWaitTimeout,
  tryLockMutex,
  unlockMutex,

  -- * Atomic Operations
  atomicAdd,
  atomicCAS,
  atomicCASPtr,
  atomicDecRef,
  atomicGet,
  atomicGetPtr,
  atomicIncRef,
  atomicLock,
  atomicSet,
  atomicSetPtr,
  atomicTryLock,
  atomicUnlock
) where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_CreateThread" createThread :: ThreadFunction -> CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_DetachThread" detachThread :: Ptr Thread -> IO ()
foreign import ccall "SDL.h SDL_GetThreadID" getThreadID :: Ptr Thread -> IO ThreadID
foreign import ccall "SDL.h SDL_GetThreadName" getThreadName :: Ptr Thread -> IO CString
foreign import ccall "SDL.h SDL_SetThreadPriority" setThreadPriority :: ThreadPriority -> IO CInt
foreign import ccall "SDL.h SDL_TLSCreate" tlsCreate :: IO TLSID
foreign import ccall "SDL.h SDL_TLSGet" tlsGet :: TLSID -> IO (Ptr ())
foreign import ccall "SDL.h SDL_TLSSet" tlsSet :: TLSID -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall "SDL.h SDL_ThreadID" threadID :: IO ThreadID
foreign import ccall "SDL.h SDL_WaitThread" waitThread :: Ptr Thread -> Ptr CInt -> IO ()

foreign import ccall "SDL.h SDL_CondBroadcast" condBroadcast :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h SDL_CondSignal" condSignal :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h SDL_CondWait" condWait :: Ptr Cond -> Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_CondWaitTimeout" condWaitTimeout :: Ptr Cond -> Ptr Mutex -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_CreateCond" createCond :: IO (Ptr Cond)
foreign import ccall "SDL.h SDL_CreateMutex" createMutex :: IO (Ptr Mutex)
foreign import ccall "SDL.h SDL_CreateSemaphore" createSemaphore :: Word32 -> IO (Ptr Sem)
foreign import ccall "SDL.h SDL_DestroyCond" destroyCond :: Ptr Cond -> IO ()
foreign import ccall "SDL.h SDL_DestroyMutex" destroyMutex :: Ptr Mutex -> IO ()
foreign import ccall "SDL.h SDL_DestroySemaphore" destroySemaphore :: Ptr Sem -> IO ()
foreign import ccall "SDL.h SDL_LockMutex" lockMutex :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_SemPost" semPost :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemTryWait" semTryWait :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemValue" semValue :: Ptr Sem -> IO Word32
foreign import ccall "SDL.h SDL_SemWait" semWait :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemWaitTimeout" semWaitTimeout :: Ptr Sem -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_TryLockMutex" tryLockMutex :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_UnlockMutex" unlockMutex :: Ptr Mutex -> IO CInt

foreign import ccall "SDL.h SDL_AtomicAdd" atomicAdd :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_AtomicCAS" atomicCAS :: Ptr Atomic -> CInt -> CInt -> IO Bool
foreign import ccall "SDL.h SDL_AtomicCASPtr" atomicCASPtr :: Ptr (Ptr ()) -> Ptr () -> Ptr () -> IO Bool
foreign import ccall "SDL.h SDL_AtomicGet" atomicGet :: Ptr Atomic -> IO CInt
foreign import ccall "SDL.h SDL_AtomicGetPtr" atomicGetPtr :: Ptr (Ptr ()) -> IO (Ptr ())
foreign import ccall "SDL.h SDL_AtomicLock" atomicLock :: Ptr SpinLock -> IO ()
foreign import ccall "SDL.h SDL_AtomicSet" atomicSet :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_AtomicSetPtr" atomicSetPtr :: Ptr (Ptr ()) -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_AtomicTryLock" atomicTryLock :: Ptr SpinLock -> IO Bool
foreign import ccall "SDL.h SDL_AtomicUnlock" atomicUnlock :: Ptr SpinLock -> IO ()

atomicDecRef :: Ptr Atomic -> IO Bool
atomicDecRef a = do
  old <- atomicAdd a (-1)
  return $ old == 1

atomicIncRef :: Ptr Atomic -> IO CInt
atomicIncRef a = atomicAdd a 1
