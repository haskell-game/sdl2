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

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum
import SDL.Raw.Types

foreign import ccall "SDL.h SDL_CreateThread" createThreadFFI :: ThreadFunction -> CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_DetachThread" detachThreadFFI :: Ptr Thread -> IO ()
foreign import ccall "SDL.h SDL_GetThreadID" getThreadIDFFI :: Ptr Thread -> IO ThreadID
foreign import ccall "SDL.h SDL_GetThreadName" getThreadNameFFI :: Ptr Thread -> IO CString
foreign import ccall "SDL.h SDL_SetThreadPriority" setThreadPriorityFFI :: ThreadPriority -> IO CInt
foreign import ccall "SDL.h SDL_TLSCreate" tlsCreateFFI :: IO TLSID
foreign import ccall "SDL.h SDL_TLSGet" tlsGetFFI :: TLSID -> IO (Ptr ())
foreign import ccall "SDL.h SDL_TLSSet" tlsSetFFI :: TLSID -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall "SDL.h SDL_ThreadID" threadIDFFI :: IO ThreadID
foreign import ccall "SDL.h SDL_WaitThread" waitThreadFFI :: Ptr Thread -> Ptr CInt -> IO ()

foreign import ccall "SDL.h SDL_CondBroadcast" condBroadcastFFI :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h SDL_CondSignal" condSignalFFI :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h SDL_CondWait" condWaitFFI :: Ptr Cond -> Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_CondWaitTimeout" condWaitTimeoutFFI :: Ptr Cond -> Ptr Mutex -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_CreateCond" createCondFFI :: IO (Ptr Cond)
foreign import ccall "SDL.h SDL_CreateMutex" createMutexFFI :: IO (Ptr Mutex)
foreign import ccall "SDL.h SDL_CreateSemaphore" createSemaphoreFFI :: Word32 -> IO (Ptr Sem)
foreign import ccall "SDL.h SDL_DestroyCond" destroyCondFFI :: Ptr Cond -> IO ()
foreign import ccall "SDL.h SDL_DestroyMutex" destroyMutexFFI :: Ptr Mutex -> IO ()
foreign import ccall "SDL.h SDL_DestroySemaphore" destroySemaphoreFFI :: Ptr Sem -> IO ()
foreign import ccall "SDL.h SDL_LockMutex" lockMutexFFI :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_SemPost" semPostFFI :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemTryWait" semTryWaitFFI :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemValue" semValueFFI :: Ptr Sem -> IO Word32
foreign import ccall "SDL.h SDL_SemWait" semWaitFFI :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemWaitTimeout" semWaitTimeoutFFI :: Ptr Sem -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_TryLockMutex" tryLockMutexFFI :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_UnlockMutex" unlockMutexFFI :: Ptr Mutex -> IO CInt

foreign import ccall "SDL.h SDL_AtomicAdd" atomicAddFFI :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_AtomicCAS" atomicCASFFI :: Ptr Atomic -> CInt -> CInt -> IO Bool
foreign import ccall "SDL.h SDL_AtomicCASPtr" atomicCASPtrFFI :: Ptr (Ptr ()) -> Ptr () -> Ptr () -> IO Bool
foreign import ccall "SDL.h SDL_AtomicGet" atomicGetFFI :: Ptr Atomic -> IO CInt
foreign import ccall "SDL.h SDL_AtomicGetPtr" atomicGetPtrFFI :: Ptr (Ptr ()) -> IO (Ptr ())
foreign import ccall "SDL.h SDL_AtomicLock" atomicLockFFI :: Ptr SpinLock -> IO ()
foreign import ccall "SDL.h SDL_AtomicSet" atomicSetFFI :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_AtomicSetPtr" atomicSetPtrFFI :: Ptr (Ptr ()) -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_AtomicTryLock" atomicTryLockFFI :: Ptr SpinLock -> IO Bool
foreign import ccall "SDL.h SDL_AtomicUnlock" atomicUnlockFFI :: Ptr SpinLock -> IO ()

createThread :: MonadIO m => ThreadFunction -> CString -> m (Ptr ())
createThread v1 v2 = liftIO $ createThreadFFI v1 v2
{-# INLINE createThread #-}

detachThread :: MonadIO m => Ptr Thread -> m ()
detachThread v1 = liftIO $ detachThreadFFI v1
{-# INLINE detachThread #-}

getThreadID :: MonadIO m => Ptr Thread -> m ThreadID
getThreadID v1 = liftIO $ getThreadIDFFI v1
{-# INLINE getThreadID #-}

getThreadName :: MonadIO m => Ptr Thread -> m CString
getThreadName v1 = liftIO $ getThreadNameFFI v1
{-# INLINE getThreadName #-}

setThreadPriority :: MonadIO m => ThreadPriority -> m CInt
setThreadPriority v1 = liftIO $ setThreadPriorityFFI v1
{-# INLINE setThreadPriority #-}

tlsCreate :: MonadIO m => m TLSID
tlsCreate = liftIO tlsCreateFFI
{-# INLINE tlsCreate #-}

tlsGet :: MonadIO m => TLSID -> m (Ptr ())
tlsGet v1 = liftIO $ tlsGetFFI v1
{-# INLINE tlsGet #-}

tlsSet :: MonadIO m => TLSID -> Ptr () -> FunPtr (Ptr () -> IO ()) -> m CInt
tlsSet v1 v2 v3 = liftIO $ tlsSetFFI v1 v2 v3
{-# INLINE tlsSet #-}

threadID :: MonadIO m => m ThreadID
threadID = liftIO threadIDFFI
{-# INLINE threadID #-}

waitThread :: MonadIO m => Ptr Thread -> Ptr CInt -> m ()
waitThread v1 v2 = liftIO $ waitThreadFFI v1 v2
{-# INLINE waitThread #-}

condBroadcast :: MonadIO m => Ptr Cond -> m CInt
condBroadcast v1 = liftIO $ condBroadcastFFI v1
{-# INLINE condBroadcast #-}

condSignal :: MonadIO m => Ptr Cond -> m CInt
condSignal v1 = liftIO $ condSignalFFI v1
{-# INLINE condSignal #-}

condWait :: MonadIO m => Ptr Cond -> Ptr Mutex -> m CInt
condWait v1 v2 = liftIO $ condWaitFFI v1 v2
{-# INLINE condWait #-}

condWaitTimeout :: MonadIO m => Ptr Cond -> Ptr Mutex -> Word32 -> m CInt
condWaitTimeout v1 v2 v3 = liftIO $ condWaitTimeoutFFI v1 v2 v3
{-# INLINE condWaitTimeout #-}

createCond :: MonadIO m => m (Ptr Cond)
createCond = liftIO createCondFFI
{-# INLINE createCond #-}

createMutex :: MonadIO m => m (Ptr Mutex)
createMutex = liftIO createMutexFFI
{-# INLINE createMutex #-}

createSemaphore :: MonadIO m => Word32 -> m (Ptr Sem)
createSemaphore v1 = liftIO $ createSemaphoreFFI v1
{-# INLINE createSemaphore #-}

destroyCond :: MonadIO m => Ptr Cond -> m ()
destroyCond v1 = liftIO $ destroyCondFFI v1
{-# INLINE destroyCond #-}

destroyMutex :: MonadIO m => Ptr Mutex -> m ()
destroyMutex v1 = liftIO $ destroyMutexFFI v1
{-# INLINE destroyMutex #-}

destroySemaphore :: MonadIO m => Ptr Sem -> m ()
destroySemaphore v1 = liftIO $ destroySemaphoreFFI v1
{-# INLINE destroySemaphore #-}

lockMutex :: MonadIO m => Ptr Mutex -> m CInt
lockMutex v1 = liftIO $ lockMutexFFI v1
{-# INLINE lockMutex #-}

semPost :: MonadIO m => Ptr Sem -> m CInt
semPost v1 = liftIO $ semPostFFI v1
{-# INLINE semPost #-}

semTryWait :: MonadIO m => Ptr Sem -> m CInt
semTryWait v1 = liftIO $ semTryWaitFFI v1
{-# INLINE semTryWait #-}

semValue :: MonadIO m => Ptr Sem -> m Word32
semValue v1 = liftIO $ semValueFFI v1
{-# INLINE semValue #-}

semWait :: MonadIO m => Ptr Sem -> m CInt
semWait v1 = liftIO $ semWaitFFI v1
{-# INLINE semWait #-}

semWaitTimeout :: MonadIO m => Ptr Sem -> Word32 -> m CInt
semWaitTimeout v1 v2 = liftIO $ semWaitTimeoutFFI v1 v2
{-# INLINE semWaitTimeout #-}

tryLockMutex :: MonadIO m => Ptr Mutex -> m CInt
tryLockMutex v1 = liftIO $ tryLockMutexFFI v1
{-# INLINE tryLockMutex #-}

unlockMutex :: MonadIO m => Ptr Mutex -> m CInt
unlockMutex v1 = liftIO $ unlockMutexFFI v1
{-# INLINE unlockMutex #-}

atomicAdd :: MonadIO m => Ptr Atomic -> CInt -> m CInt
atomicAdd v1 v2 = liftIO $ atomicAddFFI v1 v2
{-# INLINE atomicAdd #-}

atomicCAS :: MonadIO m => Ptr Atomic -> CInt -> CInt -> m Bool
atomicCAS v1 v2 v3 = liftIO $ atomicCASFFI v1 v2 v3
{-# INLINE atomicCAS #-}

atomicCASPtr :: MonadIO m => Ptr (Ptr ()) -> Ptr () -> Ptr () -> m Bool
atomicCASPtr v1 v2 v3 = liftIO $ atomicCASPtrFFI v1 v2 v3
{-# INLINE atomicCASPtr #-}

atomicDecRef :: Ptr Atomic -> IO Bool
atomicDecRef a = do
  old <- atomicAdd a (-1)
  return $ old == 1
{-# INLINE atomicDecRef #-}

atomicGet :: MonadIO m => Ptr Atomic -> m CInt
atomicGet v1 = liftIO $ atomicGetFFI v1
{-# INLINE atomicGet #-}

atomicGetPtr :: MonadIO m => Ptr (Ptr ()) -> m (Ptr ())
atomicGetPtr v1 = liftIO $ atomicGetPtrFFI v1
{-# INLINE atomicGetPtr #-}

atomicIncRef :: Ptr Atomic -> IO CInt
atomicIncRef a = atomicAdd a 1
{-# INLINE atomicIncRef #-}

atomicLock :: MonadIO m => Ptr SpinLock -> m ()
atomicLock v1 = liftIO $ atomicLockFFI v1
{-# INLINE atomicLock #-}

atomicSet :: MonadIO m => Ptr Atomic -> CInt -> m CInt
atomicSet v1 v2 = liftIO $ atomicSetFFI v1 v2
{-# INLINE atomicSet #-}

atomicSetPtr :: MonadIO m => Ptr (Ptr ()) -> Ptr () -> m (Ptr ())
atomicSetPtr v1 v2 = liftIO $ atomicSetPtrFFI v1 v2
{-# INLINE atomicSetPtr #-}

atomicTryLock :: MonadIO m => Ptr SpinLock -> m Bool
atomicTryLock v1 = liftIO $ atomicTryLockFFI v1
{-# INLINE atomicTryLock #-}

atomicUnlock :: MonadIO m => Ptr SpinLock -> m ()
atomicUnlock v1 = liftIO $ atomicUnlockFFI v1
{-# INLINE atomicUnlock #-}
