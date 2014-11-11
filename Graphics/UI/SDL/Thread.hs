module Graphics.UI.SDL.Thread (
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
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h SDL_CreateThread" createThread' :: ThreadFunction -> CString -> IO (Ptr ())
foreign import ccall "SDL.h SDL_DetachThread" detachThread' :: Ptr Thread -> IO ()
foreign import ccall "SDL.h SDL_GetThreadID" getThreadID' :: Ptr Thread -> IO ThreadID
foreign import ccall "SDL.h SDL_GetThreadName" getThreadName' :: Ptr Thread -> IO CString
foreign import ccall "SDL.h SDL_SetThreadPriority" setThreadPriority' :: ThreadPriority -> IO CInt
foreign import ccall "SDL.h SDL_TLSCreate" tlsCreate' :: IO TLSID
foreign import ccall "SDL.h SDL_TLSGet" tlsGet' :: TLSID -> IO (Ptr ())
foreign import ccall "SDL.h SDL_TLSSet" tlsSet' :: TLSID -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall "SDL.h SDL_ThreadID" threadID' :: IO ThreadID
foreign import ccall "SDL.h SDL_WaitThread" waitThread' :: Ptr Thread -> Ptr CInt -> IO ()

foreign import ccall "SDL.h SDL_CondBroadcast" condBroadcast' :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h SDL_CondSignal" condSignal' :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h SDL_CondWait" condWait' :: Ptr Cond -> Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_CondWaitTimeout" condWaitTimeout' :: Ptr Cond -> Ptr Mutex -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_CreateCond" createCond' :: IO (Ptr Cond)
foreign import ccall "SDL.h SDL_CreateMutex" createMutex' :: IO (Ptr Mutex)
foreign import ccall "SDL.h SDL_CreateSemaphore" createSemaphore' :: Word32 -> IO (Ptr Sem)
foreign import ccall "SDL.h SDL_DestroyCond" destroyCond' :: Ptr Cond -> IO ()
foreign import ccall "SDL.h SDL_DestroyMutex" destroyMutex' :: Ptr Mutex -> IO ()
foreign import ccall "SDL.h SDL_DestroySemaphore" destroySemaphore' :: Ptr Sem -> IO ()
foreign import ccall "SDL.h SDL_LockMutex" lockMutex' :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_SemPost" semPost' :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemTryWait" semTryWait' :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemValue" semValue' :: Ptr Sem -> IO Word32
foreign import ccall "SDL.h SDL_SemWait" semWait' :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SDL_SemWaitTimeout" semWaitTimeout' :: Ptr Sem -> Word32 -> IO CInt
foreign import ccall "SDL.h SDL_TryLockMutex" tryLockMutex' :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SDL_UnlockMutex" unlockMutex' :: Ptr Mutex -> IO CInt

foreign import ccall "SDL.h SDL_AtomicAdd" atomicAdd' :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_AtomicCAS" atomicCAS' :: Ptr Atomic -> CInt -> CInt -> IO Bool
foreign import ccall "SDL.h SDL_AtomicCASPtr" atomicCASPtr' :: Ptr (Ptr ()) -> Ptr () -> Ptr () -> IO Bool
foreign import ccall "SDL.h SDL_AtomicGet" atomicGet' :: Ptr Atomic -> IO CInt
foreign import ccall "SDL.h SDL_AtomicGetPtr" atomicGetPtr' :: Ptr (Ptr ()) -> IO (Ptr ())
foreign import ccall "SDL.h SDL_AtomicLock" atomicLock' :: Ptr SpinLock -> IO ()
foreign import ccall "SDL.h SDL_AtomicSet" atomicSet' :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_AtomicSetPtr" atomicSetPtr' :: Ptr (Ptr ()) -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h SDL_AtomicTryLock" atomicTryLock' :: Ptr SpinLock -> IO Bool
foreign import ccall "SDL.h SDL_AtomicUnlock" atomicUnlock' :: Ptr SpinLock -> IO ()

createThread :: MonadIO m => ThreadFunction -> CString -> m (Ptr ())
createThread v1 v2 = liftIO $ createThread' v1 v2
{-# INLINE createThread #-}

detachThread :: MonadIO m => Ptr Thread -> m ()
detachThread v1 = liftIO $ detachThread' v1
{-# INLINE detachThread #-}

getThreadID :: MonadIO m => Ptr Thread -> m ThreadID
getThreadID v1 = liftIO $ getThreadID' v1
{-# INLINE getThreadID #-}

getThreadName :: MonadIO m => Ptr Thread -> m CString
getThreadName v1 = liftIO $ getThreadName' v1
{-# INLINE getThreadName #-}

setThreadPriority :: MonadIO m => ThreadPriority -> m CInt
setThreadPriority v1 = liftIO $ setThreadPriority' v1
{-# INLINE setThreadPriority #-}

tlsCreate :: MonadIO m => m TLSID
tlsCreate = liftIO tlsCreate'
{-# INLINE tlsCreate #-}

tlsGet :: MonadIO m => TLSID -> m (Ptr ())
tlsGet v1 = liftIO $ tlsGet' v1
{-# INLINE tlsGet #-}

tlsSet :: MonadIO m => TLSID -> Ptr () -> FunPtr (Ptr () -> IO ()) -> m CInt
tlsSet v1 v2 v3 = liftIO $ tlsSet' v1 v2 v3
{-# INLINE tlsSet #-}

threadID :: MonadIO m => m ThreadID
threadID = liftIO threadID'
{-# INLINE threadID #-}

waitThread :: MonadIO m => Ptr Thread -> Ptr CInt -> m ()
waitThread v1 v2 = liftIO $ waitThread' v1 v2
{-# INLINE waitThread #-}

condBroadcast :: MonadIO m => Ptr Cond -> m CInt
condBroadcast v1 = liftIO $ condBroadcast' v1
{-# INLINE condBroadcast #-}

condSignal :: MonadIO m => Ptr Cond -> m CInt
condSignal v1 = liftIO $ condSignal' v1
{-# INLINE condSignal #-}

condWait :: MonadIO m => Ptr Cond -> Ptr Mutex -> m CInt
condWait v1 v2 = liftIO $ condWait' v1 v2
{-# INLINE condWait #-}

condWaitTimeout :: MonadIO m => Ptr Cond -> Ptr Mutex -> Word32 -> m CInt
condWaitTimeout v1 v2 v3 = liftIO $ condWaitTimeout' v1 v2 v3
{-# INLINE condWaitTimeout #-}

createCond :: MonadIO m => m (Ptr Cond)
createCond = liftIO createCond'
{-# INLINE createCond #-}

createMutex :: MonadIO m => m (Ptr Mutex)
createMutex = liftIO createMutex'
{-# INLINE createMutex #-}

createSemaphore :: MonadIO m => Word32 -> m (Ptr Sem)
createSemaphore v1 = liftIO $ createSemaphore' v1
{-# INLINE createSemaphore #-}

destroyCond :: MonadIO m => Ptr Cond -> m ()
destroyCond v1 = liftIO $ destroyCond' v1
{-# INLINE destroyCond #-}

destroyMutex :: MonadIO m => Ptr Mutex -> m ()
destroyMutex v1 = liftIO $ destroyMutex' v1
{-# INLINE destroyMutex #-}

destroySemaphore :: MonadIO m => Ptr Sem -> m ()
destroySemaphore v1 = liftIO $ destroySemaphore' v1
{-# INLINE destroySemaphore #-}

lockMutex :: MonadIO m => Ptr Mutex -> m CInt
lockMutex v1 = liftIO $ lockMutex' v1
{-# INLINE lockMutex #-}

semPost :: MonadIO m => Ptr Sem -> m CInt
semPost v1 = liftIO $ semPost' v1
{-# INLINE semPost #-}

semTryWait :: MonadIO m => Ptr Sem -> m CInt
semTryWait v1 = liftIO $ semTryWait' v1
{-# INLINE semTryWait #-}

semValue :: MonadIO m => Ptr Sem -> m Word32
semValue v1 = liftIO $ semValue' v1
{-# INLINE semValue #-}

semWait :: MonadIO m => Ptr Sem -> m CInt
semWait v1 = liftIO $ semWait' v1
{-# INLINE semWait #-}

semWaitTimeout :: MonadIO m => Ptr Sem -> Word32 -> m CInt
semWaitTimeout v1 v2 = liftIO $ semWaitTimeout' v1 v2
{-# INLINE semWaitTimeout #-}

tryLockMutex :: MonadIO m => Ptr Mutex -> m CInt
tryLockMutex v1 = liftIO $ tryLockMutex' v1
{-# INLINE tryLockMutex #-}

unlockMutex :: MonadIO m => Ptr Mutex -> m CInt
unlockMutex v1 = liftIO $ unlockMutex' v1
{-# INLINE unlockMutex #-}

atomicAdd :: MonadIO m => Ptr Atomic -> CInt -> m CInt
atomicAdd v1 v2 = liftIO $ atomicAdd' v1 v2
{-# INLINE atomicAdd #-}

atomicCAS :: MonadIO m => Ptr Atomic -> CInt -> CInt -> m Bool
atomicCAS v1 v2 v3 = liftIO $ atomicCAS' v1 v2 v3
{-# INLINE atomicCAS #-}

atomicCASPtr :: MonadIO m => Ptr (Ptr ()) -> Ptr () -> Ptr () -> m Bool
atomicCASPtr v1 v2 v3 = liftIO $ atomicCASPtr' v1 v2 v3
{-# INLINE atomicCASPtr #-}

atomicDecRef :: Ptr Atomic -> IO Bool
atomicDecRef a = do
  old <- atomicAdd a (-1)
  return $ old == 1
{-# INLINE atomicDecRef #-}

atomicGet :: MonadIO m => Ptr Atomic -> m CInt
atomicGet v1 = liftIO $ atomicGet' v1
{-# INLINE atomicGet #-}

atomicGetPtr :: MonadIO m => Ptr (Ptr ()) -> m (Ptr ())
atomicGetPtr v1 = liftIO $ atomicGetPtr' v1
{-# INLINE atomicGetPtr #-}

atomicIncRef :: Ptr Atomic -> IO CInt
atomicIncRef a = atomicAdd a 1
{-# INLINE atomicIncRef #-}

atomicLock :: MonadIO m => Ptr SpinLock -> m ()
atomicLock v1 = liftIO $ atomicLock' v1
{-# INLINE atomicLock #-}

atomicSet :: MonadIO m => Ptr Atomic -> CInt -> m CInt
atomicSet v1 v2 = liftIO $ atomicSet' v1 v2
{-# INLINE atomicSet #-}

atomicSetPtr :: MonadIO m => Ptr (Ptr ()) -> Ptr () -> m (Ptr ())
atomicSetPtr v1 v2 = liftIO $ atomicSetPtr' v1 v2
{-# INLINE atomicSetPtr #-}

atomicTryLock :: MonadIO m => Ptr SpinLock -> m Bool
atomicTryLock v1 = liftIO $ atomicTryLock' v1
{-# INLINE atomicTryLock #-}

atomicUnlock :: MonadIO m => Ptr SpinLock -> m ()
atomicUnlock v1 = liftIO $ atomicUnlock' v1
{-# INLINE atomicUnlock #-}
