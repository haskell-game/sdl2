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

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Types

foreign import ccall "SDL.h CreateThread" createThread :: ThreadFunction -> CString -> IO (Ptr ())
foreign import ccall "SDL.h DetachThread" detachThread :: Ptr Thread -> IO ()
foreign import ccall "SDL.h GetThreadID" getThreadID :: Ptr Thread -> IO ThreadID
foreign import ccall "SDL.h GetThreadName" getThreadName :: Ptr Thread -> IO CString
foreign import ccall "SDL.h SetThreadPriority" setThreadPriority :: ThreadPriority -> IO CInt
foreign import ccall "SDL.h TLSCreate" tlsCreate :: IO TLSID
foreign import ccall "SDL.h TLSGet" tlsGet :: TLSID -> IO (Ptr ())
foreign import ccall "SDL.h TLSSet" tlsSet :: TLSID -> Ptr () -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall "SDL.h ThreadID" threadID :: IO ThreadID
foreign import ccall "SDL.h WaitThread" waitThread :: Ptr Thread -> Ptr CInt -> IO ()

foreign import ccall "SDL.h CondBroadcast" condBroadcast :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h CondSignal" condSignal :: Ptr Cond -> IO CInt
foreign import ccall "SDL.h CondWait" condWait :: Ptr Cond -> Ptr Mutex -> IO CInt
foreign import ccall "SDL.h CondWaitTimeout" condWaitTimeout :: Ptr Cond -> Ptr Mutex -> Word32 -> IO CInt
foreign import ccall "SDL.h CreateCond" createCond :: IO (Ptr Cond)
foreign import ccall "SDL.h CreateMutex" createMutex :: IO (Ptr Mutex)
foreign import ccall "SDL.h CreateSemaphore" createSemaphore :: Word32 -> IO (Ptr Sem)
foreign import ccall "SDL.h DestroyCond" destroyCond :: Ptr Cond -> IO ()
foreign import ccall "SDL.h DestroyMutex" destroyMutex :: Ptr Mutex -> IO ()
foreign import ccall "SDL.h DestroySemaphore" destroySemaphore :: Ptr Sem -> IO ()
foreign import ccall "SDL.h LockMutex" lockMutex :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h SemPost" semPost :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SemTryWait" semTryWait :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SemValue" semValue :: Ptr Sem -> IO Word32
foreign import ccall "SDL.h SemWait" semWait :: Ptr Sem -> IO CInt
foreign import ccall "SDL.h SemWaitTimeout" semWaitTimeout :: Ptr Sem -> Word32 -> IO CInt
foreign import ccall "SDL.h TryLockMutex" tryLockMutex :: Ptr Mutex -> IO CInt
foreign import ccall "SDL.h UnlockMutex" unlockMutex :: Ptr Mutex -> IO CInt

foreign import ccall "SDL.h AtomicAdd" atomicAdd :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h AtomicCAS" atomicCAS :: Ptr Atomic -> CInt -> CInt -> IO Bool
foreign import ccall "SDL.h AtomicCASPtr" atomicCASPtr :: Ptr (Ptr ()) -> Ptr () -> Ptr () -> IO Bool
foreign import ccall "SDL.h AtomicGet" atomicGet :: Ptr Atomic -> IO CInt
foreign import ccall "SDL.h AtomicGetPtr" atomicGetPtr :: Ptr (Ptr ()) -> IO (Ptr ())
foreign import ccall "SDL.h AtomicLock" atomicLock :: Ptr SpinLock -> IO ()
foreign import ccall "SDL.h AtomicSet" atomicSet :: Ptr Atomic -> CInt -> IO CInt
foreign import ccall "SDL.h AtomicSetPtr" atomicSetPtr :: Ptr (Ptr ()) -> Ptr () -> IO (Ptr ())
foreign import ccall "SDL.h AtomicTryLock" atomicTryLock :: Ptr SpinLock -> IO Bool
foreign import ccall "SDL.h AtomicUnlock" atomicUnlock :: Ptr SpinLock -> IO ()

atomicDecRef :: Ptr Atomic -> IO Bool
atomicDecRef a = do
	old <- atomicAdd a (-1)
	return $ old == 1

atomicIncRef :: Ptr Atomic -> IO CInt
atomicIncRef a = atomicAdd a 1
