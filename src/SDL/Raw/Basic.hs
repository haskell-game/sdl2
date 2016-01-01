module SDL.Raw.Basic (
  -- * Initialization and Shutdown
  init,
  initSubSystem,
  quit,
  quitSubSystem,
  setMainReady,
  wasInit,

  -- * Memory Management
  free,

  -- * Configuration Variables
  addHintCallback,
  clearHints,
  delHintCallback,
  getHint,
  setHint,
  setHintWithPriority,

  -- * Log Handling
  log,
  logCritical,
  logDebug,
  logError,
  logGetOutputFunction,
  logGetPriority,
  logInfo,
  logMessage,
  logResetPriorities,
  logSetAllPriority,
  logSetOutputFunction,
  logSetPriority,
  logVerbose,
  logWarn,

  -- * Assertions
  -- | Use Haskell's own assertion primitives rather than SDL's.

  -- * Querying SDL Version
  getRevision,
  getRevisionNumber,
  getVersion
) where

import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL.Raw.Enum
import SDL.Raw.Types
import Prelude hiding (init, log)

foreign import ccall "SDL.h SDL_Init" initFFI :: InitFlag -> IO CInt
foreign import ccall "SDL.h SDL_InitSubSystem" initSubSystemFFI :: InitFlag -> IO CInt
foreign import ccall "SDL.h SDL_Quit" quitFFI :: IO ()
foreign import ccall "SDL.h SDL_QuitSubSystem" quitSubSystemFFI :: InitFlag -> IO ()
foreign import ccall "SDL.h SDL_SetMainReady" setMainReadyFFI :: IO ()
foreign import ccall "SDL.h SDL_WasInit" wasInitFFI :: InitFlag -> IO InitFlag

foreign import ccall "SDL.h SDL_free" freeFFI :: Ptr () -> IO ()

foreign import ccall "SDL.h SDL_AddHintCallback" addHintCallbackFFI :: CString -> HintCallback -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_ClearHints" clearHintsFFI :: IO ()
foreign import ccall "SDL.h SDL_DelHintCallback" delHintCallbackFFI :: CString -> HintCallback -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_GetHint" getHintFFI :: CString -> IO CString
foreign import ccall "SDL.h SDL_SetHint" setHintFFI :: CString -> CString -> IO Bool
foreign import ccall "SDL.h SDL_SetHintWithPriority" setHintWithPriorityFFI :: CString -> CString -> HintPriority -> IO Bool

foreign import ccall "SDL.h SDL_LogGetOutputFunction" logGetOutputFunctionFFI :: Ptr LogOutputFunction -> Ptr (Ptr ()) -> IO ()
foreign import ccall "SDL.h SDL_LogGetPriority" logGetPriorityFFI :: CInt -> IO LogPriority
foreign import ccall "sdlhelper.c SDLHelper_LogMessage" logMessageFFI :: CInt -> LogPriority -> CString -> IO ()
foreign import ccall "SDL.h SDL_LogResetPriorities" logResetPrioritiesFFI :: IO ()
foreign import ccall "SDL.h SDL_LogSetAllPriority" logSetAllPriorityFFI :: LogPriority -> IO ()
foreign import ccall "SDL.h SDL_LogSetOutputFunction" logSetOutputFunctionFFI :: LogOutputFunction -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_LogSetPriority" logSetPriorityFFI :: CInt -> LogPriority -> IO ()

foreign import ccall "SDL.h SDL_GetRevision" getRevisionFFI :: IO CString
foreign import ccall "SDL.h SDL_GetRevisionNumber" getRevisionNumberFFI :: IO CInt
foreign import ccall "SDL.h SDL_GetVersion" getVersionFFI :: Ptr Version -> IO ()

init :: MonadIO m => InitFlag -> m CInt
init v1 = liftIO $ initFFI v1
{-# INLINE init #-}

initSubSystem :: MonadIO m => InitFlag -> m CInt
initSubSystem v1 = liftIO $ initSubSystemFFI v1
{-# INLINE initSubSystem #-}

quit :: MonadIO m => m ()
quit = liftIO quitFFI
{-# INLINE quit #-}

quitSubSystem :: MonadIO m => InitFlag -> m ()
quitSubSystem v1 = liftIO $ quitSubSystemFFI v1
{-# INLINE quitSubSystem #-}

setMainReady :: MonadIO m => m ()
setMainReady = liftIO setMainReadyFFI
{-# INLINE setMainReady #-}

wasInit :: MonadIO m => InitFlag -> m InitFlag
wasInit v1 = liftIO $ wasInitFFI v1
{-# INLINE wasInit #-}

free :: MonadIO m => Ptr () -> m ()
free v1 = liftIO $ freeFFI v1
{-# INLINE free #-}

addHintCallback :: MonadIO m => CString -> HintCallback -> Ptr () -> m ()
addHintCallback v1 v2 v3 = liftIO $ addHintCallbackFFI v1 v2 v3
{-# INLINE addHintCallback #-}

clearHints :: MonadIO m => m ()
clearHints = liftIO clearHintsFFI
{-# INLINE clearHints #-}

delHintCallback :: MonadIO m => CString -> HintCallback -> Ptr () -> m ()
delHintCallback v1 v2 v3 = liftIO $ delHintCallbackFFI v1 v2 v3
{-# INLINE delHintCallback #-}

getHint :: MonadIO m => CString -> m CString
getHint v1 = liftIO $ getHintFFI v1
{-# INLINE getHint #-}

setHint :: MonadIO m => CString -> CString -> m Bool
setHint v1 v2 = liftIO $ setHintFFI v1 v2
{-# INLINE setHint #-}

setHintWithPriority :: MonadIO m => CString -> CString -> HintPriority -> m Bool
setHintWithPriority v1 v2 v3 = liftIO $ setHintWithPriorityFFI v1 v2 v3
{-# INLINE setHintWithPriority #-}

log :: CString -> IO ()
log = logMessage SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_INFO
{-# INLINE log #-}

logCritical :: CInt -> CString -> IO ()
logCritical category = logMessage category SDL_LOG_PRIORITY_CRITICAL
{-# INLINE logCritical #-}

logDebug :: CInt -> CString -> IO ()
logDebug category = logMessage category SDL_LOG_PRIORITY_DEBUG
{-# INLINE logDebug #-}

logError :: CInt -> CString -> IO ()
logError category = logMessage category SDL_LOG_PRIORITY_ERROR
{-# INLINE logError #-}

logGetOutputFunction :: MonadIO m => Ptr LogOutputFunction -> Ptr (Ptr ()) -> m ()
logGetOutputFunction v1 v2 = liftIO $ logGetOutputFunctionFFI v1 v2
{-# INLINE logGetOutputFunction #-}

logGetPriority :: MonadIO m => CInt -> m LogPriority
logGetPriority v1 = liftIO $ logGetPriorityFFI v1
{-# INLINE logGetPriority #-}

logInfo :: CInt -> CString -> IO ()
logInfo category = logMessage category SDL_LOG_PRIORITY_INFO
{-# INLINE logInfo #-}

logMessage :: MonadIO m => CInt -> LogPriority -> CString -> m ()
logMessage v1 v2 v3 = liftIO $ logMessageFFI v1 v2 v3
{-# INLINE logMessage #-}

logResetPriorities :: MonadIO m => m ()
logResetPriorities = liftIO logResetPrioritiesFFI
{-# INLINE logResetPriorities #-}

logSetAllPriority :: MonadIO m => LogPriority -> m ()
logSetAllPriority v1 = liftIO $ logSetAllPriorityFFI v1
{-# INLINE logSetAllPriority #-}

logSetOutputFunction :: MonadIO m => LogOutputFunction -> Ptr () -> m ()
logSetOutputFunction v1 v2 = liftIO $ logSetOutputFunctionFFI v1 v2
{-# INLINE logSetOutputFunction #-}

logSetPriority :: MonadIO m => CInt -> LogPriority -> m ()
logSetPriority v1 v2 = liftIO $ logSetPriorityFFI v1 v2
{-# INLINE logSetPriority #-}

logVerbose :: CInt -> CString -> IO ()
logVerbose category = logMessage category SDL_LOG_PRIORITY_VERBOSE
{-# INLINE logVerbose #-}

logWarn :: CInt -> CString -> IO ()
logWarn category = logMessage category SDL_LOG_PRIORITY_WARN
{-# INLINE logWarn #-}

getRevision :: MonadIO m => m CString
getRevision = liftIO getRevisionFFI
{-# INLINE getRevision #-}

getRevisionNumber :: MonadIO m => m CInt
getRevisionNumber = liftIO getRevisionNumberFFI
{-# INLINE getRevisionNumber #-}

getVersion :: MonadIO m => Ptr Version -> m ()
getVersion v1 = liftIO $ getVersionFFI v1
{-# INLINE getVersion #-}
