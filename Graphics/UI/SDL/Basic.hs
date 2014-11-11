module Graphics.UI.SDL.Basic (
  -- * Initialization and Shutdown
  init,
  initSubSystem,
  quit,
  quitSubSystem,
  setMainReady,
  wasInit,

  -- * Configuration Variables
  addHintCallback,
  clearHints,
  delHintCallback,
  getHint,
  setHint,
  setHintWithPriority,

  -- * Error Handling
  clearError,
  getError,
  setError,

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
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Types
import Prelude hiding (init, log)

foreign import ccall "SDL.h SDL_Init" init' :: InitFlag -> IO CInt
foreign import ccall "SDL.h SDL_InitSubSystem" initSubSystem' :: InitFlag -> IO CInt
foreign import ccall "SDL.h SDL_Quit" quit' :: IO ()
foreign import ccall "SDL.h SDL_QuitSubSystem" quitSubSystem' :: InitFlag -> IO ()
foreign import ccall "SDL.h SDL_SetMainReady" setMainReady' :: IO ()
foreign import ccall "SDL.h SDL_WasInit" wasInit' :: InitFlag -> IO InitFlag

foreign import ccall "SDL.h SDL_AddHintCallback" addHintCallback' :: CString -> HintCallback -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_ClearHints" clearHints' :: IO ()
foreign import ccall "SDL.h SDL_DelHintCallback" delHintCallback' :: CString -> HintCallback -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_GetHint" getHint' :: CString -> IO CString
foreign import ccall "SDL.h SDL_SetHint" setHint' :: CString -> CString -> IO Bool
foreign import ccall "SDL.h SDL_SetHintWithPriority" setHintWithPriority' :: CString -> CString -> HintPriority -> IO Bool

foreign import ccall "SDL.h SDL_ClearError" clearError' :: IO ()
foreign import ccall "SDL.h SDL_GetError" getError' :: IO CString
foreign import ccall "sdlhelper.c SDLHelper_SetError" setError' :: CString -> IO CInt

foreign import ccall "SDL.h SDL_LogGetOutputFunction" logGetOutputFunction' :: Ptr LogOutputFunction -> Ptr (Ptr ()) -> IO ()
foreign import ccall "SDL.h SDL_LogGetPriority" logGetPriority' :: CInt -> IO LogPriority
foreign import ccall "sdlhelper.c SDLHelper_LogMessage" logMessage' :: CInt -> LogPriority -> CString -> IO ()
foreign import ccall "SDL.h SDL_LogResetPriorities" logResetPriorities' :: IO ()
foreign import ccall "SDL.h SDL_LogSetAllPriority" logSetAllPriority' :: LogPriority -> IO ()
foreign import ccall "SDL.h SDL_LogSetOutputFunction" logSetOutputFunction' :: LogOutputFunction -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_LogSetPriority" logSetPriority' :: CInt -> LogPriority -> IO ()

foreign import ccall "SDL.h SDL_GetRevision" getRevision' :: IO CString
foreign import ccall "SDL.h SDL_GetRevisionNumber" getRevisionNumber' :: IO CInt
foreign import ccall "SDL.h SDL_GetVersion" getVersion' :: Ptr Version -> IO ()

init :: MonadIO m => InitFlag -> m CInt
init v1 = liftIO $ init' v1
{-# INLINE init #-}

initSubSystem :: MonadIO m => InitFlag -> m CInt
initSubSystem v1 = liftIO $ initSubSystem' v1
{-# INLINE initSubSystem #-}

quit :: MonadIO m => m ()
quit = liftIO quit'
{-# INLINE quit #-}

quitSubSystem :: MonadIO m => InitFlag -> m ()
quitSubSystem v1 = liftIO $ quitSubSystem' v1
{-# INLINE quitSubSystem #-}

setMainReady :: MonadIO m => m ()
setMainReady = liftIO setMainReady'
{-# INLINE setMainReady #-}

wasInit :: MonadIO m => InitFlag -> m InitFlag
wasInit v1 = liftIO $ wasInit' v1
{-# INLINE wasInit #-}

addHintCallback :: MonadIO m => CString -> HintCallback -> Ptr () -> m ()
addHintCallback v1 v2 v3 = liftIO $ addHintCallback' v1 v2 v3
{-# INLINE addHintCallback #-}

clearHints :: MonadIO m => m ()
clearHints = liftIO clearHints'
{-# INLINE clearHints #-}

delHintCallback :: MonadIO m => CString -> HintCallback -> Ptr () -> m ()
delHintCallback v1 v2 v3 = liftIO $ delHintCallback' v1 v2 v3
{-# INLINE delHintCallback #-}

getHint :: MonadIO m => CString -> m CString
getHint v1 = liftIO $ getHint' v1
{-# INLINE getHint #-}

setHint :: MonadIO m => CString -> CString -> m Bool
setHint v1 v2 = liftIO $ setHint' v1 v2
{-# INLINE setHint #-}

setHintWithPriority :: MonadIO m => CString -> CString -> HintPriority -> m Bool
setHintWithPriority v1 v2 v3 = liftIO $ setHintWithPriority' v1 v2 v3
{-# INLINE setHintWithPriority #-}

clearError :: MonadIO m => m ()
clearError = liftIO clearError'
{-# INLINE clearError #-}

getError :: MonadIO m => m CString
getError = liftIO getError'
{-# INLINE getError #-}

setError :: MonadIO m => CString -> m CInt
setError v1 = liftIO $ setError' v1
{-# INLINE setError #-}

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
logGetOutputFunction v1 v2 = liftIO $ logGetOutputFunction' v1 v2
{-# INLINE logGetOutputFunction #-}

logGetPriority :: MonadIO m => CInt -> m LogPriority
logGetPriority v1 = liftIO $ logGetPriority' v1
{-# INLINE logGetPriority #-}

logInfo :: CInt -> CString -> IO ()
logInfo category = logMessage category SDL_LOG_PRIORITY_INFO
{-# INLINE logInfo #-}

logMessage :: MonadIO m => CInt -> LogPriority -> CString -> m ()
logMessage v1 v2 v3 = liftIO $ logMessage' v1 v2 v3
{-# INLINE logMessage #-}

logResetPriorities :: MonadIO m => m ()
logResetPriorities = liftIO logResetPriorities'
{-# INLINE logResetPriorities #-}

logSetAllPriority :: MonadIO m => LogPriority -> m ()
logSetAllPriority v1 = liftIO $ logSetAllPriority' v1
{-# INLINE logSetAllPriority #-}

logSetOutputFunction :: MonadIO m => LogOutputFunction -> Ptr () -> m ()
logSetOutputFunction v1 v2 = liftIO $ logSetOutputFunction' v1 v2
{-# INLINE logSetOutputFunction #-}

logSetPriority :: MonadIO m => CInt -> LogPriority -> m ()
logSetPriority v1 v2 = liftIO $ logSetPriority' v1 v2
{-# INLINE logSetPriority #-}

logVerbose :: CInt -> CString -> IO ()
logVerbose category = logMessage category SDL_LOG_PRIORITY_VERBOSE
{-# INLINE logVerbose #-}

logWarn :: CInt -> CString -> IO ()
logWarn category = logMessage category SDL_LOG_PRIORITY_WARN
{-# INLINE logWarn #-}

getRevision :: MonadIO m => m CString
getRevision = liftIO getRevision'
{-# INLINE getRevision #-}

getRevisionNumber :: MonadIO m => m CInt
getRevisionNumber = liftIO getRevisionNumber'
{-# INLINE getRevisionNumber #-}

getVersion :: MonadIO m => Ptr Version -> m ()
getVersion v1 = liftIO $ getVersion' v1
{-# INLINE getVersion #-}
