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

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.SDL.Enum
import Graphics.UI.SDL.Types
import Prelude hiding (init, log)

foreign import ccall "SDL.h SDL_Init" init :: InitFlag -> IO CInt
foreign import ccall "SDL.h SDL_InitSubSystem" initSubSystem :: InitFlag -> IO CInt
foreign import ccall "SDL.h SDL_Quit" quit :: IO ()
foreign import ccall "SDL.h SDL_QuitSubSystem" quitSubSystem :: InitFlag -> IO ()
foreign import ccall "SDL.h SDL_SetMainReady" setMainReady :: IO ()
foreign import ccall "SDL.h SDL_WasInit" wasInit :: InitFlag -> IO Word32

foreign import ccall "SDL.h SDL_AddHintCallback" addHintCallback :: CString -> HintCallback -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_ClearHints" clearHints :: IO ()
foreign import ccall "SDL.h SDL_DelHintCallback" delHintCallback :: CString -> HintCallback -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_GetHint" getHint :: CString -> IO CString
foreign import ccall "SDL.h SDL_SetHint" setHint :: CString -> CString -> IO Bool
foreign import ccall "SDL.h SDL_SetHintWithPriority" setHintWithPriority :: CString -> CString -> HintPriority -> IO Bool

foreign import ccall "SDL.h SDL_ClearError" clearError :: IO ()
foreign import ccall "SDL.h SDL_GetError" getError :: IO CString
foreign import ccall "sdlhelper.c SDLHelper_SetError" setError :: CString -> IO CInt

foreign import ccall "SDL.h SDL_LogGetOutputFunction" logGetOutputFunction :: Ptr LogOutputFunction -> Ptr (Ptr ()) -> IO ()
foreign import ccall "SDL.h SDL_LogGetPriority" logGetPriority :: CInt -> IO LogPriority
foreign import ccall "sdlhelper.c SDLHelper_LogMessage" logMessage :: CInt -> LogPriority -> CString -> IO ()
foreign import ccall "SDL.h SDL_LogResetPriorities" logResetPriorities :: IO ()
foreign import ccall "SDL.h SDL_LogSetAllPriority" logSetAllPriority :: LogPriority -> IO ()
foreign import ccall "SDL.h SDL_LogSetOutputFunction" logSetOutputFunction :: LogOutputFunction -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_LogSetPriority" logSetPriority :: CInt -> LogPriority -> IO ()

foreign import ccall "SDL.h SDL_GetRevision" getRevision :: IO CString
foreign import ccall "SDL.h SDL_GetRevisionNumber" getRevisionNumber :: IO CInt
foreign import ccall "SDL.h SDL_GetVersion" getVersion :: Ptr Version -> IO ()

log :: CString -> IO ()
log = logMessage SDL_LOG_CATEGORY_APPLICATION SDL_LOG_PRIORITY_INFO

logCritical :: CInt -> CString -> IO ()
logCritical category = logMessage category SDL_LOG_PRIORITY_CRITICAL

logDebug :: CInt -> CString -> IO ()
logDebug category = logMessage category SDL_LOG_PRIORITY_DEBUG

logError :: CInt -> CString -> IO ()
logError category = logMessage category SDL_LOG_PRIORITY_ERROR

logInfo :: CInt -> CString -> IO ()
logInfo category = logMessage category SDL_LOG_PRIORITY_INFO

logVerbose :: CInt -> CString -> IO ()
logVerbose category = logMessage category SDL_LOG_PRIORITY_VERBOSE

logWarn :: CInt -> CString -> IO ()
logWarn category = logMessage category SDL_LOG_PRIORITY_WARN
