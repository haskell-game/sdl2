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
	-- setError,

	-- * Log Handling
	-- log,
	-- logCritical,
	-- logDebug,
	-- logError,
	logGetOutputFunction,
	logGetPriority,
	-- logInfo,
	-- logMessage,
	-- logMessageV,
	logResetPriorities,
	logSetAllPriority,
	logSetOutputFunction,
	logSetPriority,
	-- logVerbose,
	-- logWarn,

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
import Prelude hiding (init)

foreign import ccall "SDL.h SDL_Init" init :: Word32 -> IO CInt
foreign import ccall "SDL.h SDL_InitSubSystem" initSubSystem :: Word32 -> IO CInt
foreign import ccall "SDL.h SDL_Quit" quit :: IO ()
foreign import ccall "SDL.h SDL_QuitSubSystem" quitSubSystem :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_SetMainReady" setMainReady :: IO ()
foreign import ccall "SDL.h SDL_WasInit" wasInit :: Word32 -> IO Word32

foreign import ccall "SDL.h SDL_AddHintCallback" addHintCallback :: CString -> FunPtr (Ptr () -> CString -> CString -> CString -> IO ()) -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_ClearHints" clearHints :: IO ()
foreign import ccall "SDL.h SDL_DelHintCallback" delHintCallback :: CString -> FunPtr (Ptr () -> CString -> CString -> CString -> IO ()) -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_GetHint" getHint :: CString -> IO CString
foreign import ccall "SDL.h SDL_SetHint" setHint :: CString -> CString -> IO Bool
foreign import ccall "SDL.h SDL_SetHintWithPriority" setHintWithPriority :: CString -> CString -> HintPriority -> IO Bool

foreign import ccall "SDL.h SDL_ClearError" clearError :: IO ()
foreign import ccall "SDL.h SDL_GetError" getError :: IO CString

foreign import ccall "SDL.h SDL_LogGetOutputFunction" logGetOutputFunction :: Ptr LogOutputFunction -> Ptr (Ptr ()) -> IO ()
foreign import ccall "SDL.h SDL_LogGetPriority" logGetPriority :: CInt -> IO LogPriority
foreign import ccall "SDL.h SDL_LogResetPriorities" logResetPriorities :: IO ()
foreign import ccall "SDL.h SDL_LogSetAllPriority" logSetAllPriority :: LogPriority -> IO ()
foreign import ccall "SDL.h SDL_LogSetOutputFunction" logSetOutputFunction :: LogOutputFunction -> Ptr () -> IO ()
foreign import ccall "SDL.h SDL_LogSetPriority" logSetPriority :: CInt -> LogPriority -> IO ()

foreign import ccall "SDL.h SDL_GetRevision" getRevision :: IO CString
foreign import ccall "SDL.h SDL_GetRevisionNumber" getRevisionNumber :: IO CInt
foreign import ccall "SDL.h SDL_GetVersion" getVersion :: Ptr () -> IO ()
