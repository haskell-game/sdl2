module SDL.Raw.Enum.Internal (
	-- * Enumerations
	AudioStatus,
	BlendMode,
	EventAction,
	GameControllerAxis,
	GameControllerButton,
	GLattr,
	HintPriority,
	InitFlag,
	Keycode,
	Keymod,
	LogPriority,
	PowerState,
	RendererFlip,
	Scancode,
	SystemCursor,
	ThreadPriority
) where

#include "SDL.h"

import Data.Int
import Data.Word

type AudioStatus = (#type SDL_AudioStatus)
type BlendMode = (#type SDL_BlendMode)
type EventAction = (#type SDL_eventaction)
type GameControllerAxis = (#type SDL_GameControllerAxis)
type GameControllerButton = (#type SDL_GameControllerButton)
type GLattr = (#type SDL_GLattr)
type HintPriority = (#type SDL_HintPriority)
type InitFlag = Word32
type Keycode = (#type SDL_Keycode)
type Keymod = (#type SDL_Keymod)
type LogPriority = (#type SDL_LogPriority)
type PowerState = (#type SDL_PowerState)
type RendererFlip = (#type SDL_RendererFlip)
type Scancode = (#type SDL_Scancode)
type SystemCursor = (#type SDL_SystemCursor)
type ThreadPriority = (#type SDL_ThreadPriority)
