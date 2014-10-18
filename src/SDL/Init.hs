{-# LANGUAGE OverloadedStrings #-}
module SDL.Init (init, initSubSystem, InitFlag(..)) where

import Prelude hiding (init)

import Data.Bitmask (foldFlags)
import Data.Foldable
import Foreign

import qualified SDL.Exception as SDLEx
import qualified SDL.Raw as Raw

data InitFlag
  = InitTimer
  | InitAudio
  | InitVideo
  | InitJoystick
  | InitHaptic
  | InitGameController
  | InitEvents
  | InitEverything
  | InitNoParachute
  deriving (Eq,Ord,Show,Read,Bounded,Enum)

initFlagToC :: InitFlag -> Word32
initFlagToC InitTimer = Raw.initFlagTimer
initFlagToC InitAudio = Raw.initFlagAudio
initFlagToC InitVideo = Raw.initFlagVideo
initFlagToC InitJoystick = Raw.initFlagJoystick
initFlagToC InitHaptic = Raw.initFlagHaptic
initFlagToC InitGameController = Raw.initFlagGameController
initFlagToC InitEvents = Raw.initFlagEvents
initFlagToC InitEverything = Raw.initFlagEverything
initFlagToC InitNoParachute = Raw.initFlagNoParachute

-- | Initializes SDL and the given subsystems. Do not call any SDL functions
-- prior to this one, unless otherwise documented that you may do so.
--
-- Throws 'SDLEx.SDLException' if initialization fails.
init :: Foldable f => f InitFlag -> IO ()
init flags =
  SDLEx.throwIfNeg_ "SDL.Init.init" "SDL_Init" $
    Raw.init (foldFlags initFlagToC flags)

-- | Initialize individual subsystems. SDL needs to be initializied prior
-- to calls to this function.
--
-- Throws 'SDLEx.SDLException' if initialization fails.
initSubSystem :: Foldable f => f InitFlag -> IO ()
initSubSystem flags =
  SDLEx.throwIfNeg_ "SDL.Init.initSubSystem" "SDL_InitSubSystem" $
    Raw.initSubSystem (foldFlags initFlagToC flags)
