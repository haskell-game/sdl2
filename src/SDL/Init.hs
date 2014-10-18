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

init :: Foldable f => f InitFlag -> IO ()
init flags =
  SDLEx.throwIfNeg_ "SDL.Init.init" "SDL_Init" $
    Raw.init (foldFlags initFlagToC flags)

initSubSystem :: Foldable f => f InitFlag -> IO ()
initSubSystem flags =
  SDLEx.throwIfNeg_ "SDL.Init.initSubSystem" "SDL_InitSubSystem" $
    Raw.initSubSystem (foldFlags initFlagToC flags)
