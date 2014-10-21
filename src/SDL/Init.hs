{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module SDL.Init
  ( init
  , initSubSystem
  , InitFlag(..)
  , quit
  , version
  ) where

import Prelude hiding (init)

import Data.Bitmask (foldFlags)
import Data.Foldable
import Foreign
import SDL.Internal.Numbered

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

instance ToNumber InitFlag Word32 where
  toNumber InitTimer = Raw.initFlagTimer
  toNumber InitAudio = Raw.initFlagAudio
  toNumber InitVideo = Raw.initFlagVideo
  toNumber InitJoystick = Raw.initFlagJoystick
  toNumber InitHaptic = Raw.initFlagHaptic
  toNumber InitGameController = Raw.initFlagGameController
  toNumber InitEvents = Raw.initFlagEvents
  toNumber InitEverything = Raw.initFlagEverything
  toNumber InitNoParachute = Raw.initFlagNoParachute

-- | Initializes SDL and the given subsystems. Do not call any SDL functions
-- prior to this one, unless otherwise documented that you may do so.
--
-- Throws 'SDLEx.SDLException' if initialization fails.
init :: Foldable f => f InitFlag -> IO ()
init flags =
  SDLEx.throwIfNeg_ "SDL.Init.init" "SDL_Init" $
    Raw.init (foldFlags toNumber flags)

-- | Initialize individual subsystems. SDL needs to be initializied prior
-- to calls to this function.
--
-- Throws 'SDLEx.SDLException' if initialization fails.
initSubSystem :: Foldable f => f InitFlag -> IO ()
initSubSystem flags =
  SDLEx.throwIfNeg_ "SDL.Init.initSubSystem" "SDL_InitSubSystem" $
    Raw.initSubSystem (foldFlags toNumber flags)

-- | Quit and shutdown SDL, freeing any resources that may have been in use.
-- Do not call any SDL functions after you've called this function, unless
-- otherwise documented that you may do so.
quit :: IO ()
quit = Raw.quit

-- | The major, minor, and patch versions of the SDL library linked with.
-- Does not require initialization.
version :: Integral a => IO (a, a, a)
version = do
	Raw.Version major minor patch <- alloca $ \v -> Raw.getVersion v >> peek v
	return (fromIntegral major, fromIntegral minor, fromIntegral patch)
