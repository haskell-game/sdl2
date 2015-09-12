{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module SDL.Init
  ( initialize
  , initializeAll
  , InitFlag(..)
  , quit
  , version
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bitmask (foldFlags)
import Data.Data (Data)
import Data.Typeable
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Storable
import GHC.Generics
import SDL.Exception
import SDL.Internal.Numbered
import qualified SDL.Raw as Raw

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable
#endif

{-# DEPRECATED InitEverything "Instead of initialize [InitEverything], use initializeAll" #-}
data InitFlag
  = InitTimer
  | InitAudio
  | InitVideo
  | InitJoystick
  | InitHaptic
  | InitGameController
  | InitEvents
  | InitEverything
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

instance ToNumber InitFlag Word32 where
  toNumber InitTimer = Raw.SDL_INIT_TIMER
  toNumber InitAudio = Raw.SDL_INIT_AUDIO
  toNumber InitVideo = Raw.SDL_INIT_VIDEO
  toNumber InitJoystick = Raw.SDL_INIT_JOYSTICK
  toNumber InitHaptic = Raw.SDL_INIT_HAPTIC
  toNumber InitGameController = Raw.SDL_INIT_GAMECONTROLLER
  toNumber InitEvents = Raw.SDL_INIT_EVENTS
  toNumber InitEverything = Raw.SDL_INIT_EVERYTHING

-- | Initializes SDL and the given subsystems. Do not call any SDL functions
-- prior to this one, unless otherwise documented that you may do so.
--
-- You may call this function again with additional subsystems to initialize.
--
-- Throws 'SDLEx.SDLException' if initialization fails.
initialize :: (Foldable f, Functor m, MonadIO m) => f InitFlag -> m ()
initialize flags =
  throwIfNeg_ "SDL.Init.init" "SDL_Init" $
    Raw.init (foldFlags toNumber flags)

-- | Equivalent to @'initialize' ['minBound' .. 'maxBound']@.
initializeAll :: (Functor m, MonadIO m) => m ()
initializeAll = initialize [minBound .. maxBound]

-- | Quit and shutdown SDL, freeing any resources that may have been in use.
-- Do not call any SDL functions after you've called this function, unless
-- otherwise documented that you may do so.
quit :: MonadIO m => m ()
quit = Raw.quit

-- | The major, minor, and patch versions of the SDL library linked with.
-- Does not require initialization.
version :: (Integral a, MonadIO m) => m (a, a, a)
version = liftIO $ do
  Raw.Version major minor patch <- alloca $ \v -> Raw.getVersion v >> peek v
  return (fromIntegral major, fromIntegral minor, fromIntegral patch)
