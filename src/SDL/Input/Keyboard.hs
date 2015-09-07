{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SDL.Input.Keyboard
  ( -- * Keyboard Modifiers
    getModState
  , KeyModifier(..)

  , getKeyboardState

  -- * Text Input
  , startTextInput
  , stopTextInput

  -- * Screen Keyboard
  , hasScreenKeyboardSupport
  , isScreenKeyboardShown

  -- * Scancodes
  , getScancodeName
  , Scancode(..)

  -- * Keycodes
  , Keycode(..)

  -- * Keysym
  , Keysym(..)

  -- * Keycodes and Scancodes
  , module SDL.Input.Keyboard.Codes
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits
import Data.Data (Data)
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Generics (Generic)
import SDL.Input.Keyboard.Codes
import SDL.Internal.Numbered
import SDL.Internal.Types
import qualified Data.Vector as V
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Get the current key modifier state for the keyboard. The key modifier state is a mask special keys that are held down.
--
-- See @<https://wiki.libsdl.org/SDL_GetModState SDL_GetModState>@ for C documentation.
getModState :: (Functor m, MonadIO m) => m KeyModifier
getModState = fromNumber <$> Raw.getModState

-- | Information about which keys are currently held down. Use 'getModState' to generate this information.
data KeyModifier = KeyModifier
  { keyModifierLeftShift  :: Bool
  , keyModifierRightShift :: Bool
  , keyModifierLeftCtrl   :: Bool
  , keyModifierRightCtrl  :: Bool
  , keyModifierLeftAlt    :: Bool
  , keyModifierRightAlt   :: Bool
  , keyModifierLeftGUI    :: Bool
  , keyModifierRightGUI   :: Bool
  , keyModifierNumLock    :: Bool
  , keyModifierCapsLock   :: Bool
  , keyModifierAltGr      :: Bool
  } deriving (Data, Eq, Ord, Read, Generic, Show, Typeable)

instance FromNumber KeyModifier Word32 where
  fromNumber m' = let m = m' in KeyModifier
    { keyModifierLeftShift  = m .&. Raw.KMOD_LSHIFT > 0
    , keyModifierRightShift = m .&. Raw.KMOD_RSHIFT > 0
    , keyModifierLeftCtrl   = m .&. Raw.KMOD_LCTRL  > 0
    , keyModifierRightCtrl  = m .&. Raw.KMOD_RCTRL  > 0
    , keyModifierLeftAlt    = m .&. Raw.KMOD_LALT   > 0
    , keyModifierRightAlt   = m .&. Raw.KMOD_RALT   > 0
    , keyModifierLeftGUI    = m .&. Raw.KMOD_LGUI   > 0
    , keyModifierRightGUI   = m .&. Raw.KMOD_RGUI   > 0
    , keyModifierNumLock    = m .&. Raw.KMOD_NUM    > 0
    , keyModifierCapsLock   = m .&. Raw.KMOD_CAPS   > 0
    , keyModifierAltGr      = m .&. Raw.KMOD_MODE   > 0
    }

instance ToNumber KeyModifier Word32 where
  toNumber m = foldr (.|.) 0
    [ if keyModifierLeftShift m  then Raw.KMOD_LSHIFT else 0
    , if keyModifierRightShift m then Raw.KMOD_RSHIFT else 0
    , if keyModifierLeftCtrl m   then Raw.KMOD_LCTRL  else 0
    , if keyModifierRightCtrl m  then Raw.KMOD_RCTRL  else 0
    , if keyModifierLeftAlt m    then Raw.KMOD_LALT   else 0
    , if keyModifierRightAlt m   then Raw.KMOD_RALT   else 0
    , if keyModifierLeftGUI m    then Raw.KMOD_LGUI   else 0
    , if keyModifierRightGUI m   then Raw.KMOD_RGUI   else 0
    , if keyModifierNumLock m    then Raw.KMOD_NUM    else 0
    , if keyModifierCapsLock m   then Raw.KMOD_CAPS   else 0
    , if keyModifierAltGr m      then Raw.KMOD_MODE   else 0
    ]

-- | Set the rectangle used to type text inputs and start accepting text input
-- events.
--
-- See @<https://wiki.libsdl.org/SDL_StartTextInput SDL_StartTextInput>@ for C documentation.
startTextInput :: MonadIO m => Raw.Rect -> m ()
startTextInput rect = liftIO $ do
  alloca $ \ptr -> do
    poke ptr rect
    Raw.setTextInputRect ptr
  Raw.startTextInput

-- | Stop receiving any text input events.
--
-- See @<https://wiki.libsdl.org/SDL_StopTextInput SDL_StopTextInput>@ for C documentation.
stopTextInput :: MonadIO m => m ()
stopTextInput = Raw.stopTextInput

-- | Check whether the platform has screen keyboard support.
--
-- See @<https://wiki.libsdl.org/SDL_HasScreenKeyboardSupport SDL_HasScreenKeyboardSupport>@ for C documentation.
hasScreenKeyboardSupport :: MonadIO m => m Bool
hasScreenKeyboardSupport = Raw.hasScreenKeyboardSupport

-- | Check whether the screen keyboard is shown for the given window.
--
-- See @<https://wiki.libsdl.org/SDL_IsScreenKeyboardShown SDL_IsScreenKeyboardShown>@ for C documentation.
isScreenKeyboardShown :: MonadIO m => Window -> m Bool
isScreenKeyboardShown (Window w) = Raw.isScreenKeyboardShown w

-- | Get a human-readable name for a scancode. If the scancode doesn't have a name this function returns the empty string.
--
-- See @<https://wiki.libsdl.org/SDL_GetScancodeName SDL_GetScancodeName>@ for C documentation.
getScancodeName :: MonadIO m => Scancode -> m String
getScancodeName scancode = liftIO $ do
  name <- Raw.getScancodeName $ toNumber scancode
  peekCString name

-- | Information about a key press or key release event.
data Keysym = Keysym
  { keysymScancode :: Scancode
    -- ^ The keyboard 'Scancode'
  , keysymKeycode  :: Keycode
    -- ^ SDL's virtual key representation for this key
  , keysymModifier :: KeyModifier
    -- ^ A set of modifiers that were held at the time this data was generated
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Get a snapshot of the current state of the keyboard.
--
-- This computation generates a mapping from 'Scancode' to 'Bool' - evaluating the function at specific 'Scancode's will inform you as to whether or not that key was held down when 'getKeyboardState' was called.
--
-- See @<https://wiki.libsdl.org/SDL_GetKeyboardState SDL_GetKeyboardState>@ for C documentation.
getKeyboardState :: MonadIO m => m (Scancode -> Bool)
getKeyboardState = liftIO $ do
  alloca $ \nkeys -> do
    keyptr <- Raw.getKeyboardState nkeys
    n <- peek nkeys
    keys <- V.fromList <$> peekArray (fromIntegral n) keyptr
    return $ \scancode -> 1 == keys V.! fromIntegral (toNumber scancode)
