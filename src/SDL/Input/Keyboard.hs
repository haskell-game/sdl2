{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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

  -- * Keycodes and scancodes
  , module SDL.Input.Keyboard.Codes
) where

import Control.Applicative
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

-- | Get the current key modifier state for the keyboard.
getModState :: (Functor m, MonadIO m) => m KeyModifier
getModState = fromNumber <$> Raw.getModState

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
startTextInput :: MonadIO m => Raw.Rect -> m ()
startTextInput rect = liftIO $ do
  alloca $ \ptr -> do
    poke ptr rect
    Raw.setTextInputRect ptr
  Raw.startTextInput

-- | Stop receiving any text input events.
stopTextInput :: MonadIO m => m ()
stopTextInput = Raw.stopTextInput

-- | Check whether the platform has screen keyboard support.
hasScreenKeyboardSupport :: MonadIO m => m Bool
hasScreenKeyboardSupport = Raw.hasScreenKeyboardSupport

-- | Check whether the screen keyboard is shown for the given window.
isScreenKeyboardShown :: MonadIO m => Window -> m Bool
isScreenKeyboardShown (Window w) = Raw.isScreenKeyboardShown w

getScancodeName :: MonadIO m => Scancode -> m String
getScancodeName scancode = liftIO $ do
  name <- Raw.getScancodeName $ toNumber scancode
  peekCString name

data Keysym = Keysym
  { keysymScancode :: Scancode
  , keysymKeycode  :: Keycode
  , keysymModifier :: KeyModifier
  } deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

getKeyboardState :: MonadIO m => m (Scancode -> Bool)
getKeyboardState = liftIO $ do
  alloca $ \nkeys -> do
    keyptr <- Raw.getKeyboardState nkeys
    n <- peek nkeys
    keys <- V.fromList <$> peekArray (fromIntegral n) keyptr
    return $ \scancode -> 1 == keys V.! fromIntegral (toNumber scancode)
