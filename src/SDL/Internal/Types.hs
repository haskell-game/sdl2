module SDL.Internal.Types (WindowID(..)) where

import Foreign

newtype WindowID = WindowID Word32
  deriving (Eq,Ord,Show)
