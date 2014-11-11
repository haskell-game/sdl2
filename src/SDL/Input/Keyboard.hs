{-# LANGUAGE DeriveDataTypeable #-}
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
) where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import SDL.Internal.Numbered
import SDL.Internal.Types

import qualified Data.Vector as V
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

-- | Get the current key modifier state for the keyboard.
getModState :: IO KeyModifier
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
  } deriving (Eq, Show, Typeable)

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
startTextInput :: Raw.Rect -> IO ()
startTextInput rect = do
  alloca $ \ptr -> do
    poke ptr rect
    Raw.setTextInputRect ptr
  Raw.startTextInput

-- | Stop receiving any text input events.
stopTextInput :: IO ()
stopTextInput = Raw.stopTextInput

-- | Check whether the platform has screen keyboard support.
hasScreenKeyboardSupport :: IO Bool
hasScreenKeyboardSupport = Raw.hasScreenKeyboardSupport

-- | Check whether the screen keyboard is shown for the given window.
isScreenKeyboardShown :: Window -> IO Bool
isScreenKeyboardShown (Window w) = Raw.isScreenKeyboardShown w

getScancodeName :: Scancode -> IO String
getScancodeName scancode = do
  name <- Raw.getScancodeName $ toNumber scancode
  peekCString name

data Scancode
  = ScancodeUnknown
  | ScancodeA
  | ScancodeB
  | ScancodeC
  | ScancodeD
  | ScancodeE
  | ScancodeF
  | ScancodeG
  | ScancodeH
  | ScancodeI
  | ScancodeJ
  | ScancodeK
  | ScancodeL
  | ScancodeM
  | ScancodeN
  | ScancodeO
  | ScancodeP
  | ScancodeQ
  | ScancodeR
  | ScancodeS
  | ScancodeT
  | ScancodeU
  | ScancodeV
  | ScancodeW
  | ScancodeX
  | ScancodeY
  | ScancodeZ
  | Scancode1
  | Scancode2
  | Scancode3
  | Scancode4
  | Scancode5
  | Scancode6
  | Scancode7
  | Scancode8
  | Scancode9
  | Scancode0
  | ScancodeReturn
  | ScancodeEscape
  | ScancodeBackspace
  | ScancodeTab
  | ScancodeSpace
  | ScancodeMinus
  | ScancodeEquals
  | ScancodeLeftBracket
  | ScancodeRightBracket
  | ScancodeBackslash
  | ScancodeNonUSHash
  | ScancodeSemicolon
  | ScancodeApostrophe
  | ScancodeGrave
  | ScancodeComma
  | ScancodePeriod
  | ScancodeSlash
  | ScancodeCapsLock
  | ScancodeF1
  | ScancodeF2
  | ScancodeF3
  | ScancodeF4
  | ScancodeF5
  | ScancodeF6
  | ScancodeF7
  | ScancodeF8
  | ScancodeF9
  | ScancodeF10
  | ScancodeF11
  | ScancodeF12
  | ScancodePrintScreen
  | ScancodeScrollLock
  | ScancodePause
  | ScancodeInsert
  | ScancodeHome
  | ScancodePageUp
  | ScancodeDelete
  | ScancodeEnd
  | ScancodePageDown
  | ScancodeRight
  | ScancodeLeft
  | ScancodeDown
  | ScancodeUp
  | ScancodeNumLockClear
  | ScancodeKPDivide
  | ScancodeKPMultiply
  | ScancodeKPMinus
  | ScancodeKPPlus
  | ScancodeKPEnter
  | ScancodeKP1
  | ScancodeKP2
  | ScancodeKP3
  | ScancodeKP4
  | ScancodeKP5
  | ScancodeKP6
  | ScancodeKP7
  | ScancodeKP8
  | ScancodeKP9
  | ScancodeKP0
  | ScancodeKPPeriod
  | ScancodeNonUSBackslash
  | ScancodeApplication
  | ScancodePower
  | ScancodeKPEquals
  | ScancodeF13
  | ScancodeF14
  | ScancodeF15
  | ScancodeF16
  | ScancodeF17
  | ScancodeF18
  | ScancodeF19
  | ScancodeF20
  | ScancodeF21
  | ScancodeF22
  | ScancodeF23
  | ScancodeF24
  | ScancodeExecute
  | ScancodeHelp
  | ScancodeMenu
  | ScancodeSelect
  | ScancodeStop
  | ScancodeAgain
  | ScancodeUndo
  | ScancodeCut
  | ScancodeCopy
  | ScancodePaste
  | ScancodeFind
  | ScancodeMute
  | ScancodeVolumeUp
  | ScancodeVolumeDown
  | ScancodeKPComma
  | ScancodeKPEqualsAS400
  | ScancodeInternational1
  | ScancodeInternational2
  | ScancodeInternational3
  | ScancodeInternational4
  | ScancodeInternational5
  | ScancodeInternational6
  | ScancodeInternational7
  | ScancodeInternational8
  | ScancodeInternational9
  | ScancodeLang1
  | ScancodeLang2
  | ScancodeLang3
  | ScancodeLang4
  | ScancodeLang5
  | ScancodeLang6
  | ScancodeLang7
  | ScancodeLang8
  | ScancodeLang9
  | ScancodeAltErase
  | ScancodeSysReq
  | ScancodeCancel
  | ScancodeClear
  | ScancodePrior
  | ScancodeReturn2
  | ScancodeSeparator
  | ScancodeOut
  | ScancodeOper
  | ScancodeClearAgain
  | ScancodeCrSel
  | ScancodeExSel
  | ScancodeKP00
  | ScancodeKP000
  | ScancodeThousandsSeparator
  | ScancodeDecimalSeparator
  | ScancodeCurrencyUnit
  | ScancodeCurrencySubunit
  | ScancodeLeftParen
  | ScancodeRightParen
  | ScancodeLeftBrace
  | ScancodeRightBrace
  | ScancodeKPTab
  | ScancodeKPBackspace
  | ScancodeKPA
  | ScancodeKPB
  | ScancodeKPC
  | ScancodeKPD
  | ScancodeKPE
  | ScancodeKPF
  | ScancodeKPXOR
  | ScancodeKPPower
  | ScancodeKPPercent
  | ScancodeKPLess
  | ScancodeKPGreater
  | ScancodeKPAmpersand
  | ScancodeKPDblAmpersand
  | ScancodeKPVerticalBar
  | ScancodeKPDblVerticalBar
  | ScancodeKPColon
  | ScancodeKPHash
  | ScancodeKPSpace
  | ScancodeKPAt
  | ScancodeKPExclam
  | ScancodeKPMemStore
  | ScancodeKPMemRecall
  | ScancodeKPMemClear
  | ScancodeKPMemAdd
  | ScancodeKPMemSubtract
  | ScancodeKPMemMultiply
  | ScancodeKPMemDivide
  | ScancodeKPPlusMinus
  | ScancodeKPClear
  | ScancodeKPClearEntry
  | ScancodeKPBinary
  | ScancodeKPOctal
  | ScancodeKPDecimal
  | ScancodeKPHexadecimal
  | ScancodeLCtrl
  | ScancodeLShift
  | ScancodeLAlt
  | ScancodeLGUI
  | ScancodeRCtrl
  | ScancodeRShift
  | ScancodeRAlt
  | ScancodeRGUI
  | ScancodeMode
  | ScancodeAudioNext
  | ScancodeAudioPrev
  | ScancodeAudioStop
  | ScancodeAudioPlay
  | ScancodeAudioMute
  | ScancodeMediaSelect
  | ScancodeWWW
  | ScancodeMail
  | ScancodeCalculator
  | ScancodeComputer
  | ScancodeACSearch
  | ScancodeACHome
  | ScancodeACBack
  | ScancodeACForward
  | ScancodeACStop
  | ScancodeACRefresh
  | ScancodeACBookmarks
  | ScancodeBrightnessDown
  | ScancodeBrightnessUp
  | ScancodeDisplaySwitch
  | ScancodeKBDIllumToggle
  | ScancodeKBDIllumDown
  | ScancodeKBDIllumUp
  | ScancodeEject
  | ScancodeSleep
  | ScancodeApp1
  | ScancodeApp2
  deriving (Eq, Ord, Show, Typeable)

instance FromNumber Scancode Word32 where
  fromNumber n' = case n' of
    Raw.SDL_SCANCODE_UNKNOWN -> ScancodeUnknown
    Raw.SDL_SCANCODE_A -> ScancodeA
    Raw.SDL_SCANCODE_B -> ScancodeB
    Raw.SDL_SCANCODE_C -> ScancodeC
    Raw.SDL_SCANCODE_D -> ScancodeD
    Raw.SDL_SCANCODE_E -> ScancodeE
    Raw.SDL_SCANCODE_F -> ScancodeF
    Raw.SDL_SCANCODE_G -> ScancodeG
    Raw.SDL_SCANCODE_H -> ScancodeH
    Raw.SDL_SCANCODE_I -> ScancodeI
    Raw.SDL_SCANCODE_J -> ScancodeJ
    Raw.SDL_SCANCODE_K -> ScancodeK
    Raw.SDL_SCANCODE_L -> ScancodeL
    Raw.SDL_SCANCODE_M -> ScancodeM
    Raw.SDL_SCANCODE_N -> ScancodeN
    Raw.SDL_SCANCODE_O -> ScancodeO
    Raw.SDL_SCANCODE_P -> ScancodeP
    Raw.SDL_SCANCODE_Q -> ScancodeQ
    Raw.SDL_SCANCODE_R -> ScancodeR
    Raw.SDL_SCANCODE_S -> ScancodeS
    Raw.SDL_SCANCODE_T -> ScancodeT
    Raw.SDL_SCANCODE_U -> ScancodeU
    Raw.SDL_SCANCODE_V -> ScancodeV
    Raw.SDL_SCANCODE_W -> ScancodeW
    Raw.SDL_SCANCODE_X -> ScancodeX
    Raw.SDL_SCANCODE_Y -> ScancodeY
    Raw.SDL_SCANCODE_Z -> ScancodeZ
    Raw.SDL_SCANCODE_1 -> Scancode1
    Raw.SDL_SCANCODE_2 -> Scancode2
    Raw.SDL_SCANCODE_3 -> Scancode3
    Raw.SDL_SCANCODE_4 -> Scancode4
    Raw.SDL_SCANCODE_5 -> Scancode5
    Raw.SDL_SCANCODE_6 -> Scancode6
    Raw.SDL_SCANCODE_7 -> Scancode7
    Raw.SDL_SCANCODE_8 -> Scancode8
    Raw.SDL_SCANCODE_9 -> Scancode9
    Raw.SDL_SCANCODE_0 -> Scancode0
    Raw.SDL_SCANCODE_RETURN -> ScancodeReturn
    Raw.SDL_SCANCODE_ESCAPE -> ScancodeEscape
    Raw.SDL_SCANCODE_BACKSPACE -> ScancodeBackspace
    Raw.SDL_SCANCODE_TAB -> ScancodeTab
    Raw.SDL_SCANCODE_SPACE -> ScancodeSpace
    Raw.SDL_SCANCODE_MINUS -> ScancodeMinus
    Raw.SDL_SCANCODE_EQUALS -> ScancodeEquals
    Raw.SDL_SCANCODE_LEFTBRACKET -> ScancodeLeftBracket
    Raw.SDL_SCANCODE_RIGHTBRACKET -> ScancodeRightBracket
    Raw.SDL_SCANCODE_BACKSLASH -> ScancodeBackslash
    Raw.SDL_SCANCODE_NONUSHASH -> ScancodeNonUSHash
    Raw.SDL_SCANCODE_SEMICOLON -> ScancodeSemicolon
    Raw.SDL_SCANCODE_APOSTROPHE -> ScancodeApostrophe
    Raw.SDL_SCANCODE_GRAVE -> ScancodeGrave
    Raw.SDL_SCANCODE_COMMA -> ScancodeComma
    Raw.SDL_SCANCODE_PERIOD -> ScancodePeriod
    Raw.SDL_SCANCODE_SLASH -> ScancodeSlash
    Raw.SDL_SCANCODE_CAPSLOCK -> ScancodeCapsLock
    Raw.SDL_SCANCODE_F1 -> ScancodeF1
    Raw.SDL_SCANCODE_F2 -> ScancodeF2
    Raw.SDL_SCANCODE_F3 -> ScancodeF3
    Raw.SDL_SCANCODE_F4 -> ScancodeF4
    Raw.SDL_SCANCODE_F5 -> ScancodeF5
    Raw.SDL_SCANCODE_F6 -> ScancodeF6
    Raw.SDL_SCANCODE_F7 -> ScancodeF7
    Raw.SDL_SCANCODE_F8 -> ScancodeF8
    Raw.SDL_SCANCODE_F9 -> ScancodeF9
    Raw.SDL_SCANCODE_F10 -> ScancodeF10
    Raw.SDL_SCANCODE_F11 -> ScancodeF11
    Raw.SDL_SCANCODE_F12 -> ScancodeF12
    Raw.SDL_SCANCODE_PRINTSCREEN -> ScancodePrintScreen
    Raw.SDL_SCANCODE_SCROLLLOCK -> ScancodeScrollLock
    Raw.SDL_SCANCODE_PAUSE -> ScancodePause
    Raw.SDL_SCANCODE_INSERT -> ScancodeInsert
    Raw.SDL_SCANCODE_HOME -> ScancodeHome
    Raw.SDL_SCANCODE_PAGEUP -> ScancodePageUp
    Raw.SDL_SCANCODE_DELETE -> ScancodeDelete
    Raw.SDL_SCANCODE_END -> ScancodeEnd
    Raw.SDL_SCANCODE_PAGEDOWN -> ScancodePageDown
    Raw.SDL_SCANCODE_RIGHT -> ScancodeRight
    Raw.SDL_SCANCODE_LEFT -> ScancodeLeft
    Raw.SDL_SCANCODE_DOWN -> ScancodeDown
    Raw.SDL_SCANCODE_UP -> ScancodeUp
    Raw.SDL_SCANCODE_NUMLOCKCLEAR -> ScancodeNumLockClear
    Raw.SDL_SCANCODE_KP_DIVIDE -> ScancodeKPDivide
    Raw.SDL_SCANCODE_KP_MULTIPLY -> ScancodeKPMultiply
    Raw.SDL_SCANCODE_KP_MINUS -> ScancodeKPMinus
    Raw.SDL_SCANCODE_KP_PLUS -> ScancodeKPPlus
    Raw.SDL_SCANCODE_KP_ENTER -> ScancodeKPEnter
    Raw.SDL_SCANCODE_KP_1 -> ScancodeKP1
    Raw.SDL_SCANCODE_KP_2 -> ScancodeKP2
    Raw.SDL_SCANCODE_KP_3 -> ScancodeKP3
    Raw.SDL_SCANCODE_KP_4 -> ScancodeKP4
    Raw.SDL_SCANCODE_KP_5 -> ScancodeKP5
    Raw.SDL_SCANCODE_KP_6 -> ScancodeKP6
    Raw.SDL_SCANCODE_KP_7 -> ScancodeKP7
    Raw.SDL_SCANCODE_KP_8 -> ScancodeKP8
    Raw.SDL_SCANCODE_KP_9 -> ScancodeKP9
    Raw.SDL_SCANCODE_KP_0 -> ScancodeKP0
    Raw.SDL_SCANCODE_KP_PERIOD -> ScancodeKPPeriod
    Raw.SDL_SCANCODE_NONUSBACKSLASH -> ScancodeNonUSBackslash
    Raw.SDL_SCANCODE_APPLICATION -> ScancodeApplication
    Raw.SDL_SCANCODE_POWER -> ScancodePower
    Raw.SDL_SCANCODE_KP_EQUALS -> ScancodeKPEquals
    Raw.SDL_SCANCODE_F13 -> ScancodeF13
    Raw.SDL_SCANCODE_F14 -> ScancodeF14
    Raw.SDL_SCANCODE_F15 -> ScancodeF15
    Raw.SDL_SCANCODE_F16 -> ScancodeF16
    Raw.SDL_SCANCODE_F17 -> ScancodeF17
    Raw.SDL_SCANCODE_F18 -> ScancodeF18
    Raw.SDL_SCANCODE_F19 -> ScancodeF19
    Raw.SDL_SCANCODE_F20 -> ScancodeF20
    Raw.SDL_SCANCODE_F21 -> ScancodeF21
    Raw.SDL_SCANCODE_F22 -> ScancodeF22
    Raw.SDL_SCANCODE_F23 -> ScancodeF23
    Raw.SDL_SCANCODE_F24 -> ScancodeF24
    Raw.SDL_SCANCODE_EXECUTE -> ScancodeExecute
    Raw.SDL_SCANCODE_HELP -> ScancodeHelp
    Raw.SDL_SCANCODE_MENU -> ScancodeMenu
    Raw.SDL_SCANCODE_SELECT -> ScancodeSelect
    Raw.SDL_SCANCODE_STOP -> ScancodeStop
    Raw.SDL_SCANCODE_AGAIN -> ScancodeAgain
    Raw.SDL_SCANCODE_UNDO -> ScancodeUndo
    Raw.SDL_SCANCODE_CUT -> ScancodeCut
    Raw.SDL_SCANCODE_COPY -> ScancodeCopy
    Raw.SDL_SCANCODE_PASTE -> ScancodePaste
    Raw.SDL_SCANCODE_FIND -> ScancodeFind
    Raw.SDL_SCANCODE_MUTE -> ScancodeMute
    Raw.SDL_SCANCODE_VOLUMEUP -> ScancodeVolumeUp
    Raw.SDL_SCANCODE_VOLUMEDOWN -> ScancodeVolumeDown
    Raw.SDL_SCANCODE_KP_COMMA -> ScancodeKPComma
    Raw.SDL_SCANCODE_KP_EQUALSAS400 -> ScancodeKPEqualsAS400
    Raw.SDL_SCANCODE_INTERNATIONAL1 -> ScancodeInternational1
    Raw.SDL_SCANCODE_INTERNATIONAL2 -> ScancodeInternational2
    Raw.SDL_SCANCODE_INTERNATIONAL3 -> ScancodeInternational3
    Raw.SDL_SCANCODE_INTERNATIONAL4 -> ScancodeInternational4
    Raw.SDL_SCANCODE_INTERNATIONAL5 -> ScancodeInternational5
    Raw.SDL_SCANCODE_INTERNATIONAL6 -> ScancodeInternational6
    Raw.SDL_SCANCODE_INTERNATIONAL7 -> ScancodeInternational7
    Raw.SDL_SCANCODE_INTERNATIONAL8 -> ScancodeInternational8
    Raw.SDL_SCANCODE_INTERNATIONAL9 -> ScancodeInternational9
    Raw.SDL_SCANCODE_LANG1 -> ScancodeLang1
    Raw.SDL_SCANCODE_LANG2 -> ScancodeLang2
    Raw.SDL_SCANCODE_LANG3 -> ScancodeLang3
    Raw.SDL_SCANCODE_LANG4 -> ScancodeLang4
    Raw.SDL_SCANCODE_LANG5 -> ScancodeLang5
    Raw.SDL_SCANCODE_LANG6 -> ScancodeLang6
    Raw.SDL_SCANCODE_LANG7 -> ScancodeLang7
    Raw.SDL_SCANCODE_LANG8 -> ScancodeLang8
    Raw.SDL_SCANCODE_LANG9 -> ScancodeLang9
    Raw.SDL_SCANCODE_ALTERASE -> ScancodeAltErase
    Raw.SDL_SCANCODE_SYSREQ -> ScancodeSysReq
    Raw.SDL_SCANCODE_CANCEL -> ScancodeCancel
    Raw.SDL_SCANCODE_CLEAR -> ScancodeClear
    Raw.SDL_SCANCODE_PRIOR -> ScancodePrior
    Raw.SDL_SCANCODE_RETURN2 -> ScancodeReturn2
    Raw.SDL_SCANCODE_SEPARATOR -> ScancodeSeparator
    Raw.SDL_SCANCODE_OUT -> ScancodeOut
    Raw.SDL_SCANCODE_OPER -> ScancodeOper
    Raw.SDL_SCANCODE_CLEARAGAIN -> ScancodeClearAgain
    Raw.SDL_SCANCODE_CRSEL -> ScancodeCrSel
    Raw.SDL_SCANCODE_EXSEL -> ScancodeExSel
    Raw.SDL_SCANCODE_KP_00 -> ScancodeKP00
    Raw.SDL_SCANCODE_KP_000 -> ScancodeKP000
    Raw.SDL_SCANCODE_THOUSANDSSEPARATOR -> ScancodeThousandsSeparator
    Raw.SDL_SCANCODE_DECIMALSEPARATOR -> ScancodeDecimalSeparator
    Raw.SDL_SCANCODE_CURRENCYUNIT -> ScancodeCurrencyUnit
    Raw.SDL_SCANCODE_CURRENCYSUBUNIT -> ScancodeCurrencySubunit
    Raw.SDL_SCANCODE_KP_LEFTPAREN -> ScancodeLeftParen
    Raw.SDL_SCANCODE_KP_RIGHTPAREN -> ScancodeRightParen
    Raw.SDL_SCANCODE_KP_LEFTBRACE -> ScancodeLeftBrace
    Raw.SDL_SCANCODE_KP_RIGHTBRACE -> ScancodeRightBrace
    Raw.SDL_SCANCODE_KP_TAB -> ScancodeKPTab
    Raw.SDL_SCANCODE_KP_BACKSPACE -> ScancodeKPBackspace
    Raw.SDL_SCANCODE_KP_A -> ScancodeKPA
    Raw.SDL_SCANCODE_KP_B -> ScancodeKPB
    Raw.SDL_SCANCODE_KP_C -> ScancodeKPC
    Raw.SDL_SCANCODE_KP_D -> ScancodeKPD
    Raw.SDL_SCANCODE_KP_E -> ScancodeKPE
    Raw.SDL_SCANCODE_KP_F -> ScancodeKPF
    Raw.SDL_SCANCODE_KP_XOR -> ScancodeKPXOR
    Raw.SDL_SCANCODE_KP_POWER -> ScancodeKPPower
    Raw.SDL_SCANCODE_KP_PERCENT -> ScancodeKPPercent
    Raw.SDL_SCANCODE_KP_LESS -> ScancodeKPLess
    Raw.SDL_SCANCODE_KP_GREATER -> ScancodeKPGreater
    Raw.SDL_SCANCODE_KP_AMPERSAND -> ScancodeKPAmpersand
    Raw.SDL_SCANCODE_KP_DBLAMPERSAND -> ScancodeKPDblAmpersand
    Raw.SDL_SCANCODE_KP_VERTICALBAR -> ScancodeKPVerticalBar
    Raw.SDL_SCANCODE_KP_DBLVERTICALBAR -> ScancodeKPDblVerticalBar
    Raw.SDL_SCANCODE_KP_COLON -> ScancodeKPColon
    Raw.SDL_SCANCODE_KP_HASH -> ScancodeKPHash
    Raw.SDL_SCANCODE_KP_SPACE -> ScancodeKPSpace
    Raw.SDL_SCANCODE_KP_AT -> ScancodeKPAt
    Raw.SDL_SCANCODE_KP_EXCLAM -> ScancodeKPExclam
    Raw.SDL_SCANCODE_KP_MEMSTORE -> ScancodeKPMemStore
    Raw.SDL_SCANCODE_KP_MEMRECALL -> ScancodeKPMemRecall
    Raw.SDL_SCANCODE_KP_MEMCLEAR -> ScancodeKPMemClear
    Raw.SDL_SCANCODE_KP_MEMADD -> ScancodeKPMemAdd
    Raw.SDL_SCANCODE_KP_MEMSUBTRACT -> ScancodeKPMemSubtract
    Raw.SDL_SCANCODE_KP_MEMMULTIPLY -> ScancodeKPMemMultiply
    Raw.SDL_SCANCODE_KP_MEMDIVIDE -> ScancodeKPMemDivide
    Raw.SDL_SCANCODE_KP_PLUSMINUS -> ScancodeKPPlusMinus
    Raw.SDL_SCANCODE_KP_CLEAR -> ScancodeKPClear
    Raw.SDL_SCANCODE_KP_CLEARENTRY -> ScancodeKPClearEntry
    Raw.SDL_SCANCODE_KP_BINARY -> ScancodeKPBinary
    Raw.SDL_SCANCODE_KP_OCTAL -> ScancodeKPOctal
    Raw.SDL_SCANCODE_KP_DECIMAL -> ScancodeKPDecimal
    Raw.SDL_SCANCODE_KP_HEXADECIMAL -> ScancodeKPHexadecimal
    Raw.SDL_SCANCODE_LCTRL -> ScancodeLCtrl
    Raw.SDL_SCANCODE_LSHIFT -> ScancodeLShift
    Raw.SDL_SCANCODE_LALT -> ScancodeLAlt
    Raw.SDL_SCANCODE_LGUI -> ScancodeLGUI
    Raw.SDL_SCANCODE_RCTRL -> ScancodeRCtrl
    Raw.SDL_SCANCODE_RSHIFT -> ScancodeRShift
    Raw.SDL_SCANCODE_RALT -> ScancodeRAlt
    Raw.SDL_SCANCODE_RGUI -> ScancodeRGUI
    Raw.SDL_SCANCODE_MODE -> ScancodeMode
    Raw.SDL_SCANCODE_AUDIONEXT -> ScancodeAudioNext
    Raw.SDL_SCANCODE_AUDIOPREV -> ScancodeAudioPrev
    Raw.SDL_SCANCODE_AUDIOSTOP -> ScancodeAudioStop
    Raw.SDL_SCANCODE_AUDIOPLAY -> ScancodeAudioPlay
    Raw.SDL_SCANCODE_AUDIOMUTE -> ScancodeAudioMute
    Raw.SDL_SCANCODE_MEDIASELECT -> ScancodeMediaSelect
    Raw.SDL_SCANCODE_WWW -> ScancodeWWW
    Raw.SDL_SCANCODE_MAIL -> ScancodeMail
    Raw.SDL_SCANCODE_CALCULATOR -> ScancodeCalculator
    Raw.SDL_SCANCODE_COMPUTER -> ScancodeComputer
    Raw.SDL_SCANCODE_AC_SEARCH -> ScancodeACSearch
    Raw.SDL_SCANCODE_AC_HOME -> ScancodeACHome
    Raw.SDL_SCANCODE_AC_BACK -> ScancodeACBack
    Raw.SDL_SCANCODE_AC_FORWARD -> ScancodeACForward
    Raw.SDL_SCANCODE_AC_STOP -> ScancodeACStop
    Raw.SDL_SCANCODE_AC_REFRESH -> ScancodeACRefresh
    Raw.SDL_SCANCODE_AC_BOOKMARKS -> ScancodeACBookmarks
    Raw.SDL_SCANCODE_BRIGHTNESSDOWN -> ScancodeBrightnessDown
    Raw.SDL_SCANCODE_BRIGHTNESSUP -> ScancodeBrightnessUp
    Raw.SDL_SCANCODE_DISPLAYSWITCH -> ScancodeDisplaySwitch
    Raw.SDL_SCANCODE_KBDILLUMTOGGLE -> ScancodeKBDIllumToggle
    Raw.SDL_SCANCODE_KBDILLUMDOWN -> ScancodeKBDIllumDown
    Raw.SDL_SCANCODE_KBDILLUMUP -> ScancodeKBDIllumUp
    Raw.SDL_SCANCODE_EJECT -> ScancodeEject
    Raw.SDL_SCANCODE_SLEEP -> ScancodeSleep
    Raw.SDL_SCANCODE_APP1 -> ScancodeApp1
    Raw.SDL_SCANCODE_APP2 -> ScancodeApp2
    _ -> error "fromNumber: not numbered"

instance ToNumber Scancode Word32 where
  toNumber ScancodeUnknown = Raw.SDL_SCANCODE_UNKNOWN
  toNumber ScancodeA = Raw.SDL_SCANCODE_A
  toNumber ScancodeB = Raw.SDL_SCANCODE_B
  toNumber ScancodeC = Raw.SDL_SCANCODE_C
  toNumber ScancodeD = Raw.SDL_SCANCODE_D
  toNumber ScancodeE = Raw.SDL_SCANCODE_E
  toNumber ScancodeF = Raw.SDL_SCANCODE_F
  toNumber ScancodeG = Raw.SDL_SCANCODE_G
  toNumber ScancodeH = Raw.SDL_SCANCODE_H
  toNumber ScancodeI = Raw.SDL_SCANCODE_I
  toNumber ScancodeJ = Raw.SDL_SCANCODE_J
  toNumber ScancodeK = Raw.SDL_SCANCODE_K
  toNumber ScancodeL = Raw.SDL_SCANCODE_L
  toNumber ScancodeM = Raw.SDL_SCANCODE_M
  toNumber ScancodeN = Raw.SDL_SCANCODE_N
  toNumber ScancodeO = Raw.SDL_SCANCODE_O
  toNumber ScancodeP = Raw.SDL_SCANCODE_P
  toNumber ScancodeQ = Raw.SDL_SCANCODE_Q
  toNumber ScancodeR = Raw.SDL_SCANCODE_R
  toNumber ScancodeS = Raw.SDL_SCANCODE_S
  toNumber ScancodeT = Raw.SDL_SCANCODE_T
  toNumber ScancodeU = Raw.SDL_SCANCODE_U
  toNumber ScancodeV = Raw.SDL_SCANCODE_V
  toNumber ScancodeW = Raw.SDL_SCANCODE_W
  toNumber ScancodeX = Raw.SDL_SCANCODE_X
  toNumber ScancodeY = Raw.SDL_SCANCODE_Y
  toNumber ScancodeZ = Raw.SDL_SCANCODE_Z
  toNumber Scancode1 = Raw.SDL_SCANCODE_1
  toNumber Scancode2 = Raw.SDL_SCANCODE_2
  toNumber Scancode3 = Raw.SDL_SCANCODE_3
  toNumber Scancode4 = Raw.SDL_SCANCODE_4
  toNumber Scancode5 = Raw.SDL_SCANCODE_5
  toNumber Scancode6 = Raw.SDL_SCANCODE_6
  toNumber Scancode7 = Raw.SDL_SCANCODE_7
  toNumber Scancode8 = Raw.SDL_SCANCODE_8
  toNumber Scancode9 = Raw.SDL_SCANCODE_9
  toNumber Scancode0 = Raw.SDL_SCANCODE_0
  toNumber ScancodeReturn = Raw.SDL_SCANCODE_RETURN
  toNumber ScancodeEscape = Raw.SDL_SCANCODE_ESCAPE
  toNumber ScancodeBackspace = Raw.SDL_SCANCODE_BACKSPACE
  toNumber ScancodeTab = Raw.SDL_SCANCODE_TAB
  toNumber ScancodeSpace = Raw.SDL_SCANCODE_SPACE
  toNumber ScancodeMinus = Raw.SDL_SCANCODE_MINUS
  toNumber ScancodeEquals = Raw.SDL_SCANCODE_EQUALS
  toNumber ScancodeLeftBracket = Raw.SDL_SCANCODE_LEFTBRACKET
  toNumber ScancodeRightBracket = Raw.SDL_SCANCODE_RIGHTBRACKET
  toNumber ScancodeBackslash = Raw.SDL_SCANCODE_BACKSLASH
  toNumber ScancodeNonUSHash = Raw.SDL_SCANCODE_NONUSHASH
  toNumber ScancodeSemicolon = Raw.SDL_SCANCODE_SEMICOLON
  toNumber ScancodeApostrophe = Raw.SDL_SCANCODE_APOSTROPHE
  toNumber ScancodeGrave = Raw.SDL_SCANCODE_GRAVE
  toNumber ScancodeComma = Raw.SDL_SCANCODE_COMMA
  toNumber ScancodePeriod = Raw.SDL_SCANCODE_PERIOD
  toNumber ScancodeSlash = Raw.SDL_SCANCODE_SLASH
  toNumber ScancodeCapsLock = Raw.SDL_SCANCODE_CAPSLOCK
  toNumber ScancodeF1 = Raw.SDL_SCANCODE_F1
  toNumber ScancodeF2 = Raw.SDL_SCANCODE_F2
  toNumber ScancodeF3 = Raw.SDL_SCANCODE_F3
  toNumber ScancodeF4 = Raw.SDL_SCANCODE_F4
  toNumber ScancodeF5 = Raw.SDL_SCANCODE_F5
  toNumber ScancodeF6 = Raw.SDL_SCANCODE_F6
  toNumber ScancodeF7 = Raw.SDL_SCANCODE_F7
  toNumber ScancodeF8 = Raw.SDL_SCANCODE_F8
  toNumber ScancodeF9 = Raw.SDL_SCANCODE_F9
  toNumber ScancodeF10 = Raw.SDL_SCANCODE_F10
  toNumber ScancodeF11 = Raw.SDL_SCANCODE_F11
  toNumber ScancodeF12 = Raw.SDL_SCANCODE_F12
  toNumber ScancodePrintScreen = Raw.SDL_SCANCODE_PRINTSCREEN
  toNumber ScancodeScrollLock = Raw.SDL_SCANCODE_SCROLLLOCK
  toNumber ScancodePause = Raw.SDL_SCANCODE_PAUSE
  toNumber ScancodeInsert = Raw.SDL_SCANCODE_INSERT
  toNumber ScancodeHome = Raw.SDL_SCANCODE_HOME
  toNumber ScancodePageUp = Raw.SDL_SCANCODE_PAGEUP
  toNumber ScancodeDelete = Raw.SDL_SCANCODE_DELETE
  toNumber ScancodeEnd = Raw.SDL_SCANCODE_END
  toNumber ScancodePageDown = Raw.SDL_SCANCODE_PAGEDOWN
  toNumber ScancodeRight = Raw.SDL_SCANCODE_RIGHT
  toNumber ScancodeLeft = Raw.SDL_SCANCODE_LEFT
  toNumber ScancodeDown = Raw.SDL_SCANCODE_DOWN
  toNumber ScancodeUp = Raw.SDL_SCANCODE_UP
  toNumber ScancodeNumLockClear = Raw.SDL_SCANCODE_NUMLOCKCLEAR
  toNumber ScancodeKPDivide = Raw.SDL_SCANCODE_KP_DIVIDE
  toNumber ScancodeKPMultiply = Raw.SDL_SCANCODE_KP_MULTIPLY
  toNumber ScancodeKPMinus = Raw.SDL_SCANCODE_KP_MINUS
  toNumber ScancodeKPPlus = Raw.SDL_SCANCODE_KP_PLUS
  toNumber ScancodeKPEnter = Raw.SDL_SCANCODE_KP_ENTER
  toNumber ScancodeKP1 = Raw.SDL_SCANCODE_KP_1
  toNumber ScancodeKP2 = Raw.SDL_SCANCODE_KP_2
  toNumber ScancodeKP3 = Raw.SDL_SCANCODE_KP_3
  toNumber ScancodeKP4 = Raw.SDL_SCANCODE_KP_4
  toNumber ScancodeKP5 = Raw.SDL_SCANCODE_KP_5
  toNumber ScancodeKP6 = Raw.SDL_SCANCODE_KP_6
  toNumber ScancodeKP7 = Raw.SDL_SCANCODE_KP_7
  toNumber ScancodeKP8 = Raw.SDL_SCANCODE_KP_8
  toNumber ScancodeKP9 = Raw.SDL_SCANCODE_KP_9
  toNumber ScancodeKP0 = Raw.SDL_SCANCODE_KP_0
  toNumber ScancodeKPPeriod = Raw.SDL_SCANCODE_KP_PERIOD
  toNumber ScancodeNonUSBackslash = Raw.SDL_SCANCODE_NONUSBACKSLASH
  toNumber ScancodeApplication = Raw.SDL_SCANCODE_APPLICATION
  toNumber ScancodePower = Raw.SDL_SCANCODE_POWER
  toNumber ScancodeKPEquals = Raw.SDL_SCANCODE_KP_EQUALS
  toNumber ScancodeF13 = Raw.SDL_SCANCODE_F13
  toNumber ScancodeF14 = Raw.SDL_SCANCODE_F14
  toNumber ScancodeF15 = Raw.SDL_SCANCODE_F15
  toNumber ScancodeF16 = Raw.SDL_SCANCODE_F16
  toNumber ScancodeF17 = Raw.SDL_SCANCODE_F17
  toNumber ScancodeF18 = Raw.SDL_SCANCODE_F18
  toNumber ScancodeF19 = Raw.SDL_SCANCODE_F19
  toNumber ScancodeF20 = Raw.SDL_SCANCODE_F20
  toNumber ScancodeF21 = Raw.SDL_SCANCODE_F21
  toNumber ScancodeF22 = Raw.SDL_SCANCODE_F22
  toNumber ScancodeF23 = Raw.SDL_SCANCODE_F23
  toNumber ScancodeF24 = Raw.SDL_SCANCODE_F24
  toNumber ScancodeExecute = Raw.SDL_SCANCODE_EXECUTE
  toNumber ScancodeHelp = Raw.SDL_SCANCODE_HELP
  toNumber ScancodeMenu = Raw.SDL_SCANCODE_MENU
  toNumber ScancodeSelect = Raw.SDL_SCANCODE_SELECT
  toNumber ScancodeStop = Raw.SDL_SCANCODE_STOP
  toNumber ScancodeAgain = Raw.SDL_SCANCODE_AGAIN
  toNumber ScancodeUndo = Raw.SDL_SCANCODE_UNDO
  toNumber ScancodeCut = Raw.SDL_SCANCODE_CUT
  toNumber ScancodeCopy = Raw.SDL_SCANCODE_COPY
  toNumber ScancodePaste = Raw.SDL_SCANCODE_PASTE
  toNumber ScancodeFind = Raw.SDL_SCANCODE_FIND
  toNumber ScancodeMute = Raw.SDL_SCANCODE_MUTE
  toNumber ScancodeVolumeUp = Raw.SDL_SCANCODE_VOLUMEUP
  toNumber ScancodeVolumeDown = Raw.SDL_SCANCODE_VOLUMEDOWN
  toNumber ScancodeKPComma = Raw.SDL_SCANCODE_KP_COMMA
  toNumber ScancodeKPEqualsAS400 = Raw.SDL_SCANCODE_KP_EQUALSAS400
  toNumber ScancodeInternational1 = Raw.SDL_SCANCODE_INTERNATIONAL1
  toNumber ScancodeInternational2 = Raw.SDL_SCANCODE_INTERNATIONAL2
  toNumber ScancodeInternational3 = Raw.SDL_SCANCODE_INTERNATIONAL3
  toNumber ScancodeInternational4 = Raw.SDL_SCANCODE_INTERNATIONAL4
  toNumber ScancodeInternational5 = Raw.SDL_SCANCODE_INTERNATIONAL5
  toNumber ScancodeInternational6 = Raw.SDL_SCANCODE_INTERNATIONAL6
  toNumber ScancodeInternational7 = Raw.SDL_SCANCODE_INTERNATIONAL7
  toNumber ScancodeInternational8 = Raw.SDL_SCANCODE_INTERNATIONAL8
  toNumber ScancodeInternational9 = Raw.SDL_SCANCODE_INTERNATIONAL9
  toNumber ScancodeLang1 = Raw.SDL_SCANCODE_LANG1
  toNumber ScancodeLang2 = Raw.SDL_SCANCODE_LANG2
  toNumber ScancodeLang3 = Raw.SDL_SCANCODE_LANG3
  toNumber ScancodeLang4 = Raw.SDL_SCANCODE_LANG4
  toNumber ScancodeLang5 = Raw.SDL_SCANCODE_LANG5
  toNumber ScancodeLang6 = Raw.SDL_SCANCODE_LANG6
  toNumber ScancodeLang7 = Raw.SDL_SCANCODE_LANG7
  toNumber ScancodeLang8 = Raw.SDL_SCANCODE_LANG8
  toNumber ScancodeLang9 = Raw.SDL_SCANCODE_LANG9
  toNumber ScancodeAltErase = Raw.SDL_SCANCODE_ALTERASE
  toNumber ScancodeSysReq = Raw.SDL_SCANCODE_SYSREQ
  toNumber ScancodeCancel = Raw.SDL_SCANCODE_CANCEL
  toNumber ScancodeClear = Raw.SDL_SCANCODE_CLEAR
  toNumber ScancodePrior = Raw.SDL_SCANCODE_PRIOR
  toNumber ScancodeReturn2 = Raw.SDL_SCANCODE_RETURN2
  toNumber ScancodeSeparator = Raw.SDL_SCANCODE_SEPARATOR
  toNumber ScancodeOut = Raw.SDL_SCANCODE_OUT
  toNumber ScancodeOper = Raw.SDL_SCANCODE_OPER
  toNumber ScancodeClearAgain = Raw.SDL_SCANCODE_CLEARAGAIN
  toNumber ScancodeCrSel = Raw.SDL_SCANCODE_CRSEL
  toNumber ScancodeExSel = Raw.SDL_SCANCODE_EXSEL
  toNumber ScancodeKP00 = Raw.SDL_SCANCODE_KP_00
  toNumber ScancodeKP000 = Raw.SDL_SCANCODE_KP_000
  toNumber ScancodeThousandsSeparator = Raw.SDL_SCANCODE_THOUSANDSSEPARATOR
  toNumber ScancodeDecimalSeparator = Raw.SDL_SCANCODE_DECIMALSEPARATOR
  toNumber ScancodeCurrencyUnit = Raw.SDL_SCANCODE_CURRENCYUNIT
  toNumber ScancodeCurrencySubunit = Raw.SDL_SCANCODE_CURRENCYSUBUNIT
  toNumber ScancodeLeftParen = Raw.SDL_SCANCODE_KP_LEFTPAREN
  toNumber ScancodeRightParen = Raw.SDL_SCANCODE_KP_RIGHTPAREN
  toNumber ScancodeLeftBrace = Raw.SDL_SCANCODE_KP_LEFTBRACE
  toNumber ScancodeRightBrace = Raw.SDL_SCANCODE_KP_RIGHTBRACE
  toNumber ScancodeKPTab = Raw.SDL_SCANCODE_KP_TAB
  toNumber ScancodeKPBackspace = Raw.SDL_SCANCODE_KP_BACKSPACE
  toNumber ScancodeKPA = Raw.SDL_SCANCODE_KP_A
  toNumber ScancodeKPB = Raw.SDL_SCANCODE_KP_B
  toNumber ScancodeKPC = Raw.SDL_SCANCODE_KP_C
  toNumber ScancodeKPD = Raw.SDL_SCANCODE_KP_D
  toNumber ScancodeKPE = Raw.SDL_SCANCODE_KP_E
  toNumber ScancodeKPF = Raw.SDL_SCANCODE_KP_F
  toNumber ScancodeKPXOR = Raw.SDL_SCANCODE_KP_XOR
  toNumber ScancodeKPPower = Raw.SDL_SCANCODE_KP_POWER
  toNumber ScancodeKPPercent = Raw.SDL_SCANCODE_KP_PERCENT
  toNumber ScancodeKPLess = Raw.SDL_SCANCODE_KP_LESS
  toNumber ScancodeKPGreater = Raw.SDL_SCANCODE_KP_GREATER
  toNumber ScancodeKPAmpersand = Raw.SDL_SCANCODE_KP_AMPERSAND
  toNumber ScancodeKPDblAmpersand = Raw.SDL_SCANCODE_KP_DBLAMPERSAND
  toNumber ScancodeKPVerticalBar = Raw.SDL_SCANCODE_KP_VERTICALBAR
  toNumber ScancodeKPDblVerticalBar = Raw.SDL_SCANCODE_KP_DBLVERTICALBAR
  toNumber ScancodeKPColon = Raw.SDL_SCANCODE_KP_COLON
  toNumber ScancodeKPHash = Raw.SDL_SCANCODE_KP_HASH
  toNumber ScancodeKPSpace = Raw.SDL_SCANCODE_KP_SPACE
  toNumber ScancodeKPAt = Raw.SDL_SCANCODE_KP_AT
  toNumber ScancodeKPExclam = Raw.SDL_SCANCODE_KP_EXCLAM
  toNumber ScancodeKPMemStore = Raw.SDL_SCANCODE_KP_MEMSTORE
  toNumber ScancodeKPMemRecall = Raw.SDL_SCANCODE_KP_MEMRECALL
  toNumber ScancodeKPMemClear = Raw.SDL_SCANCODE_KP_MEMCLEAR
  toNumber ScancodeKPMemAdd = Raw.SDL_SCANCODE_KP_MEMADD
  toNumber ScancodeKPMemSubtract = Raw.SDL_SCANCODE_KP_MEMSUBTRACT
  toNumber ScancodeKPMemMultiply = Raw.SDL_SCANCODE_KP_MEMMULTIPLY
  toNumber ScancodeKPMemDivide = Raw.SDL_SCANCODE_KP_MEMDIVIDE
  toNumber ScancodeKPPlusMinus = Raw.SDL_SCANCODE_KP_PLUSMINUS
  toNumber ScancodeKPClear = Raw.SDL_SCANCODE_KP_CLEAR
  toNumber ScancodeKPClearEntry = Raw.SDL_SCANCODE_KP_CLEARENTRY
  toNumber ScancodeKPBinary = Raw.SDL_SCANCODE_KP_BINARY
  toNumber ScancodeKPOctal = Raw.SDL_SCANCODE_KP_OCTAL
  toNumber ScancodeKPDecimal = Raw.SDL_SCANCODE_KP_DECIMAL
  toNumber ScancodeKPHexadecimal = Raw.SDL_SCANCODE_KP_HEXADECIMAL
  toNumber ScancodeLCtrl = Raw.SDL_SCANCODE_LCTRL
  toNumber ScancodeLShift = Raw.SDL_SCANCODE_LSHIFT
  toNumber ScancodeLAlt = Raw.SDL_SCANCODE_LALT
  toNumber ScancodeLGUI = Raw.SDL_SCANCODE_LGUI
  toNumber ScancodeRCtrl = Raw.SDL_SCANCODE_RCTRL
  toNumber ScancodeRShift = Raw.SDL_SCANCODE_RSHIFT
  toNumber ScancodeRAlt = Raw.SDL_SCANCODE_RALT
  toNumber ScancodeRGUI = Raw.SDL_SCANCODE_RGUI
  toNumber ScancodeMode = Raw.SDL_SCANCODE_MODE
  toNumber ScancodeAudioNext = Raw.SDL_SCANCODE_AUDIONEXT
  toNumber ScancodeAudioPrev = Raw.SDL_SCANCODE_AUDIOPREV
  toNumber ScancodeAudioStop = Raw.SDL_SCANCODE_AUDIOSTOP
  toNumber ScancodeAudioPlay = Raw.SDL_SCANCODE_AUDIOPLAY
  toNumber ScancodeAudioMute = Raw.SDL_SCANCODE_AUDIOMUTE
  toNumber ScancodeMediaSelect = Raw.SDL_SCANCODE_MEDIASELECT
  toNumber ScancodeWWW = Raw.SDL_SCANCODE_WWW
  toNumber ScancodeMail = Raw.SDL_SCANCODE_MAIL
  toNumber ScancodeCalculator = Raw.SDL_SCANCODE_CALCULATOR
  toNumber ScancodeComputer = Raw.SDL_SCANCODE_COMPUTER
  toNumber ScancodeACSearch = Raw.SDL_SCANCODE_AC_SEARCH
  toNumber ScancodeACHome = Raw.SDL_SCANCODE_AC_HOME
  toNumber ScancodeACBack = Raw.SDL_SCANCODE_AC_BACK
  toNumber ScancodeACForward = Raw.SDL_SCANCODE_AC_FORWARD
  toNumber ScancodeACStop = Raw.SDL_SCANCODE_AC_STOP
  toNumber ScancodeACRefresh = Raw.SDL_SCANCODE_AC_REFRESH
  toNumber ScancodeACBookmarks = Raw.SDL_SCANCODE_AC_BOOKMARKS
  toNumber ScancodeBrightnessDown = Raw.SDL_SCANCODE_BRIGHTNESSDOWN
  toNumber ScancodeBrightnessUp = Raw.SDL_SCANCODE_BRIGHTNESSUP
  toNumber ScancodeDisplaySwitch = Raw.SDL_SCANCODE_DISPLAYSWITCH
  toNumber ScancodeKBDIllumToggle = Raw.SDL_SCANCODE_KBDILLUMTOGGLE
  toNumber ScancodeKBDIllumDown = Raw.SDL_SCANCODE_KBDILLUMDOWN
  toNumber ScancodeKBDIllumUp = Raw.SDL_SCANCODE_KBDILLUMUP
  toNumber ScancodeEject = Raw.SDL_SCANCODE_EJECT
  toNumber ScancodeSleep = Raw.SDL_SCANCODE_SLEEP
  toNumber ScancodeApp1 = Raw.SDL_SCANCODE_APP1
  toNumber ScancodeApp2 = Raw.SDL_SCANCODE_APP2

data Keycode
  = KeycodeUnknown
  | KeycodeReturn
  | KeycodeEscape
  | KeycodeBackspace
  | KeycodeTab
  | KeycodeSpace
  | KeycodeExclaim
  | KeycodeQuoteDbl
  | KeycodeHash
  | KeycodePercent
  | KeycodeDollar
  | KeycodeAmpersand
  | KeycodeQuote
  | KeycodeLeftParen
  | KeycodeRightParen
  | KeycodeAsterisk
  | KeycodePlus
  | KeycodeComma
  | KeycodeMinus
  | KeycodePeriod
  | KeycodeSlash
  | Keycode0
  | Keycode1
  | Keycode2
  | Keycode3
  | Keycode4
  | Keycode5
  | Keycode6
  | Keycode7
  | Keycode8
  | Keycode9
  | KeycodeColon
  | KeycodeSemicolon
  | KeycodeLess
  | KeycodeEquals
  | KeycodeGreater
  | KeycodeQuestion
  | KeycodeAt
  | KeycodeLeftBracket
  | KeycodeBackslash
  | KeycodeRightBracket
  | KeycodeCaret
  | KeycodeUnderscore
  | KeycodeBackquote
  | KeycodeA
  | KeycodeB
  | KeycodeC
  | KeycodeD
  | KeycodeE
  | KeycodeF
  | KeycodeG
  | KeycodeH
  | KeycodeI
  | KeycodeJ
  | KeycodeK
  | KeycodeL
  | KeycodeM
  | KeycodeN
  | KeycodeO
  | KeycodeP
  | KeycodeQ
  | KeycodeR
  | KeycodeS
  | KeycodeT
  | KeycodeU
  | KeycodeV
  | KeycodeW
  | KeycodeX
  | KeycodeY
  | KeycodeZ
  | KeycodeCapsLock
  | KeycodeF1
  | KeycodeF2
  | KeycodeF3
  | KeycodeF4
  | KeycodeF5
  | KeycodeF6
  | KeycodeF7
  | KeycodeF8
  | KeycodeF9
  | KeycodeF10
  | KeycodeF11
  | KeycodeF12
  | KeycodePrintScreen
  | KeycodeScrollLock
  | KeycodePause
  | KeycodeInsert
  | KeycodeHome
  | KeycodePageUp
  | KeycodeDelete
  | KeycodeEnd
  | KeycodePageDown
  | KeycodeRight
  | KeycodeLeft
  | KeycodeDown
  | KeycodeUp
  | KeycodeNumLockClear
  | KeycodeKPDivide
  | KeycodeKPMultiply
  | KeycodeKPMinus
  | KeycodeKPPlus
  | KeycodeKPEnter
  | KeycodeKP1
  | KeycodeKP2
  | KeycodeKP3
  | KeycodeKP4
  | KeycodeKP5
  | KeycodeKP6
  | KeycodeKP7
  | KeycodeKP8
  | KeycodeKP9
  | KeycodeKP0
  | KeycodeKPPeriod
  | KeycodeApplication
  | KeycodePower
  | KeycodeKPEquals
  | KeycodeF13
  | KeycodeF14
  | KeycodeF15
  | KeycodeF16
  | KeycodeF17
  | KeycodeF18
  | KeycodeF19
  | KeycodeF20
  | KeycodeF21
  | KeycodeF22
  | KeycodeF23
  | KeycodeF24
  | KeycodeExecute
  | KeycodeHelp
  | KeycodeMenu
  | KeycodeSelect
  | KeycodeStop
  | KeycodeAgain
  | KeycodeUndo
  | KeycodeCut
  | KeycodeCopy
  | KeycodePaste
  | KeycodeFind
  | KeycodeMute
  | KeycodeVolumeUp
  | KeycodeVolumeDown
  | KeycodeKPComma
  | KeycodeKPEqualsAS400
  | KeycodeAltErase
  | KeycodeSysReq
  | KeycodeCancel
  | KeycodeClear
  | KeycodePrior
  | KeycodeReturn2
  | KeycodeSeparator
  | KeycodeOut
  | KeycodeOper
  | KeycodeClearAgain
  | KeycodeCrSel
  | KeycodeExSel
  | KeycodeKP00
  | KeycodeKP000
  | KeycodeThousandsSeparator
  | KeycodeDecimalSeparator
  | KeycodeCurrencyUnit
  | KeycodeCurrencySubunit
  | KeycodeKPLeftParen
  | KeycodeKPRightParen
  | KeycodeKPLeftBrace
  | KeycodeKPRightBrace
  | KeycodeKPTab
  | KeycodeKPBackspace
  | KeycodeKPA
  | KeycodeKPB
  | KeycodeKPC
  | KeycodeKPD
  | KeycodeKPE
  | KeycodeKPF
  | KeycodeKPXor
  | KeycodeKPPower
  | KeycodeKPPercent
  | KeycodeKPLess
  | KeycodeKPGreater
  | KeycodeKPAmpersand
  | KeycodeKPDblAmpersand
  | KeycodeKPVerticalBar
  | KeycodeKPDblVerticalBar
  | KeycodeKPColon
  | KeycodeKPHash
  | KeycodeKPSpace
  | KeycodeKPAt
  | KeycodeKPExclam
  | KeycodeKPMemStore
  | KeycodeKPMemRecall
  | KeycodeKPMemClear
  | KeycodeKPMemAdd
  | KeycodeKPMemSubtract
  | KeycodeKPMemMultiply
  | KeycodeKPMemDivide
  | KeycodeKPPlusMinus
  | KeycodeKPClear
  | KeycodeKPClearEntry
  | KeycodeKPBinary
  | KeycodeKPOctal
  | KeycodeKPDecimal
  | KeycodeKPHexadecimal
  | KeycodeLCtrl
  | KeycodeLShift
  | KeycodeLAlt
  | KeycodeLGUI
  | KeycodeRCtrl
  | KeycodeRShift
  | KeycodeRAlt
  | KeycodeRGUI
  | KeycodeMode
  | KeycodeAudioNext
  | KeycodeAudioPrev
  | KeycodeAudioStop
  | KeycodeAudioPlay
  | KeycodeAudioMute
  | KeycodeMediaSelect
  | KeycodeWWW
  | KeycodeMail
  | KeycodeCalculator
  | KeycodeComputer
  | KeycodeACSearch
  | KeycodeACHome
  | KeycodeACBack
  | KeycodeACForward
  | KeycodeACStop
  | KeycodeACRefresh
  | KeycodeACBookmarks
  | KeycodeBrightnessDown
  | KeycodeBrightnessUp
  | KeycodeDisplaySwitch
  | KeycodeKbdIllumToggle
  | KeycodeKbdIllumDown
  | KeycodeKbdIllumUp
  | KeycodeEject
  | KeycodeSleep
  deriving (Eq, Ord, Show, Typeable)

instance FromNumber Keycode Int32 where
  fromNumber n' = case n' of
    Raw.SDLK_UNKNOWN -> KeycodeUnknown
    Raw.SDLK_RETURN -> KeycodeReturn
    Raw.SDLK_ESCAPE -> KeycodeEscape
    Raw.SDLK_BACKSPACE -> KeycodeBackspace
    Raw.SDLK_TAB -> KeycodeTab
    Raw.SDLK_SPACE -> KeycodeSpace
    Raw.SDLK_EXCLAIM -> KeycodeExclaim
    Raw.SDLK_QUOTEDBL -> KeycodeQuoteDbl
    Raw.SDLK_HASH -> KeycodeHash
    Raw.SDLK_PERCENT -> KeycodePercent
    Raw.SDLK_DOLLAR -> KeycodeDollar
    Raw.SDLK_AMPERSAND -> KeycodeAmpersand
    Raw.SDLK_QUOTE -> KeycodeQuote
    Raw.SDLK_LEFTPAREN -> KeycodeLeftParen
    Raw.SDLK_RIGHTPAREN -> KeycodeRightParen
    Raw.SDLK_ASTERISK -> KeycodeAsterisk
    Raw.SDLK_PLUS -> KeycodePlus
    Raw.SDLK_COMMA -> KeycodeComma
    Raw.SDLK_MINUS -> KeycodeMinus
    Raw.SDLK_PERIOD -> KeycodePeriod
    Raw.SDLK_SLASH -> KeycodeSlash
    Raw.SDLK_0 -> Keycode0
    Raw.SDLK_1 -> Keycode1
    Raw.SDLK_2 -> Keycode2
    Raw.SDLK_3 -> Keycode3
    Raw.SDLK_4 -> Keycode4
    Raw.SDLK_5 -> Keycode5
    Raw.SDLK_6 -> Keycode6
    Raw.SDLK_7 -> Keycode7
    Raw.SDLK_8 -> Keycode8
    Raw.SDLK_9 -> Keycode9
    Raw.SDLK_COLON -> KeycodeColon
    Raw.SDLK_SEMICOLON -> KeycodeSemicolon
    Raw.SDLK_LESS -> KeycodeLess
    Raw.SDLK_EQUALS -> KeycodeEquals
    Raw.SDLK_GREATER -> KeycodeGreater
    Raw.SDLK_QUESTION -> KeycodeQuestion
    Raw.SDLK_AT -> KeycodeAt
    Raw.SDLK_LEFTBRACKET -> KeycodeLeftBracket
    Raw.SDLK_BACKSLASH -> KeycodeBackslash
    Raw.SDLK_RIGHTBRACKET -> KeycodeRightBracket
    Raw.SDLK_CARET -> KeycodeCaret
    Raw.SDLK_UNDERSCORE -> KeycodeUnderscore
    Raw.SDLK_BACKQUOTE -> KeycodeBackquote
    Raw.SDLK_a -> KeycodeA
    Raw.SDLK_b -> KeycodeB
    Raw.SDLK_c -> KeycodeC
    Raw.SDLK_d -> KeycodeD
    Raw.SDLK_e -> KeycodeE
    Raw.SDLK_f -> KeycodeF
    Raw.SDLK_g -> KeycodeG
    Raw.SDLK_h -> KeycodeH
    Raw.SDLK_i -> KeycodeI
    Raw.SDLK_j -> KeycodeJ
    Raw.SDLK_k -> KeycodeK
    Raw.SDLK_l -> KeycodeL
    Raw.SDLK_m -> KeycodeM
    Raw.SDLK_n -> KeycodeN
    Raw.SDLK_o -> KeycodeO
    Raw.SDLK_p -> KeycodeP
    Raw.SDLK_q -> KeycodeQ
    Raw.SDLK_r -> KeycodeR
    Raw.SDLK_s -> KeycodeS
    Raw.SDLK_t -> KeycodeT
    Raw.SDLK_u -> KeycodeU
    Raw.SDLK_v -> KeycodeV
    Raw.SDLK_w -> KeycodeW
    Raw.SDLK_x -> KeycodeX
    Raw.SDLK_y -> KeycodeY
    Raw.SDLK_z -> KeycodeZ
    Raw.SDLK_CAPSLOCK -> KeycodeCapsLock
    Raw.SDLK_F1 -> KeycodeF1
    Raw.SDLK_F2 -> KeycodeF2
    Raw.SDLK_F3 -> KeycodeF3
    Raw.SDLK_F4 -> KeycodeF4
    Raw.SDLK_F5 -> KeycodeF5
    Raw.SDLK_F6 -> KeycodeF6
    Raw.SDLK_F7 -> KeycodeF7
    Raw.SDLK_F8 -> KeycodeF8
    Raw.SDLK_F9 -> KeycodeF9
    Raw.SDLK_F10 -> KeycodeF10
    Raw.SDLK_F11 -> KeycodeF11
    Raw.SDLK_F12 -> KeycodeF12
    Raw.SDLK_PRINTSCREEN -> KeycodePrintScreen
    Raw.SDLK_SCROLLLOCK -> KeycodeScrollLock
    Raw.SDLK_PAUSE -> KeycodePause
    Raw.SDLK_INSERT -> KeycodeInsert
    Raw.SDLK_HOME -> KeycodeHome
    Raw.SDLK_PAGEUP -> KeycodePageUp
    Raw.SDLK_DELETE -> KeycodeDelete
    Raw.SDLK_END -> KeycodeEnd
    Raw.SDLK_PAGEDOWN -> KeycodePageDown
    Raw.SDLK_RIGHT -> KeycodeRight
    Raw.SDLK_LEFT -> KeycodeLeft
    Raw.SDLK_DOWN -> KeycodeDown
    Raw.SDLK_UP -> KeycodeUp
    Raw.SDLK_NUMLOCKCLEAR -> KeycodeNumLockClear
    Raw.SDLK_KP_DIVIDE -> KeycodeKPDivide
    Raw.SDLK_KP_MULTIPLY -> KeycodeKPMultiply
    Raw.SDLK_KP_MINUS -> KeycodeKPMinus
    Raw.SDLK_KP_PLUS -> KeycodeKPPlus
    Raw.SDLK_KP_ENTER -> KeycodeKPEnter
    Raw.SDLK_KP_1 -> KeycodeKP1
    Raw.SDLK_KP_2 -> KeycodeKP2
    Raw.SDLK_KP_3 -> KeycodeKP3
    Raw.SDLK_KP_4 -> KeycodeKP4
    Raw.SDLK_KP_5 -> KeycodeKP5
    Raw.SDLK_KP_6 -> KeycodeKP6
    Raw.SDLK_KP_7 -> KeycodeKP7
    Raw.SDLK_KP_8 -> KeycodeKP8
    Raw.SDLK_KP_9 -> KeycodeKP9
    Raw.SDLK_KP_0 -> KeycodeKP0
    Raw.SDLK_KP_PERIOD -> KeycodeKPPeriod
    Raw.SDLK_APPLICATION -> KeycodeApplication
    Raw.SDLK_POWER -> KeycodePower
    Raw.SDLK_KP_EQUALS -> KeycodeKPEquals
    Raw.SDLK_F13 -> KeycodeF13
    Raw.SDLK_F14 -> KeycodeF14
    Raw.SDLK_F15 -> KeycodeF15
    Raw.SDLK_F16 -> KeycodeF16
    Raw.SDLK_F17 -> KeycodeF17
    Raw.SDLK_F18 -> KeycodeF18
    Raw.SDLK_F19 -> KeycodeF19
    Raw.SDLK_F20 -> KeycodeF20
    Raw.SDLK_F21 -> KeycodeF21
    Raw.SDLK_F22 -> KeycodeF22
    Raw.SDLK_F23 -> KeycodeF23
    Raw.SDLK_F24 -> KeycodeF24
    Raw.SDLK_EXECUTE -> KeycodeExecute
    Raw.SDLK_HELP -> KeycodeHelp
    Raw.SDLK_MENU -> KeycodeMenu
    Raw.SDLK_SELECT -> KeycodeSelect
    Raw.SDLK_STOP -> KeycodeStop
    Raw.SDLK_AGAIN -> KeycodeAgain
    Raw.SDLK_UNDO -> KeycodeUndo
    Raw.SDLK_CUT -> KeycodeCut
    Raw.SDLK_COPY -> KeycodeCopy
    Raw.SDLK_PASTE -> KeycodePaste
    Raw.SDLK_FIND -> KeycodeFind
    Raw.SDLK_MUTE -> KeycodeMute
    Raw.SDLK_VOLUMEUP -> KeycodeVolumeUp
    Raw.SDLK_VOLUMEDOWN -> KeycodeVolumeDown
    Raw.SDLK_KP_COMMA -> KeycodeKPComma
    Raw.SDLK_KP_EQUALSAS400 -> KeycodeKPEqualsAS400
    Raw.SDLK_ALTERASE -> KeycodeAltErase
    Raw.SDLK_SYSREQ -> KeycodeSysReq
    Raw.SDLK_CANCEL -> KeycodeCancel
    Raw.SDLK_CLEAR -> KeycodeClear
    Raw.SDLK_PRIOR -> KeycodePrior
    Raw.SDLK_RETURN2 -> KeycodeReturn2
    Raw.SDLK_SEPARATOR -> KeycodeSeparator
    Raw.SDLK_OUT -> KeycodeOut
    Raw.SDLK_OPER -> KeycodeOper
    Raw.SDLK_CLEARAGAIN -> KeycodeClearAgain
    Raw.SDLK_CRSEL -> KeycodeCrSel
    Raw.SDLK_EXSEL -> KeycodeExSel
    Raw.SDLK_KP_00 -> KeycodeKP00
    Raw.SDLK_KP_000 -> KeycodeKP000
    Raw.SDLK_THOUSANDSSEPARATOR -> KeycodeThousandsSeparator
    Raw.SDLK_DECIMALSEPARATOR -> KeycodeDecimalSeparator
    Raw.SDLK_CURRENCYUNIT -> KeycodeCurrencyUnit
    Raw.SDLK_CURRENCYSUBUNIT -> KeycodeCurrencySubunit
    Raw.SDLK_KP_LEFTPAREN -> KeycodeKPLeftParen
    Raw.SDLK_KP_RIGHTPAREN -> KeycodeKPRightParen
    Raw.SDLK_KP_LEFTBRACE -> KeycodeKPLeftBrace
    Raw.SDLK_KP_RIGHTBRACE -> KeycodeKPRightBrace
    Raw.SDLK_KP_TAB -> KeycodeKPTab
    Raw.SDLK_KP_BACKSPACE -> KeycodeKPBackspace
    Raw.SDLK_KP_A -> KeycodeKPA
    Raw.SDLK_KP_B -> KeycodeKPB
    Raw.SDLK_KP_C -> KeycodeKPC
    Raw.SDLK_KP_D -> KeycodeKPD
    Raw.SDLK_KP_E -> KeycodeKPE
    Raw.SDLK_KP_F -> KeycodeKPF
    Raw.SDLK_KP_XOR -> KeycodeKPXor
    Raw.SDLK_KP_POWER -> KeycodeKPPower
    Raw.SDLK_KP_PERCENT -> KeycodeKPPercent
    Raw.SDLK_KP_LESS -> KeycodeKPLess
    Raw.SDLK_KP_GREATER -> KeycodeKPGreater
    Raw.SDLK_KP_AMPERSAND -> KeycodeKPAmpersand
    Raw.SDLK_KP_DBLAMPERSAND -> KeycodeKPDblAmpersand
    Raw.SDLK_KP_VERTICALBAR -> KeycodeKPVerticalBar
    Raw.SDLK_KP_DBLVERTICALBAR -> KeycodeKPDblVerticalBar
    Raw.SDLK_KP_COLON -> KeycodeKPColon
    Raw.SDLK_KP_HASH -> KeycodeKPHash
    Raw.SDLK_KP_SPACE -> KeycodeKPSpace
    Raw.SDLK_KP_AT -> KeycodeKPAt
    Raw.SDLK_KP_EXCLAM -> KeycodeKPExclam
    Raw.SDLK_KP_MEMSTORE -> KeycodeKPMemStore
    Raw.SDLK_KP_MEMRECALL -> KeycodeKPMemRecall
    Raw.SDLK_KP_MEMCLEAR -> KeycodeKPMemClear
    Raw.SDLK_KP_MEMADD -> KeycodeKPMemAdd
    Raw.SDLK_KP_MEMSUBTRACT -> KeycodeKPMemSubtract
    Raw.SDLK_KP_MEMMULTIPLY -> KeycodeKPMemMultiply
    Raw.SDLK_KP_MEMDIVIDE -> KeycodeKPMemDivide
    Raw.SDLK_KP_PLUSMINUS -> KeycodeKPPlusMinus
    Raw.SDLK_KP_CLEAR -> KeycodeKPClear
    Raw.SDLK_KP_CLEARENTRY -> KeycodeKPClearEntry
    Raw.SDLK_KP_BINARY -> KeycodeKPBinary
    Raw.SDLK_KP_OCTAL -> KeycodeKPOctal
    Raw.SDLK_KP_DECIMAL -> KeycodeKPDecimal
    Raw.SDLK_KP_HEXADECIMAL -> KeycodeKPHexadecimal
    Raw.SDLK_LCTRL -> KeycodeLCtrl
    Raw.SDLK_LSHIFT -> KeycodeLShift
    Raw.SDLK_LALT -> KeycodeLAlt
    Raw.SDLK_LGUI -> KeycodeLGUI
    Raw.SDLK_RCTRL -> KeycodeRCtrl
    Raw.SDLK_RSHIFT -> KeycodeRShift
    Raw.SDLK_RALT -> KeycodeRAlt
    Raw.SDLK_RGUI -> KeycodeRGUI
    Raw.SDLK_MODE -> KeycodeMode
    Raw.SDLK_AUDIONEXT -> KeycodeAudioNext
    Raw.SDLK_AUDIOPREV -> KeycodeAudioPrev
    Raw.SDLK_AUDIOSTOP -> KeycodeAudioStop
    Raw.SDLK_AUDIOPLAY -> KeycodeAudioPlay
    Raw.SDLK_AUDIOMUTE -> KeycodeAudioMute
    Raw.SDLK_MEDIASELECT -> KeycodeMediaSelect
    Raw.SDLK_WWW -> KeycodeWWW
    Raw.SDLK_MAIL -> KeycodeMail
    Raw.SDLK_CALCULATOR -> KeycodeCalculator
    Raw.SDLK_COMPUTER -> KeycodeComputer
    Raw.SDLK_AC_SEARCH -> KeycodeACSearch
    Raw.SDLK_AC_HOME -> KeycodeACHome
    Raw.SDLK_AC_BACK -> KeycodeACBack
    Raw.SDLK_AC_FORWARD -> KeycodeACForward
    Raw.SDLK_AC_STOP -> KeycodeACStop
    Raw.SDLK_AC_REFRESH -> KeycodeACRefresh
    Raw.SDLK_AC_BOOKMARKS -> KeycodeACBookmarks
    Raw.SDLK_BRIGHTNESSDOWN -> KeycodeBrightnessDown
    Raw.SDLK_BRIGHTNESSUP -> KeycodeBrightnessUp
    Raw.SDLK_DISPLAYSWITCH -> KeycodeDisplaySwitch
    Raw.SDLK_KBDILLUMTOGGLE -> KeycodeKbdIllumToggle
    Raw.SDLK_KBDILLUMDOWN -> KeycodeKbdIllumDown
    Raw.SDLK_KBDILLUMUP -> KeycodeKbdIllumUp
    Raw.SDLK_EJECT -> KeycodeEject
    Raw.SDLK_SLEEP -> KeycodeSleep
    _ -> error "fromNumber: not numbered"

instance ToNumber Keycode Int32 where
  toNumber KeycodeUnknown = Raw.SDLK_UNKNOWN
  toNumber KeycodeReturn = Raw.SDLK_RETURN
  toNumber KeycodeEscape = Raw.SDLK_ESCAPE
  toNumber KeycodeBackspace = Raw.SDLK_BACKSPACE
  toNumber KeycodeTab = Raw.SDLK_TAB
  toNumber KeycodeSpace = Raw.SDLK_SPACE
  toNumber KeycodeExclaim = Raw.SDLK_EXCLAIM
  toNumber KeycodeQuoteDbl = Raw.SDLK_QUOTEDBL
  toNumber KeycodeHash = Raw.SDLK_HASH
  toNumber KeycodePercent = Raw.SDLK_PERCENT
  toNumber KeycodeDollar = Raw.SDLK_DOLLAR
  toNumber KeycodeAmpersand = Raw.SDLK_AMPERSAND
  toNumber KeycodeQuote = Raw.SDLK_QUOTE
  toNumber KeycodeLeftParen = Raw.SDLK_LEFTPAREN
  toNumber KeycodeRightParen = Raw.SDLK_RIGHTPAREN
  toNumber KeycodeAsterisk = Raw.SDLK_ASTERISK
  toNumber KeycodePlus = Raw.SDLK_PLUS
  toNumber KeycodeComma = Raw.SDLK_COMMA
  toNumber KeycodeMinus = Raw.SDLK_MINUS
  toNumber KeycodePeriod = Raw.SDLK_PERIOD
  toNumber KeycodeSlash = Raw.SDLK_SLASH
  toNumber Keycode0 = Raw.SDLK_0
  toNumber Keycode1 = Raw.SDLK_1
  toNumber Keycode2 = Raw.SDLK_2
  toNumber Keycode3 = Raw.SDLK_3
  toNumber Keycode4 = Raw.SDLK_4
  toNumber Keycode5 = Raw.SDLK_5
  toNumber Keycode6 = Raw.SDLK_6
  toNumber Keycode7 = Raw.SDLK_7
  toNumber Keycode8 = Raw.SDLK_8
  toNumber Keycode9 = Raw.SDLK_9
  toNumber KeycodeColon = Raw.SDLK_COLON
  toNumber KeycodeSemicolon = Raw.SDLK_SEMICOLON
  toNumber KeycodeLess = Raw.SDLK_LESS
  toNumber KeycodeEquals = Raw.SDLK_EQUALS
  toNumber KeycodeGreater = Raw.SDLK_GREATER
  toNumber KeycodeQuestion = Raw.SDLK_QUESTION
  toNumber KeycodeAt = Raw.SDLK_AT
  toNumber KeycodeLeftBracket = Raw.SDLK_LEFTBRACKET
  toNumber KeycodeBackslash = Raw.SDLK_BACKSLASH
  toNumber KeycodeRightBracket = Raw.SDLK_RIGHTBRACKET
  toNumber KeycodeCaret = Raw.SDLK_CARET
  toNumber KeycodeUnderscore = Raw.SDLK_UNDERSCORE
  toNumber KeycodeBackquote = Raw.SDLK_BACKQUOTE
  toNumber KeycodeA = Raw.SDLK_a
  toNumber KeycodeB = Raw.SDLK_b
  toNumber KeycodeC = Raw.SDLK_c
  toNumber KeycodeD = Raw.SDLK_d
  toNumber KeycodeE = Raw.SDLK_e
  toNumber KeycodeF = Raw.SDLK_f
  toNumber KeycodeG = Raw.SDLK_g
  toNumber KeycodeH = Raw.SDLK_h
  toNumber KeycodeI = Raw.SDLK_i
  toNumber KeycodeJ = Raw.SDLK_j
  toNumber KeycodeK = Raw.SDLK_k
  toNumber KeycodeL = Raw.SDLK_l
  toNumber KeycodeM = Raw.SDLK_m
  toNumber KeycodeN = Raw.SDLK_n
  toNumber KeycodeO = Raw.SDLK_o
  toNumber KeycodeP = Raw.SDLK_p
  toNumber KeycodeQ = Raw.SDLK_q
  toNumber KeycodeR = Raw.SDLK_r
  toNumber KeycodeS = Raw.SDLK_s
  toNumber KeycodeT = Raw.SDLK_t
  toNumber KeycodeU = Raw.SDLK_u
  toNumber KeycodeV = Raw.SDLK_v
  toNumber KeycodeW = Raw.SDLK_w
  toNumber KeycodeX = Raw.SDLK_x
  toNumber KeycodeY = Raw.SDLK_y
  toNumber KeycodeZ = Raw.SDLK_z
  toNumber KeycodeCapsLock = Raw.SDLK_CAPSLOCK
  toNumber KeycodeF1 = Raw.SDLK_F1
  toNumber KeycodeF2 = Raw.SDLK_F2
  toNumber KeycodeF3 = Raw.SDLK_F3
  toNumber KeycodeF4 = Raw.SDLK_F4
  toNumber KeycodeF5 = Raw.SDLK_F5
  toNumber KeycodeF6 = Raw.SDLK_F6
  toNumber KeycodeF7 = Raw.SDLK_F7
  toNumber KeycodeF8 = Raw.SDLK_F8
  toNumber KeycodeF9 = Raw.SDLK_F9
  toNumber KeycodeF10 = Raw.SDLK_F10
  toNumber KeycodeF11 = Raw.SDLK_F11
  toNumber KeycodeF12 = Raw.SDLK_F12
  toNumber KeycodePrintScreen = Raw.SDLK_PRINTSCREEN
  toNumber KeycodeScrollLock = Raw.SDLK_SCROLLLOCK
  toNumber KeycodePause = Raw.SDLK_PAUSE
  toNumber KeycodeInsert = Raw.SDLK_INSERT
  toNumber KeycodeHome = Raw.SDLK_HOME
  toNumber KeycodePageUp = Raw.SDLK_PAGEUP
  toNumber KeycodeDelete = Raw.SDLK_DELETE
  toNumber KeycodeEnd = Raw.SDLK_END
  toNumber KeycodePageDown = Raw.SDLK_PAGEDOWN
  toNumber KeycodeRight = Raw.SDLK_RIGHT
  toNumber KeycodeLeft = Raw.SDLK_LEFT
  toNumber KeycodeDown = Raw.SDLK_DOWN
  toNumber KeycodeUp = Raw.SDLK_UP
  toNumber KeycodeNumLockClear = Raw.SDLK_NUMLOCKCLEAR
  toNumber KeycodeKPDivide = Raw.SDLK_KP_DIVIDE
  toNumber KeycodeKPMultiply = Raw.SDLK_KP_MULTIPLY
  toNumber KeycodeKPMinus = Raw.SDLK_KP_MINUS
  toNumber KeycodeKPPlus = Raw.SDLK_KP_PLUS
  toNumber KeycodeKPEnter = Raw.SDLK_KP_ENTER
  toNumber KeycodeKP1 = Raw.SDLK_KP_1
  toNumber KeycodeKP2 = Raw.SDLK_KP_2
  toNumber KeycodeKP3 = Raw.SDLK_KP_3
  toNumber KeycodeKP4 = Raw.SDLK_KP_4
  toNumber KeycodeKP5 = Raw.SDLK_KP_5
  toNumber KeycodeKP6 = Raw.SDLK_KP_6
  toNumber KeycodeKP7 = Raw.SDLK_KP_7
  toNumber KeycodeKP8 = Raw.SDLK_KP_8
  toNumber KeycodeKP9 = Raw.SDLK_KP_9
  toNumber KeycodeKP0 = Raw.SDLK_KP_0
  toNumber KeycodeKPPeriod = Raw.SDLK_KP_PERIOD
  toNumber KeycodeApplication = Raw.SDLK_APPLICATION
  toNumber KeycodePower = Raw.SDLK_POWER
  toNumber KeycodeKPEquals = Raw.SDLK_KP_EQUALS
  toNumber KeycodeF13 = Raw.SDLK_F13
  toNumber KeycodeF14 = Raw.SDLK_F14
  toNumber KeycodeF15 = Raw.SDLK_F15
  toNumber KeycodeF16 = Raw.SDLK_F16
  toNumber KeycodeF17 = Raw.SDLK_F17
  toNumber KeycodeF18 = Raw.SDLK_F18
  toNumber KeycodeF19 = Raw.SDLK_F19
  toNumber KeycodeF20 = Raw.SDLK_F20
  toNumber KeycodeF21 = Raw.SDLK_F21
  toNumber KeycodeF22 = Raw.SDLK_F22
  toNumber KeycodeF23 = Raw.SDLK_F23
  toNumber KeycodeF24 = Raw.SDLK_F24
  toNumber KeycodeExecute = Raw.SDLK_EXECUTE
  toNumber KeycodeHelp = Raw.SDLK_HELP
  toNumber KeycodeMenu = Raw.SDLK_MENU
  toNumber KeycodeSelect = Raw.SDLK_SELECT
  toNumber KeycodeStop = Raw.SDLK_STOP
  toNumber KeycodeAgain = Raw.SDLK_AGAIN
  toNumber KeycodeUndo = Raw.SDLK_UNDO
  toNumber KeycodeCut = Raw.SDLK_CUT
  toNumber KeycodeCopy = Raw.SDLK_COPY
  toNumber KeycodePaste = Raw.SDLK_PASTE
  toNumber KeycodeFind = Raw.SDLK_FIND
  toNumber KeycodeMute = Raw.SDLK_MUTE
  toNumber KeycodeVolumeUp = Raw.SDLK_VOLUMEUP
  toNumber KeycodeVolumeDown = Raw.SDLK_VOLUMEDOWN
  toNumber KeycodeKPComma = Raw.SDLK_KP_COMMA
  toNumber KeycodeKPEqualsAS400 = Raw.SDLK_KP_EQUALSAS400
  toNumber KeycodeAltErase = Raw.SDLK_ALTERASE
  toNumber KeycodeSysReq = Raw.SDLK_SYSREQ
  toNumber KeycodeCancel = Raw.SDLK_CANCEL
  toNumber KeycodeClear = Raw.SDLK_CLEAR
  toNumber KeycodePrior = Raw.SDLK_PRIOR
  toNumber KeycodeReturn2 = Raw.SDLK_RETURN2
  toNumber KeycodeSeparator = Raw.SDLK_SEPARATOR
  toNumber KeycodeOut = Raw.SDLK_OUT
  toNumber KeycodeOper = Raw.SDLK_OPER
  toNumber KeycodeClearAgain = Raw.SDLK_CLEARAGAIN
  toNumber KeycodeCrSel = Raw.SDLK_CRSEL
  toNumber KeycodeExSel = Raw.SDLK_EXSEL
  toNumber KeycodeKP00 = Raw.SDLK_KP_00
  toNumber KeycodeKP000 = Raw.SDLK_KP_000
  toNumber KeycodeThousandsSeparator = Raw.SDLK_THOUSANDSSEPARATOR
  toNumber KeycodeDecimalSeparator = Raw.SDLK_DECIMALSEPARATOR
  toNumber KeycodeCurrencyUnit = Raw.SDLK_CURRENCYUNIT
  toNumber KeycodeCurrencySubunit = Raw.SDLK_CURRENCYSUBUNIT
  toNumber KeycodeKPLeftParen = Raw.SDLK_KP_LEFTPAREN
  toNumber KeycodeKPRightParen = Raw.SDLK_KP_RIGHTPAREN
  toNumber KeycodeKPLeftBrace = Raw.SDLK_KP_LEFTBRACE
  toNumber KeycodeKPRightBrace = Raw.SDLK_KP_RIGHTBRACE
  toNumber KeycodeKPTab = Raw.SDLK_KP_TAB
  toNumber KeycodeKPBackspace = Raw.SDLK_KP_BACKSPACE
  toNumber KeycodeKPA = Raw.SDLK_KP_A
  toNumber KeycodeKPB = Raw.SDLK_KP_B
  toNumber KeycodeKPC = Raw.SDLK_KP_C
  toNumber KeycodeKPD = Raw.SDLK_KP_D
  toNumber KeycodeKPE = Raw.SDLK_KP_E
  toNumber KeycodeKPF = Raw.SDLK_KP_F
  toNumber KeycodeKPXor = Raw.SDLK_KP_XOR
  toNumber KeycodeKPPower = Raw.SDLK_KP_POWER
  toNumber KeycodeKPPercent = Raw.SDLK_KP_PERCENT
  toNumber KeycodeKPLess = Raw.SDLK_KP_LESS
  toNumber KeycodeKPGreater = Raw.SDLK_KP_GREATER
  toNumber KeycodeKPAmpersand = Raw.SDLK_KP_AMPERSAND
  toNumber KeycodeKPDblAmpersand = Raw.SDLK_KP_DBLAMPERSAND
  toNumber KeycodeKPVerticalBar = Raw.SDLK_KP_VERTICALBAR
  toNumber KeycodeKPDblVerticalBar = Raw.SDLK_KP_DBLVERTICALBAR
  toNumber KeycodeKPColon = Raw.SDLK_KP_COLON
  toNumber KeycodeKPHash = Raw.SDLK_KP_HASH
  toNumber KeycodeKPSpace = Raw.SDLK_KP_SPACE
  toNumber KeycodeKPAt = Raw.SDLK_KP_AT
  toNumber KeycodeKPExclam = Raw.SDLK_KP_EXCLAM
  toNumber KeycodeKPMemStore = Raw.SDLK_KP_MEMSTORE
  toNumber KeycodeKPMemRecall = Raw.SDLK_KP_MEMRECALL
  toNumber KeycodeKPMemClear = Raw.SDLK_KP_MEMCLEAR
  toNumber KeycodeKPMemAdd = Raw.SDLK_KP_MEMADD
  toNumber KeycodeKPMemSubtract = Raw.SDLK_KP_MEMSUBTRACT
  toNumber KeycodeKPMemMultiply = Raw.SDLK_KP_MEMMULTIPLY
  toNumber KeycodeKPMemDivide = Raw.SDLK_KP_MEMDIVIDE
  toNumber KeycodeKPPlusMinus = Raw.SDLK_KP_PLUSMINUS
  toNumber KeycodeKPClear = Raw.SDLK_KP_CLEAR
  toNumber KeycodeKPClearEntry = Raw.SDLK_KP_CLEARENTRY
  toNumber KeycodeKPBinary = Raw.SDLK_KP_BINARY
  toNumber KeycodeKPOctal = Raw.SDLK_KP_OCTAL
  toNumber KeycodeKPDecimal = Raw.SDLK_KP_DECIMAL
  toNumber KeycodeKPHexadecimal = Raw.SDLK_KP_HEXADECIMAL
  toNumber KeycodeLCtrl = Raw.SDLK_LCTRL
  toNumber KeycodeLShift = Raw.SDLK_LSHIFT
  toNumber KeycodeLAlt = Raw.SDLK_LALT
  toNumber KeycodeLGUI = Raw.SDLK_LGUI
  toNumber KeycodeRCtrl = Raw.SDLK_RCTRL
  toNumber KeycodeRShift = Raw.SDLK_RSHIFT
  toNumber KeycodeRAlt = Raw.SDLK_RALT
  toNumber KeycodeRGUI = Raw.SDLK_RGUI
  toNumber KeycodeMode = Raw.SDLK_MODE
  toNumber KeycodeAudioNext = Raw.SDLK_AUDIONEXT
  toNumber KeycodeAudioPrev = Raw.SDLK_AUDIOPREV
  toNumber KeycodeAudioStop = Raw.SDLK_AUDIOSTOP
  toNumber KeycodeAudioPlay = Raw.SDLK_AUDIOPLAY
  toNumber KeycodeAudioMute = Raw.SDLK_AUDIOMUTE
  toNumber KeycodeMediaSelect = Raw.SDLK_MEDIASELECT
  toNumber KeycodeWWW = Raw.SDLK_WWW
  toNumber KeycodeMail = Raw.SDLK_MAIL
  toNumber KeycodeCalculator = Raw.SDLK_CALCULATOR
  toNumber KeycodeComputer = Raw.SDLK_COMPUTER
  toNumber KeycodeACSearch = Raw.SDLK_AC_SEARCH
  toNumber KeycodeACHome = Raw.SDLK_AC_HOME
  toNumber KeycodeACBack = Raw.SDLK_AC_BACK
  toNumber KeycodeACForward = Raw.SDLK_AC_FORWARD
  toNumber KeycodeACStop = Raw.SDLK_AC_STOP
  toNumber KeycodeACRefresh = Raw.SDLK_AC_REFRESH
  toNumber KeycodeACBookmarks = Raw.SDLK_AC_BOOKMARKS
  toNumber KeycodeBrightnessDown = Raw.SDLK_BRIGHTNESSDOWN
  toNumber KeycodeBrightnessUp = Raw.SDLK_BRIGHTNESSUP
  toNumber KeycodeDisplaySwitch = Raw.SDLK_DISPLAYSWITCH
  toNumber KeycodeKbdIllumToggle = Raw.SDLK_KBDILLUMTOGGLE
  toNumber KeycodeKbdIllumDown = Raw.SDLK_KBDILLUMDOWN
  toNumber KeycodeKbdIllumUp = Raw.SDLK_KBDILLUMUP
  toNumber KeycodeEject = Raw.SDLK_EJECT
  toNumber KeycodeSleep = Raw.SDLK_SLEEP

data Keysym = Keysym
  { keysymScancode :: Scancode
  , keysymKeycode  :: Keycode
  , keysymModifier :: KeyModifier
  } deriving (Eq, Show, Typeable)

getKeyboardState :: IO (Scancode -> Bool)
getKeyboardState = do
  alloca $ \nkeys -> do
    keyptr <- Raw.getKeyboardState nkeys
    n <- peek nkeys
    keys <- V.fromList <$> peekArray (fromIntegral n) keyptr
    return $ \scancode -> 1 == keys V.! fromIntegral (toNumber scancode)
