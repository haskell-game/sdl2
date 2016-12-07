{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

{-|

An enumeration of scancodes and keycodes, allowing you to pattern match on keyboard keys.

This module uses a relatively new GHC feature called @PatternSynonyms@ in order to provide pattern
matches over the underlying 'Scancode' or 'Keycode'. We do this so that you are also able to
work with vendor-specific or otherwise unknown codes.

-}

module SDL.Input.Keyboard.Codes
       (-- * Scancodes
        -- | Scancodes are codes that correspond to the physical position of a key, independent of the current keyboard layout.
        -- Scancodes are meant to be layout-independent. Think of this as \"the user pressed the Q key as it would be on a US QWERTY keyboard\" regardless of whether this is actually a European keyboard or a Dvorak keyboard or whatever. The scancode is always the same key position.
        -- 'Scancode' makes sense when you want key presses based on their location on a keyboard - for example, the traditional \"WASD\" layout used in first-person shooters.
        Scancode(..),
        pattern ScancodeUnknown,
        pattern ScancodeA,
        pattern ScancodeB,
        pattern ScancodeC,
        pattern ScancodeD,
        pattern ScancodeE,
        pattern ScancodeF,
        pattern ScancodeG,
        pattern ScancodeH,
        pattern ScancodeI,
        pattern ScancodeJ,
        pattern ScancodeK,
        pattern ScancodeL,
        pattern ScancodeM,
        pattern ScancodeN,
        pattern ScancodeO,
        pattern ScancodeP,
        pattern ScancodeQ,
        pattern ScancodeR,
        pattern ScancodeS,
        pattern ScancodeT,
        pattern ScancodeU,
        pattern ScancodeV,
        pattern ScancodeW,
        pattern ScancodeX,
        pattern ScancodeY,
        pattern ScancodeZ,
        pattern Scancode1,
        pattern Scancode2,
        pattern Scancode3,
        pattern Scancode4,
        pattern Scancode5,
        pattern Scancode6,
        pattern Scancode7,
        pattern Scancode8,
        pattern Scancode9,
        pattern Scancode0,
        pattern ScancodeReturn,
        pattern ScancodeEscape,
        pattern ScancodeBackspace,
        pattern ScancodeTab,
        pattern ScancodeSpace,
        pattern ScancodeMinus,
        pattern ScancodeEquals,
        pattern ScancodeLeftBracket,
        pattern ScancodeRightBracket,
        pattern ScancodeBackslash,
        pattern ScancodeNonUSHash,
        pattern ScancodeSemicolon,
        pattern ScancodeApostrophe,
        pattern ScancodeGrave,
        pattern ScancodeComma,
        pattern ScancodePeriod,
        pattern ScancodeSlash,
        pattern ScancodeCapsLock,
        pattern ScancodeF1,
        pattern ScancodeF2,
        pattern ScancodeF3,
        pattern ScancodeF4,
        pattern ScancodeF5,
        pattern ScancodeF6,
        pattern ScancodeF7,
        pattern ScancodeF8,
        pattern ScancodeF9,
        pattern ScancodeF10,
        pattern ScancodeF11,
        pattern ScancodeF12,
        pattern ScancodePrintScreen,
        pattern ScancodeScrollLock,
        pattern ScancodePause,
        pattern ScancodeInsert,
        pattern ScancodeHome,
        pattern ScancodePageUp,
        pattern ScancodeDelete,
        pattern ScancodeEnd,
        pattern ScancodePageDown,
        pattern ScancodeRight,
        pattern ScancodeLeft,
        pattern ScancodeDown,
        pattern ScancodeUp,
        pattern ScancodeNumLockClear,
        pattern ScancodeKPDivide,
        pattern ScancodeKPMultiply,
        pattern ScancodeKPMinus,
        pattern ScancodeKPPlus,
        pattern ScancodeKPEnter,
        pattern ScancodeKP1,
        pattern ScancodeKP2,
        pattern ScancodeKP3,
        pattern ScancodeKP4,
        pattern ScancodeKP5,
        pattern ScancodeKP6,
        pattern ScancodeKP7,
        pattern ScancodeKP8,
        pattern ScancodeKP9,
        pattern ScancodeKP0,
        pattern ScancodeKPPeriod,
        pattern ScancodeNonUSBackslash,
        pattern ScancodeApplication,
        pattern ScancodePower,
        pattern ScancodeKPEquals,
        pattern ScancodeF13,
        pattern ScancodeF14,
        pattern ScancodeF15,
        pattern ScancodeF16,
        pattern ScancodeF17,
        pattern ScancodeF18,
        pattern ScancodeF19,
        pattern ScancodeF20,
        pattern ScancodeF21,
        pattern ScancodeF22,
        pattern ScancodeF23,
        pattern ScancodeF24,
        pattern ScancodeExecute,
        pattern ScancodeHelp,
        pattern ScancodeMenu,
        pattern ScancodeSelect,
        pattern ScancodeStop,
        pattern ScancodeAgain,
        pattern ScancodeUndo,
        pattern ScancodeCut,
        pattern ScancodeCopy,
        pattern ScancodePaste,
        pattern ScancodeFind,
        pattern ScancodeMute,
        pattern ScancodeVolumeUp,
        pattern ScancodeVolumeDown,
        pattern ScancodeKPComma,
        pattern ScancodeKPEqualsAS400,
        pattern ScancodeInternational1,
        pattern ScancodeInternational2,
        pattern ScancodeInternational3,
        pattern ScancodeInternational4,
        pattern ScancodeInternational5,
        pattern ScancodeInternational6,
        pattern ScancodeInternational7,
        pattern ScancodeInternational8,
        pattern ScancodeInternational9,
        pattern ScancodeLang1,
        pattern ScancodeLang2,
        pattern ScancodeLang3,
        pattern ScancodeLang4,
        pattern ScancodeLang5,
        pattern ScancodeLang6,
        pattern ScancodeLang7,
        pattern ScancodeLang8,
        pattern ScancodeLang9,
        pattern ScancodeAltErase,
        pattern ScancodeSysReq,
        pattern ScancodeCancel,
        pattern ScancodeClear,
        pattern ScancodePrior,
        pattern ScancodeReturn2,
        pattern ScancodeSeparator,
        pattern ScancodeOut,
        pattern ScancodeOper,
        pattern ScancodeClearAgain,
        pattern ScancodeCrSel,
        pattern ScancodeExSel,
        pattern ScancodeKP00,
        pattern ScancodeKP000,
        pattern ScancodeThousandsSeparator,
        pattern ScancodeDecimalSeparator,
        pattern ScancodeCurrencyUnit,
        pattern ScancodeCurrencySubunit,
        pattern ScancodeLeftParen,
        pattern ScancodeRightParen,
        pattern ScancodeLeftBrace,
        pattern ScancodeRightBrace,
        pattern ScancodeKPTab,
        pattern ScancodeKPBackspace,
        pattern ScancodeKPA,
        pattern ScancodeKPB,
        pattern ScancodeKPC,
        pattern ScancodeKPD,
        pattern ScancodeKPE,
        pattern ScancodeKPF,
        pattern ScancodeKPXOR,
        pattern ScancodeKPPower,
        pattern ScancodeKPPercent,
        pattern ScancodeKPLess,
        pattern ScancodeKPGreater,
        pattern ScancodeKPAmpersand,
        pattern ScancodeKPDblAmpersand,
        pattern ScancodeKPVerticalBar,
        pattern ScancodeKPDblVerticalBar,
        pattern ScancodeKPColon,
        pattern ScancodeKPHash,
        pattern ScancodeKPSpace,
        pattern ScancodeKPAt,
        pattern ScancodeKPExclam,
        pattern ScancodeKPMemStore,
        pattern ScancodeKPMemRecall,
        pattern ScancodeKPMemClear,
        pattern ScancodeKPMemAdd,
        pattern ScancodeKPMemSubtract,
        pattern ScancodeKPMemMultiply,
        pattern ScancodeKPMemDivide,
        pattern ScancodeKPPlusMinus,
        pattern ScancodeKPClear,
        pattern ScancodeKPClearEntry,
        pattern ScancodeKPBinary,
        pattern ScancodeKPOctal,
        pattern ScancodeKPDecimal,
        pattern ScancodeKPHexadecimal,
        pattern ScancodeLCtrl,
        pattern ScancodeLShift,
        pattern ScancodeLAlt,
        pattern ScancodeLGUI,
        pattern ScancodeRCtrl,
        pattern ScancodeRShift,
        pattern ScancodeRAlt,
        pattern ScancodeRGUI,
        pattern ScancodeMode,
        pattern ScancodeAudioNext,
        pattern ScancodeAudioPrev,
        pattern ScancodeAudioStop,
        pattern ScancodeAudioPlay,
        pattern ScancodeAudioMute,
        pattern ScancodeMediaSelect,
        pattern ScancodeWWW,
        pattern ScancodeMail,
        pattern ScancodeCalculator,
        pattern ScancodeComputer,
        pattern ScancodeACSearch,
        pattern ScancodeACHome,
        pattern ScancodeACBack,
        pattern ScancodeACForward,
        pattern ScancodeACStop,
        pattern ScancodeACRefresh,
        pattern ScancodeACBookmarks,
        pattern ScancodeBrightnessDown,
        pattern ScancodeBrightnessUp,
        pattern ScancodeDisplaySwitch,
        pattern ScancodeKBDIllumToggle,
        pattern ScancodeKBDIllumDown,
        pattern ScancodeKBDIllumUp,
        pattern ScancodeEject,
        pattern ScancodeSleep,
        pattern ScancodeApp1,
        pattern ScancodeApp2,

        -- * Keycode
        -- | Keycodes are layout-dependent codes, in that the code you receive can change for the same physical key depending on the users keyboard layout.
        -- Think of this as "the user pressed the key that is labelled 'Q' on a specific keyboard."
        -- 'Keycode' makes sense when you are anticipating the user to press either keys that match some mneumonic, or they are entering text.
        Keycode(..),
        pattern KeycodeUnknown,
        pattern KeycodeReturn,
        pattern KeycodeEscape,
        pattern KeycodeBackspace,
        pattern KeycodeTab,
        pattern KeycodeSpace,
        pattern KeycodeExclaim,
        pattern KeycodeQuoteDbl,
        pattern KeycodeHash,
        pattern KeycodePercent,
        pattern KeycodeDollar,
        pattern KeycodeAmpersand,
        pattern KeycodeQuote,
        pattern KeycodeLeftParen,
        pattern KeycodeRightParen,
        pattern KeycodeAsterisk,
        pattern KeycodePlus,
        pattern KeycodeComma,
        pattern KeycodeMinus,
        pattern KeycodePeriod,
        pattern KeycodeSlash,
        pattern Keycode0,
        pattern Keycode1,
        pattern Keycode2,
        pattern Keycode3,
        pattern Keycode4,
        pattern Keycode5,
        pattern Keycode6,
        pattern Keycode7,
        pattern Keycode8,
        pattern Keycode9,
        pattern KeycodeColon,
        pattern KeycodeSemicolon,
        pattern KeycodeLess,
        pattern KeycodeEquals,
        pattern KeycodeGreater,
        pattern KeycodeQuestion,
        pattern KeycodeAt,
        pattern KeycodeLeftBracket,
        pattern KeycodeBackslash,
        pattern KeycodeRightBracket,
        pattern KeycodeCaret,
        pattern KeycodeUnderscore,
        pattern KeycodeBackquote,
        pattern KeycodeA,
        pattern KeycodeB,
        pattern KeycodeC,
        pattern KeycodeD,
        pattern KeycodeE,
        pattern KeycodeF,
        pattern KeycodeG,
        pattern KeycodeH,
        pattern KeycodeI,
        pattern KeycodeJ,
        pattern KeycodeK,
        pattern KeycodeL,
        pattern KeycodeM,
        pattern KeycodeN,
        pattern KeycodeO,
        pattern KeycodeP,
        pattern KeycodeQ,
        pattern KeycodeR,
        pattern KeycodeS,
        pattern KeycodeT,
        pattern KeycodeU,
        pattern KeycodeV,
        pattern KeycodeW,
        pattern KeycodeX,
        pattern KeycodeY,
        pattern KeycodeZ,
        pattern KeycodeCapsLock,
        pattern KeycodeF1,
        pattern KeycodeF2,
        pattern KeycodeF3,
        pattern KeycodeF4,
        pattern KeycodeF5,
        pattern KeycodeF6,
        pattern KeycodeF7,
        pattern KeycodeF8,
        pattern KeycodeF9,
        pattern KeycodeF10,
        pattern KeycodeF11,
        pattern KeycodeF12,
        pattern KeycodePrintScreen,
        pattern KeycodeScrollLock,
        pattern KeycodePause,
        pattern KeycodeInsert,
        pattern KeycodeHome,
        pattern KeycodePageUp,
        pattern KeycodeDelete,
        pattern KeycodeEnd,
        pattern KeycodePageDown,
        pattern KeycodeRight,
        pattern KeycodeLeft,
        pattern KeycodeDown,
        pattern KeycodeUp,
        pattern KeycodeNumLockClear,
        pattern KeycodeKPDivide,
        pattern KeycodeKPMultiply,
        pattern KeycodeKPMinus,
        pattern KeycodeKPPlus,
        pattern KeycodeKPEnter,
        pattern KeycodeKP1,
        pattern KeycodeKP2,
        pattern KeycodeKP3,
        pattern KeycodeKP4,
        pattern KeycodeKP5,
        pattern KeycodeKP6,
        pattern KeycodeKP7,
        pattern KeycodeKP8,
        pattern KeycodeKP9,
        pattern KeycodeKP0,
        pattern KeycodeKPPeriod,
        pattern KeycodeApplication,
        pattern KeycodePower,
        pattern KeycodeKPEquals,
        pattern KeycodeF13,
        pattern KeycodeF14,
        pattern KeycodeF15,
        pattern KeycodeF16,
        pattern KeycodeF17,
        pattern KeycodeF18,
        pattern KeycodeF19,
        pattern KeycodeF20,
        pattern KeycodeF21,
        pattern KeycodeF22,
        pattern KeycodeF23,
        pattern KeycodeF24,
        pattern KeycodeExecute,
        pattern KeycodeHelp,
        pattern KeycodeMenu,
        pattern KeycodeSelect,
        pattern KeycodeStop,
        pattern KeycodeAgain,
        pattern KeycodeUndo,
        pattern KeycodeCut,
        pattern KeycodeCopy,
        pattern KeycodePaste,
        pattern KeycodeFind,
        pattern KeycodeMute,
        pattern KeycodeVolumeUp,
        pattern KeycodeVolumeDown,
        pattern KeycodeKPComma,
        pattern KeycodeKPEqualsAS400,
        pattern KeycodeAltErase,
        pattern KeycodeSysReq,
        pattern KeycodeCancel,
        pattern KeycodeClear,
        pattern KeycodePrior,
        pattern KeycodeReturn2,
        pattern KeycodeSeparator,
        pattern KeycodeOut,
        pattern KeycodeOper,
        pattern KeycodeClearAgain,
        pattern KeycodeCrSel,
        pattern KeycodeExSel,
        pattern KeycodeKP00,
        pattern KeycodeKP000,
        pattern KeycodeThousandsSeparator,
        pattern KeycodeDecimalSeparator,
        pattern KeycodeCurrencyUnit,
        pattern KeycodeCurrencySubunit,
        pattern KeycodeKPLeftParen,
        pattern KeycodeKPRightParen,
        pattern KeycodeKPLeftBrace,
        pattern KeycodeKPRightBrace,
        pattern KeycodeKPTab,
        pattern KeycodeKPBackspace,
        pattern KeycodeKPA,
        pattern KeycodeKPB,
        pattern KeycodeKPC,
        pattern KeycodeKPD,
        pattern KeycodeKPE,
        pattern KeycodeKPF,
        pattern KeycodeKPXor,
        pattern KeycodeKPPower,
        pattern KeycodeKPPercent,
        pattern KeycodeKPLess,
        pattern KeycodeKPGreater,
        pattern KeycodeKPAmpersand,
        pattern KeycodeKPDblAmpersand,
        pattern KeycodeKPVerticalBar,
        pattern KeycodeKPDblVerticalBar,
        pattern KeycodeKPColon,
        pattern KeycodeKPHash,
        pattern KeycodeKPSpace,
        pattern KeycodeKPAt,
        pattern KeycodeKPExclam,
        pattern KeycodeKPMemStore,
        pattern KeycodeKPMemRecall,
        pattern KeycodeKPMemClear,
        pattern KeycodeKPMemAdd,
        pattern KeycodeKPMemSubtract,
        pattern KeycodeKPMemMultiply,
        pattern KeycodeKPMemDivide,
        pattern KeycodeKPPlusMinus,
        pattern KeycodeKPClear,
        pattern KeycodeKPClearEntry,
        pattern KeycodeKPBinary,
        pattern KeycodeKPOctal,
        pattern KeycodeKPDecimal,
        pattern KeycodeKPHexadecimal,
        pattern KeycodeLCtrl,
        pattern KeycodeLShift,
        pattern KeycodeLAlt,
        pattern KeycodeLGUI,
        pattern KeycodeRCtrl,
        pattern KeycodeRShift,
        pattern KeycodeRAlt,
        pattern KeycodeRGUI,
        pattern KeycodeMode,
        pattern KeycodeAudioNext,
        pattern KeycodeAudioPrev,
        pattern KeycodeAudioStop,
        pattern KeycodeAudioPlay,
        pattern KeycodeAudioMute,
        pattern KeycodeMediaSelect,
        pattern KeycodeWWW,
        pattern KeycodeMail,
        pattern KeycodeCalculator,
        pattern KeycodeComputer,
        pattern KeycodeACSearch,
        pattern KeycodeACHome,
        pattern KeycodeACBack,
        pattern KeycodeACForward,
        pattern KeycodeACStop,
        pattern KeycodeACRefresh,
        pattern KeycodeACBookmarks,
        pattern KeycodeBrightnessDown,
        pattern KeycodeBrightnessUp,
        pattern KeycodeDisplaySwitch,
        pattern KeycodeKbdIllumToggle,
        pattern KeycodeKbdIllumDown,
        pattern KeycodeKbdIllumUp,
        pattern KeycodeEject,
        pattern KeycodeSleep)

       where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Int
import Data.Typeable
import Data.Word
import SDL.Internal.Numbered
import qualified SDL.Raw.Enum as Raw

newtype Scancode = Scancode { unwrapScancode :: Word32 }
  deriving (Bounded, Data, Eq, Ord, Read, Generic, Show, Typeable)

pattern ScancodeUnknown :: Scancode
pattern ScancodeUnknown = Scancode Raw.SDL_SCANCODE_UNKNOWN
pattern ScancodeA :: Scancode
pattern ScancodeA = Scancode Raw.SDL_SCANCODE_A
pattern ScancodeB :: Scancode
pattern ScancodeB = Scancode Raw.SDL_SCANCODE_B
pattern ScancodeC :: Scancode
pattern ScancodeC = Scancode Raw.SDL_SCANCODE_C
pattern ScancodeD :: Scancode
pattern ScancodeD = Scancode Raw.SDL_SCANCODE_D
pattern ScancodeE :: Scancode
pattern ScancodeE = Scancode Raw.SDL_SCANCODE_E
pattern ScancodeF :: Scancode
pattern ScancodeF = Scancode Raw.SDL_SCANCODE_F
pattern ScancodeG :: Scancode
pattern ScancodeG = Scancode Raw.SDL_SCANCODE_G
pattern ScancodeH :: Scancode
pattern ScancodeH = Scancode Raw.SDL_SCANCODE_H
pattern ScancodeI :: Scancode
pattern ScancodeI = Scancode Raw.SDL_SCANCODE_I
pattern ScancodeJ :: Scancode
pattern ScancodeJ = Scancode Raw.SDL_SCANCODE_J
pattern ScancodeK :: Scancode
pattern ScancodeK = Scancode Raw.SDL_SCANCODE_K
pattern ScancodeL :: Scancode
pattern ScancodeL = Scancode Raw.SDL_SCANCODE_L
pattern ScancodeM :: Scancode
pattern ScancodeM = Scancode Raw.SDL_SCANCODE_M
pattern ScancodeN :: Scancode
pattern ScancodeN = Scancode Raw.SDL_SCANCODE_N
pattern ScancodeO :: Scancode
pattern ScancodeO = Scancode Raw.SDL_SCANCODE_O
pattern ScancodeP :: Scancode
pattern ScancodeP = Scancode Raw.SDL_SCANCODE_P
pattern ScancodeQ :: Scancode
pattern ScancodeQ = Scancode Raw.SDL_SCANCODE_Q
pattern ScancodeR :: Scancode
pattern ScancodeR = Scancode Raw.SDL_SCANCODE_R
pattern ScancodeS :: Scancode
pattern ScancodeS = Scancode Raw.SDL_SCANCODE_S
pattern ScancodeT :: Scancode
pattern ScancodeT = Scancode Raw.SDL_SCANCODE_T
pattern ScancodeU :: Scancode
pattern ScancodeU = Scancode Raw.SDL_SCANCODE_U
pattern ScancodeV :: Scancode
pattern ScancodeV = Scancode Raw.SDL_SCANCODE_V
pattern ScancodeW :: Scancode
pattern ScancodeW = Scancode Raw.SDL_SCANCODE_W
pattern ScancodeX :: Scancode
pattern ScancodeX = Scancode Raw.SDL_SCANCODE_X
pattern ScancodeY :: Scancode
pattern ScancodeY = Scancode Raw.SDL_SCANCODE_Y
pattern ScancodeZ :: Scancode
pattern ScancodeZ = Scancode Raw.SDL_SCANCODE_Z
pattern Scancode1 :: Scancode
pattern Scancode1 = Scancode Raw.SDL_SCANCODE_1
pattern Scancode2 :: Scancode
pattern Scancode2 = Scancode Raw.SDL_SCANCODE_2
pattern Scancode3 :: Scancode
pattern Scancode3 = Scancode Raw.SDL_SCANCODE_3
pattern Scancode4 :: Scancode
pattern Scancode4 = Scancode Raw.SDL_SCANCODE_4
pattern Scancode5 :: Scancode
pattern Scancode5 = Scancode Raw.SDL_SCANCODE_5
pattern Scancode6 :: Scancode
pattern Scancode6 = Scancode Raw.SDL_SCANCODE_6
pattern Scancode7 :: Scancode
pattern Scancode7 = Scancode Raw.SDL_SCANCODE_7
pattern Scancode8 :: Scancode
pattern Scancode8 = Scancode Raw.SDL_SCANCODE_8
pattern Scancode9 :: Scancode
pattern Scancode9 = Scancode Raw.SDL_SCANCODE_9
pattern Scancode0 :: Scancode
pattern Scancode0 = Scancode Raw.SDL_SCANCODE_0
pattern ScancodeReturn :: Scancode
pattern ScancodeReturn = Scancode Raw.SDL_SCANCODE_RETURN
pattern ScancodeEscape :: Scancode
pattern ScancodeEscape = Scancode Raw.SDL_SCANCODE_ESCAPE
pattern ScancodeBackspace :: Scancode
pattern ScancodeBackspace = Scancode Raw.SDL_SCANCODE_BACKSPACE
pattern ScancodeTab :: Scancode
pattern ScancodeTab = Scancode Raw.SDL_SCANCODE_TAB
pattern ScancodeSpace :: Scancode
pattern ScancodeSpace = Scancode Raw.SDL_SCANCODE_SPACE
pattern ScancodeMinus :: Scancode
pattern ScancodeMinus = Scancode Raw.SDL_SCANCODE_MINUS
pattern ScancodeEquals :: Scancode
pattern ScancodeEquals = Scancode Raw.SDL_SCANCODE_EQUALS
pattern ScancodeLeftBracket :: Scancode
pattern ScancodeLeftBracket = Scancode Raw.SDL_SCANCODE_LEFTBRACKET
pattern ScancodeRightBracket :: Scancode
pattern ScancodeRightBracket = Scancode Raw.SDL_SCANCODE_RIGHTBRACKET
pattern ScancodeBackslash :: Scancode
pattern ScancodeBackslash = Scancode Raw.SDL_SCANCODE_BACKSLASH
pattern ScancodeNonUSHash :: Scancode
pattern ScancodeNonUSHash = Scancode Raw.SDL_SCANCODE_NONUSHASH
pattern ScancodeSemicolon :: Scancode
pattern ScancodeSemicolon = Scancode Raw.SDL_SCANCODE_SEMICOLON
pattern ScancodeApostrophe :: Scancode
pattern ScancodeApostrophe = Scancode Raw.SDL_SCANCODE_APOSTROPHE
pattern ScancodeGrave :: Scancode
pattern ScancodeGrave = Scancode Raw.SDL_SCANCODE_GRAVE
pattern ScancodeComma :: Scancode
pattern ScancodeComma = Scancode Raw.SDL_SCANCODE_COMMA
pattern ScancodePeriod :: Scancode
pattern ScancodePeriod = Scancode Raw.SDL_SCANCODE_PERIOD
pattern ScancodeSlash :: Scancode
pattern ScancodeSlash = Scancode Raw.SDL_SCANCODE_SLASH
pattern ScancodeCapsLock :: Scancode
pattern ScancodeCapsLock = Scancode Raw.SDL_SCANCODE_CAPSLOCK
pattern ScancodeF1 :: Scancode
pattern ScancodeF1 = Scancode Raw.SDL_SCANCODE_F1
pattern ScancodeF2 :: Scancode
pattern ScancodeF2 = Scancode Raw.SDL_SCANCODE_F2
pattern ScancodeF3 :: Scancode
pattern ScancodeF3 = Scancode Raw.SDL_SCANCODE_F3
pattern ScancodeF4 :: Scancode
pattern ScancodeF4 = Scancode Raw.SDL_SCANCODE_F4
pattern ScancodeF5 :: Scancode
pattern ScancodeF5 = Scancode Raw.SDL_SCANCODE_F5
pattern ScancodeF6 :: Scancode
pattern ScancodeF6 = Scancode Raw.SDL_SCANCODE_F6
pattern ScancodeF7 :: Scancode
pattern ScancodeF7 = Scancode Raw.SDL_SCANCODE_F7
pattern ScancodeF8 :: Scancode
pattern ScancodeF8 = Scancode Raw.SDL_SCANCODE_F8
pattern ScancodeF9 :: Scancode
pattern ScancodeF9 = Scancode Raw.SDL_SCANCODE_F9
pattern ScancodeF10 :: Scancode
pattern ScancodeF10 = Scancode Raw.SDL_SCANCODE_F10
pattern ScancodeF11 :: Scancode
pattern ScancodeF11 = Scancode Raw.SDL_SCANCODE_F11
pattern ScancodeF12 :: Scancode
pattern ScancodeF12 = Scancode Raw.SDL_SCANCODE_F12
pattern ScancodePrintScreen :: Scancode
pattern ScancodePrintScreen = Scancode Raw.SDL_SCANCODE_PRINTSCREEN
pattern ScancodeScrollLock :: Scancode
pattern ScancodeScrollLock = Scancode Raw.SDL_SCANCODE_SCROLLLOCK
pattern ScancodePause :: Scancode
pattern ScancodePause = Scancode Raw.SDL_SCANCODE_PAUSE
pattern ScancodeInsert :: Scancode
pattern ScancodeInsert = Scancode Raw.SDL_SCANCODE_INSERT
pattern ScancodeHome :: Scancode
pattern ScancodeHome = Scancode Raw.SDL_SCANCODE_HOME
pattern ScancodePageUp :: Scancode
pattern ScancodePageUp = Scancode Raw.SDL_SCANCODE_PAGEUP
pattern ScancodeDelete :: Scancode
pattern ScancodeDelete = Scancode Raw.SDL_SCANCODE_DELETE
pattern ScancodeEnd :: Scancode
pattern ScancodeEnd = Scancode Raw.SDL_SCANCODE_END
pattern ScancodePageDown :: Scancode
pattern ScancodePageDown = Scancode Raw.SDL_SCANCODE_PAGEDOWN
pattern ScancodeRight :: Scancode
pattern ScancodeRight = Scancode Raw.SDL_SCANCODE_RIGHT
pattern ScancodeLeft :: Scancode
pattern ScancodeLeft = Scancode Raw.SDL_SCANCODE_LEFT
pattern ScancodeDown :: Scancode
pattern ScancodeDown = Scancode Raw.SDL_SCANCODE_DOWN
pattern ScancodeUp :: Scancode
pattern ScancodeUp = Scancode Raw.SDL_SCANCODE_UP
pattern ScancodeNumLockClear :: Scancode
pattern ScancodeNumLockClear = Scancode Raw.SDL_SCANCODE_NUMLOCKCLEAR
pattern ScancodeKPDivide :: Scancode
pattern ScancodeKPDivide = Scancode Raw.SDL_SCANCODE_KP_DIVIDE
pattern ScancodeKPMultiply :: Scancode
pattern ScancodeKPMultiply = Scancode Raw.SDL_SCANCODE_KP_MULTIPLY
pattern ScancodeKPMinus :: Scancode
pattern ScancodeKPMinus = Scancode Raw.SDL_SCANCODE_KP_MINUS
pattern ScancodeKPPlus :: Scancode
pattern ScancodeKPPlus = Scancode Raw.SDL_SCANCODE_KP_PLUS
pattern ScancodeKPEnter :: Scancode
pattern ScancodeKPEnter = Scancode Raw.SDL_SCANCODE_KP_ENTER
pattern ScancodeKP1 :: Scancode
pattern ScancodeKP1 = Scancode Raw.SDL_SCANCODE_KP_1
pattern ScancodeKP2 :: Scancode
pattern ScancodeKP2 = Scancode Raw.SDL_SCANCODE_KP_2
pattern ScancodeKP3 :: Scancode
pattern ScancodeKP3 = Scancode Raw.SDL_SCANCODE_KP_3
pattern ScancodeKP4 :: Scancode
pattern ScancodeKP4 = Scancode Raw.SDL_SCANCODE_KP_4
pattern ScancodeKP5 :: Scancode
pattern ScancodeKP5 = Scancode Raw.SDL_SCANCODE_KP_5
pattern ScancodeKP6 :: Scancode
pattern ScancodeKP6 = Scancode Raw.SDL_SCANCODE_KP_6
pattern ScancodeKP7 :: Scancode
pattern ScancodeKP7 = Scancode Raw.SDL_SCANCODE_KP_7
pattern ScancodeKP8 :: Scancode
pattern ScancodeKP8 = Scancode Raw.SDL_SCANCODE_KP_8
pattern ScancodeKP9 :: Scancode
pattern ScancodeKP9 = Scancode Raw.SDL_SCANCODE_KP_9
pattern ScancodeKP0 :: Scancode
pattern ScancodeKP0 = Scancode Raw.SDL_SCANCODE_KP_0
pattern ScancodeKPPeriod :: Scancode
pattern ScancodeKPPeriod = Scancode Raw.SDL_SCANCODE_KP_PERIOD
pattern ScancodeNonUSBackslash :: Scancode
pattern ScancodeNonUSBackslash = Scancode Raw.SDL_SCANCODE_NONUSBACKSLASH
pattern ScancodeApplication :: Scancode
pattern ScancodeApplication = Scancode Raw.SDL_SCANCODE_APPLICATION
pattern ScancodePower :: Scancode
pattern ScancodePower = Scancode Raw.SDL_SCANCODE_POWER
pattern ScancodeKPEquals :: Scancode
pattern ScancodeKPEquals = Scancode Raw.SDL_SCANCODE_KP_EQUALS
pattern ScancodeF13 :: Scancode
pattern ScancodeF13 = Scancode Raw.SDL_SCANCODE_F13
pattern ScancodeF14 :: Scancode
pattern ScancodeF14 = Scancode Raw.SDL_SCANCODE_F14
pattern ScancodeF15 :: Scancode
pattern ScancodeF15 = Scancode Raw.SDL_SCANCODE_F15
pattern ScancodeF16 :: Scancode
pattern ScancodeF16 = Scancode Raw.SDL_SCANCODE_F16
pattern ScancodeF17 :: Scancode
pattern ScancodeF17 = Scancode Raw.SDL_SCANCODE_F17
pattern ScancodeF18 :: Scancode
pattern ScancodeF18 = Scancode Raw.SDL_SCANCODE_F18
pattern ScancodeF19 :: Scancode
pattern ScancodeF19 = Scancode Raw.SDL_SCANCODE_F19
pattern ScancodeF20 :: Scancode
pattern ScancodeF20 = Scancode Raw.SDL_SCANCODE_F20
pattern ScancodeF21 :: Scancode
pattern ScancodeF21 = Scancode Raw.SDL_SCANCODE_F21
pattern ScancodeF22 :: Scancode
pattern ScancodeF22 = Scancode Raw.SDL_SCANCODE_F22
pattern ScancodeF23 :: Scancode
pattern ScancodeF23 = Scancode Raw.SDL_SCANCODE_F23
pattern ScancodeF24 :: Scancode
pattern ScancodeF24 = Scancode Raw.SDL_SCANCODE_F24
pattern ScancodeExecute :: Scancode
pattern ScancodeExecute = Scancode Raw.SDL_SCANCODE_EXECUTE
pattern ScancodeHelp :: Scancode
pattern ScancodeHelp = Scancode Raw.SDL_SCANCODE_HELP
pattern ScancodeMenu :: Scancode
pattern ScancodeMenu = Scancode Raw.SDL_SCANCODE_MENU
pattern ScancodeSelect :: Scancode
pattern ScancodeSelect = Scancode Raw.SDL_SCANCODE_SELECT
pattern ScancodeStop :: Scancode
pattern ScancodeStop = Scancode Raw.SDL_SCANCODE_STOP
pattern ScancodeAgain :: Scancode
pattern ScancodeAgain = Scancode Raw.SDL_SCANCODE_AGAIN
pattern ScancodeUndo :: Scancode
pattern ScancodeUndo = Scancode Raw.SDL_SCANCODE_UNDO
pattern ScancodeCut :: Scancode
pattern ScancodeCut = Scancode Raw.SDL_SCANCODE_CUT
pattern ScancodeCopy :: Scancode
pattern ScancodeCopy = Scancode Raw.SDL_SCANCODE_COPY
pattern ScancodePaste :: Scancode
pattern ScancodePaste = Scancode Raw.SDL_SCANCODE_PASTE
pattern ScancodeFind :: Scancode
pattern ScancodeFind = Scancode Raw.SDL_SCANCODE_FIND
pattern ScancodeMute :: Scancode
pattern ScancodeMute = Scancode Raw.SDL_SCANCODE_MUTE
pattern ScancodeVolumeUp :: Scancode
pattern ScancodeVolumeUp = Scancode Raw.SDL_SCANCODE_VOLUMEUP
pattern ScancodeVolumeDown :: Scancode
pattern ScancodeVolumeDown = Scancode Raw.SDL_SCANCODE_VOLUMEDOWN
pattern ScancodeKPComma :: Scancode
pattern ScancodeKPComma = Scancode Raw.SDL_SCANCODE_KP_COMMA
pattern ScancodeKPEqualsAS400 :: Scancode
pattern ScancodeKPEqualsAS400 = Scancode Raw.SDL_SCANCODE_KP_EQUALSAS400
pattern ScancodeInternational1 :: Scancode
pattern ScancodeInternational1 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL1
pattern ScancodeInternational2 :: Scancode
pattern ScancodeInternational2 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL2
pattern ScancodeInternational3 :: Scancode
pattern ScancodeInternational3 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL3
pattern ScancodeInternational4 :: Scancode
pattern ScancodeInternational4 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL4
pattern ScancodeInternational5 :: Scancode
pattern ScancodeInternational5 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL5
pattern ScancodeInternational6 :: Scancode
pattern ScancodeInternational6 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL6
pattern ScancodeInternational7 :: Scancode
pattern ScancodeInternational7 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL7
pattern ScancodeInternational8 :: Scancode
pattern ScancodeInternational8 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL8
pattern ScancodeInternational9 :: Scancode
pattern ScancodeInternational9 = Scancode Raw.SDL_SCANCODE_INTERNATIONAL9
pattern ScancodeLang1 :: Scancode
pattern ScancodeLang1 = Scancode Raw.SDL_SCANCODE_LANG1
pattern ScancodeLang2 :: Scancode
pattern ScancodeLang2 = Scancode Raw.SDL_SCANCODE_LANG2
pattern ScancodeLang3 :: Scancode
pattern ScancodeLang3 = Scancode Raw.SDL_SCANCODE_LANG3
pattern ScancodeLang4 :: Scancode
pattern ScancodeLang4 = Scancode Raw.SDL_SCANCODE_LANG4
pattern ScancodeLang5 :: Scancode
pattern ScancodeLang5 = Scancode Raw.SDL_SCANCODE_LANG5
pattern ScancodeLang6 :: Scancode
pattern ScancodeLang6 = Scancode Raw.SDL_SCANCODE_LANG6
pattern ScancodeLang7 :: Scancode
pattern ScancodeLang7 = Scancode Raw.SDL_SCANCODE_LANG7
pattern ScancodeLang8 :: Scancode
pattern ScancodeLang8 = Scancode Raw.SDL_SCANCODE_LANG8
pattern ScancodeLang9 :: Scancode
pattern ScancodeLang9 = Scancode Raw.SDL_SCANCODE_LANG9
pattern ScancodeAltErase :: Scancode
pattern ScancodeAltErase = Scancode Raw.SDL_SCANCODE_ALTERASE
pattern ScancodeSysReq :: Scancode
pattern ScancodeSysReq = Scancode Raw.SDL_SCANCODE_SYSREQ
pattern ScancodeCancel :: Scancode
pattern ScancodeCancel = Scancode Raw.SDL_SCANCODE_CANCEL
pattern ScancodeClear :: Scancode
pattern ScancodeClear = Scancode Raw.SDL_SCANCODE_CLEAR
pattern ScancodePrior :: Scancode
pattern ScancodePrior = Scancode Raw.SDL_SCANCODE_PRIOR
pattern ScancodeReturn2 :: Scancode
pattern ScancodeReturn2 = Scancode Raw.SDL_SCANCODE_RETURN2
pattern ScancodeSeparator :: Scancode
pattern ScancodeSeparator = Scancode Raw.SDL_SCANCODE_SEPARATOR
pattern ScancodeOut :: Scancode
pattern ScancodeOut = Scancode Raw.SDL_SCANCODE_OUT
pattern ScancodeOper :: Scancode
pattern ScancodeOper = Scancode Raw.SDL_SCANCODE_OPER
pattern ScancodeClearAgain :: Scancode
pattern ScancodeClearAgain = Scancode Raw.SDL_SCANCODE_CLEARAGAIN
pattern ScancodeCrSel :: Scancode
pattern ScancodeCrSel = Scancode Raw.SDL_SCANCODE_CRSEL
pattern ScancodeExSel :: Scancode
pattern ScancodeExSel = Scancode Raw.SDL_SCANCODE_EXSEL
pattern ScancodeKP00 :: Scancode
pattern ScancodeKP00 = Scancode Raw.SDL_SCANCODE_KP_00
pattern ScancodeKP000 :: Scancode
pattern ScancodeKP000 = Scancode Raw.SDL_SCANCODE_KP_000
pattern ScancodeThousandsSeparator :: Scancode
pattern ScancodeThousandsSeparator = Scancode Raw.SDL_SCANCODE_THOUSANDSSEPARATOR
pattern ScancodeDecimalSeparator :: Scancode
pattern ScancodeDecimalSeparator = Scancode Raw.SDL_SCANCODE_DECIMALSEPARATOR
pattern ScancodeCurrencyUnit :: Scancode
pattern ScancodeCurrencyUnit = Scancode Raw.SDL_SCANCODE_CURRENCYUNIT
pattern ScancodeCurrencySubunit :: Scancode
pattern ScancodeCurrencySubunit = Scancode Raw.SDL_SCANCODE_CURRENCYSUBUNIT
pattern ScancodeLeftParen :: Scancode
pattern ScancodeLeftParen = Scancode Raw.SDL_SCANCODE_KP_LEFTPAREN
pattern ScancodeRightParen :: Scancode
pattern ScancodeRightParen = Scancode Raw.SDL_SCANCODE_KP_RIGHTPAREN
pattern ScancodeLeftBrace :: Scancode
pattern ScancodeLeftBrace = Scancode Raw.SDL_SCANCODE_KP_LEFTBRACE
pattern ScancodeRightBrace :: Scancode
pattern ScancodeRightBrace = Scancode Raw.SDL_SCANCODE_KP_RIGHTBRACE
pattern ScancodeKPTab :: Scancode
pattern ScancodeKPTab = Scancode Raw.SDL_SCANCODE_KP_TAB
pattern ScancodeKPBackspace :: Scancode
pattern ScancodeKPBackspace = Scancode Raw.SDL_SCANCODE_KP_BACKSPACE
pattern ScancodeKPA :: Scancode
pattern ScancodeKPA = Scancode Raw.SDL_SCANCODE_KP_A
pattern ScancodeKPB :: Scancode
pattern ScancodeKPB = Scancode Raw.SDL_SCANCODE_KP_B
pattern ScancodeKPC :: Scancode
pattern ScancodeKPC = Scancode Raw.SDL_SCANCODE_KP_C
pattern ScancodeKPD :: Scancode
pattern ScancodeKPD = Scancode Raw.SDL_SCANCODE_KP_D
pattern ScancodeKPE :: Scancode
pattern ScancodeKPE = Scancode Raw.SDL_SCANCODE_KP_E
pattern ScancodeKPF :: Scancode
pattern ScancodeKPF = Scancode Raw.SDL_SCANCODE_KP_F
pattern ScancodeKPXOR :: Scancode
pattern ScancodeKPXOR = Scancode Raw.SDL_SCANCODE_KP_XOR
pattern ScancodeKPPower :: Scancode
pattern ScancodeKPPower = Scancode Raw.SDL_SCANCODE_KP_POWER
pattern ScancodeKPPercent :: Scancode
pattern ScancodeKPPercent = Scancode Raw.SDL_SCANCODE_KP_PERCENT
pattern ScancodeKPLess :: Scancode
pattern ScancodeKPLess = Scancode Raw.SDL_SCANCODE_KP_LESS
pattern ScancodeKPGreater :: Scancode
pattern ScancodeKPGreater = Scancode Raw.SDL_SCANCODE_KP_GREATER
pattern ScancodeKPAmpersand :: Scancode
pattern ScancodeKPAmpersand = Scancode Raw.SDL_SCANCODE_KP_AMPERSAND
pattern ScancodeKPDblAmpersand :: Scancode
pattern ScancodeKPDblAmpersand = Scancode Raw.SDL_SCANCODE_KP_DBLAMPERSAND
pattern ScancodeKPVerticalBar :: Scancode
pattern ScancodeKPVerticalBar = Scancode Raw.SDL_SCANCODE_KP_VERTICALBAR
pattern ScancodeKPDblVerticalBar :: Scancode
pattern ScancodeKPDblVerticalBar = Scancode Raw.SDL_SCANCODE_KP_DBLVERTICALBAR
pattern ScancodeKPColon :: Scancode
pattern ScancodeKPColon = Scancode Raw.SDL_SCANCODE_KP_COLON
pattern ScancodeKPHash :: Scancode
pattern ScancodeKPHash = Scancode Raw.SDL_SCANCODE_KP_HASH
pattern ScancodeKPSpace :: Scancode
pattern ScancodeKPSpace = Scancode Raw.SDL_SCANCODE_KP_SPACE
pattern ScancodeKPAt :: Scancode
pattern ScancodeKPAt = Scancode Raw.SDL_SCANCODE_KP_AT
pattern ScancodeKPExclam :: Scancode
pattern ScancodeKPExclam = Scancode Raw.SDL_SCANCODE_KP_EXCLAM
pattern ScancodeKPMemStore :: Scancode
pattern ScancodeKPMemStore = Scancode Raw.SDL_SCANCODE_KP_MEMSTORE
pattern ScancodeKPMemRecall :: Scancode
pattern ScancodeKPMemRecall = Scancode Raw.SDL_SCANCODE_KP_MEMRECALL
pattern ScancodeKPMemClear :: Scancode
pattern ScancodeKPMemClear = Scancode Raw.SDL_SCANCODE_KP_MEMCLEAR
pattern ScancodeKPMemAdd :: Scancode
pattern ScancodeKPMemAdd = Scancode Raw.SDL_SCANCODE_KP_MEMADD
pattern ScancodeKPMemSubtract :: Scancode
pattern ScancodeKPMemSubtract = Scancode Raw.SDL_SCANCODE_KP_MEMSUBTRACT
pattern ScancodeKPMemMultiply :: Scancode
pattern ScancodeKPMemMultiply = Scancode Raw.SDL_SCANCODE_KP_MEMMULTIPLY
pattern ScancodeKPMemDivide :: Scancode
pattern ScancodeKPMemDivide = Scancode Raw.SDL_SCANCODE_KP_MEMDIVIDE
pattern ScancodeKPPlusMinus :: Scancode
pattern ScancodeKPPlusMinus = Scancode Raw.SDL_SCANCODE_KP_PLUSMINUS
pattern ScancodeKPClear :: Scancode
pattern ScancodeKPClear = Scancode Raw.SDL_SCANCODE_KP_CLEAR
pattern ScancodeKPClearEntry :: Scancode
pattern ScancodeKPClearEntry = Scancode Raw.SDL_SCANCODE_KP_CLEARENTRY
pattern ScancodeKPBinary :: Scancode
pattern ScancodeKPBinary = Scancode Raw.SDL_SCANCODE_KP_BINARY
pattern ScancodeKPOctal :: Scancode
pattern ScancodeKPOctal = Scancode Raw.SDL_SCANCODE_KP_OCTAL
pattern ScancodeKPDecimal :: Scancode
pattern ScancodeKPDecimal = Scancode Raw.SDL_SCANCODE_KP_DECIMAL
pattern ScancodeKPHexadecimal :: Scancode
pattern ScancodeKPHexadecimal = Scancode Raw.SDL_SCANCODE_KP_HEXADECIMAL
pattern ScancodeLCtrl :: Scancode
pattern ScancodeLCtrl = Scancode Raw.SDL_SCANCODE_LCTRL
pattern ScancodeLShift :: Scancode
pattern ScancodeLShift = Scancode Raw.SDL_SCANCODE_LSHIFT
pattern ScancodeLAlt :: Scancode
pattern ScancodeLAlt = Scancode Raw.SDL_SCANCODE_LALT
pattern ScancodeLGUI :: Scancode
pattern ScancodeLGUI = Scancode Raw.SDL_SCANCODE_LGUI
pattern ScancodeRCtrl :: Scancode
pattern ScancodeRCtrl = Scancode Raw.SDL_SCANCODE_RCTRL
pattern ScancodeRShift :: Scancode
pattern ScancodeRShift = Scancode Raw.SDL_SCANCODE_RSHIFT
pattern ScancodeRAlt :: Scancode
pattern ScancodeRAlt = Scancode Raw.SDL_SCANCODE_RALT
pattern ScancodeRGUI :: Scancode
pattern ScancodeRGUI = Scancode Raw.SDL_SCANCODE_RGUI
pattern ScancodeMode :: Scancode
pattern ScancodeMode = Scancode Raw.SDL_SCANCODE_MODE
pattern ScancodeAudioNext :: Scancode
pattern ScancodeAudioNext = Scancode Raw.SDL_SCANCODE_AUDIONEXT
pattern ScancodeAudioPrev :: Scancode
pattern ScancodeAudioPrev = Scancode Raw.SDL_SCANCODE_AUDIOPREV
pattern ScancodeAudioStop :: Scancode
pattern ScancodeAudioStop = Scancode Raw.SDL_SCANCODE_AUDIOSTOP
pattern ScancodeAudioPlay :: Scancode
pattern ScancodeAudioPlay = Scancode Raw.SDL_SCANCODE_AUDIOPLAY
pattern ScancodeAudioMute :: Scancode
pattern ScancodeAudioMute = Scancode Raw.SDL_SCANCODE_AUDIOMUTE
pattern ScancodeMediaSelect :: Scancode
pattern ScancodeMediaSelect = Scancode Raw.SDL_SCANCODE_MEDIASELECT
pattern ScancodeWWW :: Scancode
pattern ScancodeWWW = Scancode Raw.SDL_SCANCODE_WWW
pattern ScancodeMail :: Scancode
pattern ScancodeMail = Scancode Raw.SDL_SCANCODE_MAIL
pattern ScancodeCalculator :: Scancode
pattern ScancodeCalculator = Scancode Raw.SDL_SCANCODE_CALCULATOR
pattern ScancodeComputer :: Scancode
pattern ScancodeComputer = Scancode Raw.SDL_SCANCODE_COMPUTER
pattern ScancodeACSearch :: Scancode
pattern ScancodeACSearch = Scancode Raw.SDL_SCANCODE_AC_SEARCH
pattern ScancodeACHome :: Scancode
pattern ScancodeACHome = Scancode Raw.SDL_SCANCODE_AC_HOME
pattern ScancodeACBack :: Scancode
pattern ScancodeACBack = Scancode Raw.SDL_SCANCODE_AC_BACK
pattern ScancodeACForward :: Scancode
pattern ScancodeACForward = Scancode Raw.SDL_SCANCODE_AC_FORWARD
pattern ScancodeACStop :: Scancode
pattern ScancodeACStop = Scancode Raw.SDL_SCANCODE_AC_STOP
pattern ScancodeACRefresh :: Scancode
pattern ScancodeACRefresh = Scancode Raw.SDL_SCANCODE_AC_REFRESH
pattern ScancodeACBookmarks :: Scancode
pattern ScancodeACBookmarks = Scancode Raw.SDL_SCANCODE_AC_BOOKMARKS
pattern ScancodeBrightnessDown :: Scancode
pattern ScancodeBrightnessDown = Scancode Raw.SDL_SCANCODE_BRIGHTNESSDOWN
pattern ScancodeBrightnessUp :: Scancode
pattern ScancodeBrightnessUp = Scancode Raw.SDL_SCANCODE_BRIGHTNESSUP
pattern ScancodeDisplaySwitch :: Scancode
pattern ScancodeDisplaySwitch = Scancode Raw.SDL_SCANCODE_DISPLAYSWITCH
pattern ScancodeKBDIllumToggle :: Scancode
pattern ScancodeKBDIllumToggle = Scancode Raw.SDL_SCANCODE_KBDILLUMTOGGLE
pattern ScancodeKBDIllumDown :: Scancode
pattern ScancodeKBDIllumDown = Scancode Raw.SDL_SCANCODE_KBDILLUMDOWN
pattern ScancodeKBDIllumUp :: Scancode
pattern ScancodeKBDIllumUp = Scancode Raw.SDL_SCANCODE_KBDILLUMUP
pattern ScancodeEject :: Scancode
pattern ScancodeEject = Scancode Raw.SDL_SCANCODE_EJECT
pattern ScancodeSleep :: Scancode
pattern ScancodeSleep = Scancode Raw.SDL_SCANCODE_SLEEP
pattern ScancodeApp1 :: Scancode
pattern ScancodeApp1 = Scancode Raw.SDL_SCANCODE_APP1
pattern ScancodeApp2 :: Scancode
pattern ScancodeApp2 = Scancode Raw.SDL_SCANCODE_APP2

instance FromNumber Scancode Word32 where
  fromNumber = Scancode

instance ToNumber Scancode Word32 where
  toNumber = unwrapScancode

newtype Keycode = Keycode { unwrapKeycode :: Int32 }
  deriving (Bounded, Data, Eq, Ord, Read, Generic, Show, Typeable)

pattern KeycodeUnknown :: Keycode
pattern KeycodeUnknown = Keycode Raw.SDLK_UNKNOWN
pattern KeycodeReturn :: Keycode
pattern KeycodeReturn = Keycode Raw.SDLK_RETURN
pattern KeycodeEscape :: Keycode
pattern KeycodeEscape = Keycode Raw.SDLK_ESCAPE
pattern KeycodeBackspace :: Keycode
pattern KeycodeBackspace = Keycode Raw.SDLK_BACKSPACE
pattern KeycodeTab :: Keycode
pattern KeycodeTab = Keycode Raw.SDLK_TAB
pattern KeycodeSpace :: Keycode
pattern KeycodeSpace = Keycode Raw.SDLK_SPACE
pattern KeycodeExclaim :: Keycode
pattern KeycodeExclaim = Keycode Raw.SDLK_EXCLAIM
pattern KeycodeQuoteDbl :: Keycode
pattern KeycodeQuoteDbl = Keycode Raw.SDLK_QUOTEDBL
pattern KeycodeHash :: Keycode
pattern KeycodeHash = Keycode Raw.SDLK_HASH
pattern KeycodePercent :: Keycode
pattern KeycodePercent = Keycode Raw.SDLK_PERCENT
pattern KeycodeDollar :: Keycode
pattern KeycodeDollar = Keycode Raw.SDLK_DOLLAR
pattern KeycodeAmpersand :: Keycode
pattern KeycodeAmpersand = Keycode Raw.SDLK_AMPERSAND
pattern KeycodeQuote :: Keycode
pattern KeycodeQuote = Keycode Raw.SDLK_QUOTE
pattern KeycodeLeftParen :: Keycode
pattern KeycodeLeftParen = Keycode Raw.SDLK_LEFTPAREN
pattern KeycodeRightParen :: Keycode
pattern KeycodeRightParen = Keycode Raw.SDLK_RIGHTPAREN
pattern KeycodeAsterisk :: Keycode
pattern KeycodeAsterisk = Keycode Raw.SDLK_ASTERISK
pattern KeycodePlus :: Keycode
pattern KeycodePlus = Keycode Raw.SDLK_PLUS
pattern KeycodeComma :: Keycode
pattern KeycodeComma = Keycode Raw.SDLK_COMMA
pattern KeycodeMinus :: Keycode
pattern KeycodeMinus = Keycode Raw.SDLK_MINUS
pattern KeycodePeriod :: Keycode
pattern KeycodePeriod = Keycode Raw.SDLK_PERIOD
pattern KeycodeSlash :: Keycode
pattern KeycodeSlash = Keycode Raw.SDLK_SLASH
pattern Keycode0 :: Keycode
pattern Keycode0 = Keycode Raw.SDLK_0
pattern Keycode1 :: Keycode
pattern Keycode1 = Keycode Raw.SDLK_1
pattern Keycode2 :: Keycode
pattern Keycode2 = Keycode Raw.SDLK_2
pattern Keycode3 :: Keycode
pattern Keycode3 = Keycode Raw.SDLK_3
pattern Keycode4 :: Keycode
pattern Keycode4 = Keycode Raw.SDLK_4
pattern Keycode5 :: Keycode
pattern Keycode5 = Keycode Raw.SDLK_5
pattern Keycode6 :: Keycode
pattern Keycode6 = Keycode Raw.SDLK_6
pattern Keycode7 :: Keycode
pattern Keycode7 = Keycode Raw.SDLK_7
pattern Keycode8 :: Keycode
pattern Keycode8 = Keycode Raw.SDLK_8
pattern Keycode9 :: Keycode
pattern Keycode9 = Keycode Raw.SDLK_9
pattern KeycodeColon :: Keycode
pattern KeycodeColon = Keycode Raw.SDLK_COLON
pattern KeycodeSemicolon :: Keycode
pattern KeycodeSemicolon = Keycode Raw.SDLK_SEMICOLON
pattern KeycodeLess :: Keycode
pattern KeycodeLess = Keycode Raw.SDLK_LESS
pattern KeycodeEquals :: Keycode
pattern KeycodeEquals = Keycode Raw.SDLK_EQUALS
pattern KeycodeGreater :: Keycode
pattern KeycodeGreater = Keycode Raw.SDLK_GREATER
pattern KeycodeQuestion :: Keycode
pattern KeycodeQuestion = Keycode Raw.SDLK_QUESTION
pattern KeycodeAt :: Keycode
pattern KeycodeAt = Keycode Raw.SDLK_AT
pattern KeycodeLeftBracket :: Keycode
pattern KeycodeLeftBracket = Keycode Raw.SDLK_LEFTBRACKET
pattern KeycodeBackslash :: Keycode
pattern KeycodeBackslash = Keycode Raw.SDLK_BACKSLASH
pattern KeycodeRightBracket :: Keycode
pattern KeycodeRightBracket = Keycode Raw.SDLK_RIGHTBRACKET
pattern KeycodeCaret :: Keycode
pattern KeycodeCaret = Keycode Raw.SDLK_CARET
pattern KeycodeUnderscore :: Keycode
pattern KeycodeUnderscore = Keycode Raw.SDLK_UNDERSCORE
pattern KeycodeBackquote :: Keycode
pattern KeycodeBackquote = Keycode Raw.SDLK_BACKQUOTE
pattern KeycodeA :: Keycode
pattern KeycodeA = Keycode Raw.SDLK_a
pattern KeycodeB :: Keycode
pattern KeycodeB = Keycode Raw.SDLK_b
pattern KeycodeC :: Keycode
pattern KeycodeC = Keycode Raw.SDLK_c
pattern KeycodeD :: Keycode
pattern KeycodeD = Keycode Raw.SDLK_d
pattern KeycodeE :: Keycode
pattern KeycodeE = Keycode Raw.SDLK_e
pattern KeycodeF :: Keycode
pattern KeycodeF = Keycode Raw.SDLK_f
pattern KeycodeG :: Keycode
pattern KeycodeG = Keycode Raw.SDLK_g
pattern KeycodeH :: Keycode
pattern KeycodeH = Keycode Raw.SDLK_h
pattern KeycodeI :: Keycode
pattern KeycodeI = Keycode Raw.SDLK_i
pattern KeycodeJ :: Keycode
pattern KeycodeJ = Keycode Raw.SDLK_j
pattern KeycodeK :: Keycode
pattern KeycodeK = Keycode Raw.SDLK_k
pattern KeycodeL :: Keycode
pattern KeycodeL = Keycode Raw.SDLK_l
pattern KeycodeM :: Keycode
pattern KeycodeM = Keycode Raw.SDLK_m
pattern KeycodeN :: Keycode
pattern KeycodeN = Keycode Raw.SDLK_n
pattern KeycodeO :: Keycode
pattern KeycodeO = Keycode Raw.SDLK_o
pattern KeycodeP :: Keycode
pattern KeycodeP = Keycode Raw.SDLK_p
pattern KeycodeQ :: Keycode
pattern KeycodeQ = Keycode Raw.SDLK_q
pattern KeycodeR :: Keycode
pattern KeycodeR = Keycode Raw.SDLK_r
pattern KeycodeS :: Keycode
pattern KeycodeS = Keycode Raw.SDLK_s
pattern KeycodeT :: Keycode
pattern KeycodeT = Keycode Raw.SDLK_t
pattern KeycodeU :: Keycode
pattern KeycodeU = Keycode Raw.SDLK_u
pattern KeycodeV :: Keycode
pattern KeycodeV = Keycode Raw.SDLK_v
pattern KeycodeW :: Keycode
pattern KeycodeW = Keycode Raw.SDLK_w
pattern KeycodeX :: Keycode
pattern KeycodeX = Keycode Raw.SDLK_x
pattern KeycodeY :: Keycode
pattern KeycodeY = Keycode Raw.SDLK_y
pattern KeycodeZ :: Keycode
pattern KeycodeZ = Keycode Raw.SDLK_z
pattern KeycodeCapsLock :: Keycode
pattern KeycodeCapsLock = Keycode Raw.SDLK_CAPSLOCK
pattern KeycodeF1 :: Keycode
pattern KeycodeF1 = Keycode Raw.SDLK_F1
pattern KeycodeF2 :: Keycode
pattern KeycodeF2 = Keycode Raw.SDLK_F2
pattern KeycodeF3 :: Keycode
pattern KeycodeF3 = Keycode Raw.SDLK_F3
pattern KeycodeF4 :: Keycode
pattern KeycodeF4 = Keycode Raw.SDLK_F4
pattern KeycodeF5 :: Keycode
pattern KeycodeF5 = Keycode Raw.SDLK_F5
pattern KeycodeF6 :: Keycode
pattern KeycodeF6 = Keycode Raw.SDLK_F6
pattern KeycodeF7 :: Keycode
pattern KeycodeF7 = Keycode Raw.SDLK_F7
pattern KeycodeF8 :: Keycode
pattern KeycodeF8 = Keycode Raw.SDLK_F8
pattern KeycodeF9 :: Keycode
pattern KeycodeF9 = Keycode Raw.SDLK_F9
pattern KeycodeF10 :: Keycode
pattern KeycodeF10 = Keycode Raw.SDLK_F10
pattern KeycodeF11 :: Keycode
pattern KeycodeF11 = Keycode Raw.SDLK_F11
pattern KeycodeF12 :: Keycode
pattern KeycodeF12 = Keycode Raw.SDLK_F12
pattern KeycodePrintScreen :: Keycode
pattern KeycodePrintScreen = Keycode Raw.SDLK_PRINTSCREEN
pattern KeycodeScrollLock :: Keycode
pattern KeycodeScrollLock = Keycode Raw.SDLK_SCROLLLOCK
pattern KeycodePause :: Keycode
pattern KeycodePause = Keycode Raw.SDLK_PAUSE
pattern KeycodeInsert :: Keycode
pattern KeycodeInsert = Keycode Raw.SDLK_INSERT
pattern KeycodeHome :: Keycode
pattern KeycodeHome = Keycode Raw.SDLK_HOME
pattern KeycodePageUp :: Keycode
pattern KeycodePageUp = Keycode Raw.SDLK_PAGEUP
pattern KeycodeDelete :: Keycode
pattern KeycodeDelete = Keycode Raw.SDLK_DELETE
pattern KeycodeEnd :: Keycode
pattern KeycodeEnd = Keycode Raw.SDLK_END
pattern KeycodePageDown :: Keycode
pattern KeycodePageDown = Keycode Raw.SDLK_PAGEDOWN
pattern KeycodeRight :: Keycode
pattern KeycodeRight = Keycode Raw.SDLK_RIGHT
pattern KeycodeLeft :: Keycode
pattern KeycodeLeft = Keycode Raw.SDLK_LEFT
pattern KeycodeDown :: Keycode
pattern KeycodeDown = Keycode Raw.SDLK_DOWN
pattern KeycodeUp :: Keycode
pattern KeycodeUp = Keycode Raw.SDLK_UP
pattern KeycodeNumLockClear :: Keycode
pattern KeycodeNumLockClear = Keycode Raw.SDLK_NUMLOCKCLEAR
pattern KeycodeKPDivide :: Keycode
pattern KeycodeKPDivide = Keycode Raw.SDLK_KP_DIVIDE
pattern KeycodeKPMultiply :: Keycode
pattern KeycodeKPMultiply = Keycode Raw.SDLK_KP_MULTIPLY
pattern KeycodeKPMinus :: Keycode
pattern KeycodeKPMinus = Keycode Raw.SDLK_KP_MINUS
pattern KeycodeKPPlus :: Keycode
pattern KeycodeKPPlus = Keycode Raw.SDLK_KP_PLUS
pattern KeycodeKPEnter :: Keycode
pattern KeycodeKPEnter = Keycode Raw.SDLK_KP_ENTER
pattern KeycodeKP1 :: Keycode
pattern KeycodeKP1 = Keycode Raw.SDLK_KP_1
pattern KeycodeKP2 :: Keycode
pattern KeycodeKP2 = Keycode Raw.SDLK_KP_2
pattern KeycodeKP3 :: Keycode
pattern KeycodeKP3 = Keycode Raw.SDLK_KP_3
pattern KeycodeKP4 :: Keycode
pattern KeycodeKP4 = Keycode Raw.SDLK_KP_4
pattern KeycodeKP5 :: Keycode
pattern KeycodeKP5 = Keycode Raw.SDLK_KP_5
pattern KeycodeKP6 :: Keycode
pattern KeycodeKP6 = Keycode Raw.SDLK_KP_6
pattern KeycodeKP7 :: Keycode
pattern KeycodeKP7 = Keycode Raw.SDLK_KP_7
pattern KeycodeKP8 :: Keycode
pattern KeycodeKP8 = Keycode Raw.SDLK_KP_8
pattern KeycodeKP9 :: Keycode
pattern KeycodeKP9 = Keycode Raw.SDLK_KP_9
pattern KeycodeKP0 :: Keycode
pattern KeycodeKP0 = Keycode Raw.SDLK_KP_0
pattern KeycodeKPPeriod :: Keycode
pattern KeycodeKPPeriod = Keycode Raw.SDLK_KP_PERIOD
pattern KeycodeApplication :: Keycode
pattern KeycodeApplication = Keycode Raw.SDLK_APPLICATION
pattern KeycodePower :: Keycode
pattern KeycodePower = Keycode Raw.SDLK_POWER
pattern KeycodeKPEquals :: Keycode
pattern KeycodeKPEquals = Keycode Raw.SDLK_KP_EQUALS
pattern KeycodeF13 :: Keycode
pattern KeycodeF13 = Keycode Raw.SDLK_F13
pattern KeycodeF14 :: Keycode
pattern KeycodeF14 = Keycode Raw.SDLK_F14
pattern KeycodeF15 :: Keycode
pattern KeycodeF15 = Keycode Raw.SDLK_F15
pattern KeycodeF16 :: Keycode
pattern KeycodeF16 = Keycode Raw.SDLK_F16
pattern KeycodeF17 :: Keycode
pattern KeycodeF17 = Keycode Raw.SDLK_F17
pattern KeycodeF18 :: Keycode
pattern KeycodeF18 = Keycode Raw.SDLK_F18
pattern KeycodeF19 :: Keycode
pattern KeycodeF19 = Keycode Raw.SDLK_F19
pattern KeycodeF20 :: Keycode
pattern KeycodeF20 = Keycode Raw.SDLK_F20
pattern KeycodeF21 :: Keycode
pattern KeycodeF21 = Keycode Raw.SDLK_F21
pattern KeycodeF22 :: Keycode
pattern KeycodeF22 = Keycode Raw.SDLK_F22
pattern KeycodeF23 :: Keycode
pattern KeycodeF23 = Keycode Raw.SDLK_F23
pattern KeycodeF24 :: Keycode
pattern KeycodeF24 = Keycode Raw.SDLK_F24
pattern KeycodeExecute :: Keycode
pattern KeycodeExecute = Keycode Raw.SDLK_EXECUTE
pattern KeycodeHelp :: Keycode
pattern KeycodeHelp = Keycode Raw.SDLK_HELP
pattern KeycodeMenu :: Keycode
pattern KeycodeMenu = Keycode Raw.SDLK_MENU
pattern KeycodeSelect :: Keycode
pattern KeycodeSelect = Keycode Raw.SDLK_SELECT
pattern KeycodeStop :: Keycode
pattern KeycodeStop = Keycode Raw.SDLK_STOP
pattern KeycodeAgain :: Keycode
pattern KeycodeAgain = Keycode Raw.SDLK_AGAIN
pattern KeycodeUndo :: Keycode
pattern KeycodeUndo = Keycode Raw.SDLK_UNDO
pattern KeycodeCut :: Keycode
pattern KeycodeCut = Keycode Raw.SDLK_CUT
pattern KeycodeCopy :: Keycode
pattern KeycodeCopy = Keycode Raw.SDLK_COPY
pattern KeycodePaste :: Keycode
pattern KeycodePaste = Keycode Raw.SDLK_PASTE
pattern KeycodeFind :: Keycode
pattern KeycodeFind = Keycode Raw.SDLK_FIND
pattern KeycodeMute :: Keycode
pattern KeycodeMute = Keycode Raw.SDLK_MUTE
pattern KeycodeVolumeUp :: Keycode
pattern KeycodeVolumeUp = Keycode Raw.SDLK_VOLUMEUP
pattern KeycodeVolumeDown :: Keycode
pattern KeycodeVolumeDown = Keycode Raw.SDLK_VOLUMEDOWN
pattern KeycodeKPComma :: Keycode
pattern KeycodeKPComma = Keycode Raw.SDLK_KP_COMMA
pattern KeycodeKPEqualsAS400 :: Keycode
pattern KeycodeKPEqualsAS400 = Keycode Raw.SDLK_KP_EQUALSAS400
pattern KeycodeAltErase :: Keycode
pattern KeycodeAltErase = Keycode Raw.SDLK_ALTERASE
pattern KeycodeSysReq :: Keycode
pattern KeycodeSysReq = Keycode Raw.SDLK_SYSREQ
pattern KeycodeCancel :: Keycode
pattern KeycodeCancel = Keycode Raw.SDLK_CANCEL
pattern KeycodeClear :: Keycode
pattern KeycodeClear = Keycode Raw.SDLK_CLEAR
pattern KeycodePrior :: Keycode
pattern KeycodePrior = Keycode Raw.SDLK_PRIOR
pattern KeycodeReturn2 :: Keycode
pattern KeycodeReturn2 = Keycode Raw.SDLK_RETURN2
pattern KeycodeSeparator :: Keycode
pattern KeycodeSeparator = Keycode Raw.SDLK_SEPARATOR
pattern KeycodeOut :: Keycode
pattern KeycodeOut = Keycode Raw.SDLK_OUT
pattern KeycodeOper :: Keycode
pattern KeycodeOper = Keycode Raw.SDLK_OPER
pattern KeycodeClearAgain :: Keycode
pattern KeycodeClearAgain = Keycode Raw.SDLK_CLEARAGAIN
pattern KeycodeCrSel :: Keycode
pattern KeycodeCrSel = Keycode Raw.SDLK_CRSEL
pattern KeycodeExSel :: Keycode
pattern KeycodeExSel = Keycode Raw.SDLK_EXSEL
pattern KeycodeKP00 :: Keycode
pattern KeycodeKP00 = Keycode Raw.SDLK_KP_00
pattern KeycodeKP000 :: Keycode
pattern KeycodeKP000 = Keycode Raw.SDLK_KP_000
pattern KeycodeThousandsSeparator :: Keycode
pattern KeycodeThousandsSeparator = Keycode Raw.SDLK_THOUSANDSSEPARATOR
pattern KeycodeDecimalSeparator :: Keycode
pattern KeycodeDecimalSeparator = Keycode Raw.SDLK_DECIMALSEPARATOR
pattern KeycodeCurrencyUnit :: Keycode
pattern KeycodeCurrencyUnit = Keycode Raw.SDLK_CURRENCYUNIT
pattern KeycodeCurrencySubunit :: Keycode
pattern KeycodeCurrencySubunit = Keycode Raw.SDLK_CURRENCYSUBUNIT
pattern KeycodeKPLeftParen :: Keycode
pattern KeycodeKPLeftParen = Keycode Raw.SDLK_KP_LEFTPAREN
pattern KeycodeKPRightParen :: Keycode
pattern KeycodeKPRightParen = Keycode Raw.SDLK_KP_RIGHTPAREN
pattern KeycodeKPLeftBrace :: Keycode
pattern KeycodeKPLeftBrace = Keycode Raw.SDLK_KP_LEFTBRACE
pattern KeycodeKPRightBrace :: Keycode
pattern KeycodeKPRightBrace = Keycode Raw.SDLK_KP_RIGHTBRACE
pattern KeycodeKPTab :: Keycode
pattern KeycodeKPTab = Keycode Raw.SDLK_KP_TAB
pattern KeycodeKPBackspace :: Keycode
pattern KeycodeKPBackspace = Keycode Raw.SDLK_KP_BACKSPACE
pattern KeycodeKPA :: Keycode
pattern KeycodeKPA = Keycode Raw.SDLK_KP_A
pattern KeycodeKPB :: Keycode
pattern KeycodeKPB = Keycode Raw.SDLK_KP_B
pattern KeycodeKPC :: Keycode
pattern KeycodeKPC = Keycode Raw.SDLK_KP_C
pattern KeycodeKPD :: Keycode
pattern KeycodeKPD = Keycode Raw.SDLK_KP_D
pattern KeycodeKPE :: Keycode
pattern KeycodeKPE = Keycode Raw.SDLK_KP_E
pattern KeycodeKPF :: Keycode
pattern KeycodeKPF = Keycode Raw.SDLK_KP_F
pattern KeycodeKPXor :: Keycode
pattern KeycodeKPXor = Keycode Raw.SDLK_KP_XOR
pattern KeycodeKPPower :: Keycode
pattern KeycodeKPPower = Keycode Raw.SDLK_KP_POWER
pattern KeycodeKPPercent :: Keycode
pattern KeycodeKPPercent = Keycode Raw.SDLK_KP_PERCENT
pattern KeycodeKPLess :: Keycode
pattern KeycodeKPLess = Keycode Raw.SDLK_KP_LESS
pattern KeycodeKPGreater :: Keycode
pattern KeycodeKPGreater = Keycode Raw.SDLK_KP_GREATER
pattern KeycodeKPAmpersand :: Keycode
pattern KeycodeKPAmpersand = Keycode Raw.SDLK_KP_AMPERSAND
pattern KeycodeKPDblAmpersand :: Keycode
pattern KeycodeKPDblAmpersand = Keycode Raw.SDLK_KP_DBLAMPERSAND
pattern KeycodeKPVerticalBar :: Keycode
pattern KeycodeKPVerticalBar = Keycode Raw.SDLK_KP_VERTICALBAR
pattern KeycodeKPDblVerticalBar :: Keycode
pattern KeycodeKPDblVerticalBar = Keycode Raw.SDLK_KP_DBLVERTICALBAR
pattern KeycodeKPColon :: Keycode
pattern KeycodeKPColon = Keycode Raw.SDLK_KP_COLON
pattern KeycodeKPHash :: Keycode
pattern KeycodeKPHash = Keycode Raw.SDLK_KP_HASH
pattern KeycodeKPSpace :: Keycode
pattern KeycodeKPSpace = Keycode Raw.SDLK_KP_SPACE
pattern KeycodeKPAt :: Keycode
pattern KeycodeKPAt = Keycode Raw.SDLK_KP_AT
pattern KeycodeKPExclam :: Keycode
pattern KeycodeKPExclam = Keycode Raw.SDLK_KP_EXCLAM
pattern KeycodeKPMemStore :: Keycode
pattern KeycodeKPMemStore = Keycode Raw.SDLK_KP_MEMSTORE
pattern KeycodeKPMemRecall :: Keycode
pattern KeycodeKPMemRecall = Keycode Raw.SDLK_KP_MEMRECALL
pattern KeycodeKPMemClear :: Keycode
pattern KeycodeKPMemClear = Keycode Raw.SDLK_KP_MEMCLEAR
pattern KeycodeKPMemAdd :: Keycode
pattern KeycodeKPMemAdd = Keycode Raw.SDLK_KP_MEMADD
pattern KeycodeKPMemSubtract :: Keycode
pattern KeycodeKPMemSubtract = Keycode Raw.SDLK_KP_MEMSUBTRACT
pattern KeycodeKPMemMultiply :: Keycode
pattern KeycodeKPMemMultiply = Keycode Raw.SDLK_KP_MEMMULTIPLY
pattern KeycodeKPMemDivide :: Keycode
pattern KeycodeKPMemDivide = Keycode Raw.SDLK_KP_MEMDIVIDE
pattern KeycodeKPPlusMinus :: Keycode
pattern KeycodeKPPlusMinus = Keycode Raw.SDLK_KP_PLUSMINUS
pattern KeycodeKPClear :: Keycode
pattern KeycodeKPClear = Keycode Raw.SDLK_KP_CLEAR
pattern KeycodeKPClearEntry :: Keycode
pattern KeycodeKPClearEntry = Keycode Raw.SDLK_KP_CLEARENTRY
pattern KeycodeKPBinary :: Keycode
pattern KeycodeKPBinary = Keycode Raw.SDLK_KP_BINARY
pattern KeycodeKPOctal :: Keycode
pattern KeycodeKPOctal = Keycode Raw.SDLK_KP_OCTAL
pattern KeycodeKPDecimal :: Keycode
pattern KeycodeKPDecimal = Keycode Raw.SDLK_KP_DECIMAL
pattern KeycodeKPHexadecimal :: Keycode
pattern KeycodeKPHexadecimal = Keycode Raw.SDLK_KP_HEXADECIMAL
pattern KeycodeLCtrl :: Keycode
pattern KeycodeLCtrl = Keycode Raw.SDLK_LCTRL
pattern KeycodeLShift :: Keycode
pattern KeycodeLShift = Keycode Raw.SDLK_LSHIFT
pattern KeycodeLAlt :: Keycode
pattern KeycodeLAlt = Keycode Raw.SDLK_LALT
pattern KeycodeLGUI :: Keycode
pattern KeycodeLGUI = Keycode Raw.SDLK_LGUI
pattern KeycodeRCtrl :: Keycode
pattern KeycodeRCtrl = Keycode Raw.SDLK_RCTRL
pattern KeycodeRShift :: Keycode
pattern KeycodeRShift = Keycode Raw.SDLK_RSHIFT
pattern KeycodeRAlt :: Keycode
pattern KeycodeRAlt = Keycode Raw.SDLK_RALT
pattern KeycodeRGUI :: Keycode
pattern KeycodeRGUI = Keycode Raw.SDLK_RGUI
pattern KeycodeMode :: Keycode
pattern KeycodeMode = Keycode Raw.SDLK_MODE
pattern KeycodeAudioNext :: Keycode
pattern KeycodeAudioNext = Keycode Raw.SDLK_AUDIONEXT
pattern KeycodeAudioPrev :: Keycode
pattern KeycodeAudioPrev = Keycode Raw.SDLK_AUDIOPREV
pattern KeycodeAudioStop :: Keycode
pattern KeycodeAudioStop = Keycode Raw.SDLK_AUDIOSTOP
pattern KeycodeAudioPlay :: Keycode
pattern KeycodeAudioPlay = Keycode Raw.SDLK_AUDIOPLAY
pattern KeycodeAudioMute :: Keycode
pattern KeycodeAudioMute = Keycode Raw.SDLK_AUDIOMUTE
pattern KeycodeMediaSelect :: Keycode
pattern KeycodeMediaSelect = Keycode Raw.SDLK_MEDIASELECT
pattern KeycodeWWW :: Keycode
pattern KeycodeWWW = Keycode Raw.SDLK_WWW
pattern KeycodeMail :: Keycode
pattern KeycodeMail = Keycode Raw.SDLK_MAIL
pattern KeycodeCalculator :: Keycode
pattern KeycodeCalculator = Keycode Raw.SDLK_CALCULATOR
pattern KeycodeComputer :: Keycode
pattern KeycodeComputer = Keycode Raw.SDLK_COMPUTER
pattern KeycodeACSearch :: Keycode
pattern KeycodeACSearch = Keycode Raw.SDLK_AC_SEARCH
pattern KeycodeACHome :: Keycode
pattern KeycodeACHome = Keycode Raw.SDLK_AC_HOME
pattern KeycodeACBack :: Keycode
pattern KeycodeACBack = Keycode Raw.SDLK_AC_BACK
pattern KeycodeACForward :: Keycode
pattern KeycodeACForward = Keycode Raw.SDLK_AC_FORWARD
pattern KeycodeACStop :: Keycode
pattern KeycodeACStop = Keycode Raw.SDLK_AC_STOP
pattern KeycodeACRefresh :: Keycode
pattern KeycodeACRefresh = Keycode Raw.SDLK_AC_REFRESH
pattern KeycodeACBookmarks :: Keycode
pattern KeycodeACBookmarks = Keycode Raw.SDLK_AC_BOOKMARKS
pattern KeycodeBrightnessDown :: Keycode
pattern KeycodeBrightnessDown = Keycode Raw.SDLK_BRIGHTNESSDOWN
pattern KeycodeBrightnessUp :: Keycode
pattern KeycodeBrightnessUp = Keycode Raw.SDLK_BRIGHTNESSUP
pattern KeycodeDisplaySwitch :: Keycode
pattern KeycodeDisplaySwitch = Keycode Raw.SDLK_DISPLAYSWITCH
pattern KeycodeKbdIllumToggle :: Keycode
pattern KeycodeKbdIllumToggle = Keycode Raw.SDLK_KBDILLUMTOGGLE
pattern KeycodeKbdIllumDown :: Keycode
pattern KeycodeKbdIllumDown = Keycode Raw.SDLK_KBDILLUMDOWN
pattern KeycodeKbdIllumUp :: Keycode
pattern KeycodeKbdIllumUp = Keycode Raw.SDLK_KBDILLUMUP
pattern KeycodeEject :: Keycode
pattern KeycodeEject = Keycode Raw.SDLK_EJECT
pattern KeycodeSleep :: Keycode
pattern KeycodeSleep = Keycode Raw.SDLK_SLEEP

instance FromNumber Keycode Int32 where
  fromNumber = Keycode

instance ToNumber Keycode Int32 where
  toNumber = unwrapKeycode
