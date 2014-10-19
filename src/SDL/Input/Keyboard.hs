{-# LANGUAGE MultiParamTypeClasses #-}
module SDL.Input.Keyboard
  ( -- * Keyboard Modifiers
    getModState
  , KeyModifier(..)

  -- * Text Input
  , startTextInput
  , stopTextInput

  -- * Screen Keyboard
  , hasScreenKeyboardSupport
  , isScreenKeyboardShown

  -- * Scancodes
  , getScancodeName
  , Scancode(..)
) where

import Control.Applicative
import Data.Bits
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import SDL.Internal.Numbered
import SDL.Internal.Types

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
  } deriving (Eq, Show)

instance FromNumber KeyModifier Word32 where
  fromNumber m' = let m = m' in KeyModifier
    { keyModifierLeftShift  = m .&. Raw.keymodLShift > 0
    , keyModifierRightShift = m .&. Raw.keymodRShift > 0
    , keyModifierLeftCtrl   = m .&. Raw.keymodLCtrl  > 0
    , keyModifierRightCtrl  = m .&. Raw.keymodRCtrl  > 0
    , keyModifierLeftAlt    = m .&. Raw.keymodLAlt   > 0
    , keyModifierRightAlt   = m .&. Raw.keymodRAlt   > 0
    , keyModifierLeftGUI    = m .&. Raw.keymodLGUI   > 0
    , keyModifierRightGUI   = m .&. Raw.keymodRGUI   > 0
    , keyModifierNumLock    = m .&. Raw.keymodNum    > 0
    , keyModifierCapsLock   = m .&. Raw.keymodCaps   > 0
    , keyModifierAltGr      = m .&. Raw.keymodMode   > 0
    }

instance ToNumber KeyModifier Word32 where
  toNumber m = foldr (.|.) 0
    [ if keyModifierLeftShift m  then Raw.keymodLShift else 0
    , if keyModifierRightShift m then Raw.keymodRShift else 0
    , if keyModifierLeftCtrl m   then Raw.keymodLCtrl  else 0
    , if keyModifierRightCtrl m  then Raw.keymodRCtrl  else 0
    , if keyModifierLeftAlt m    then Raw.keymodLAlt   else 0
    , if keyModifierRightAlt m   then Raw.keymodRAlt   else 0
    , if keyModifierLeftGUI m    then Raw.keymodLGUI   else 0
    , if keyModifierRightGUI m   then Raw.keymodRGUI   else 0
    , if keyModifierNumLock m    then Raw.keymodNum    else 0
    , if keyModifierCapsLock m   then Raw.keymodCaps   else 0
    , if keyModifierAltGr m      then Raw.keymodMode   else 0
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
  | ScancodeEqualsAs400
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
  | ScancodeKPDBLAmpersand
  | ScancodeKPVerticalBar
  | ScancodeKPDBLVerticalBar
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
  | ScancodeNum
  deriving (Eq, Show)

instance FromNumber Scancode Word32 where
  fromNumber n' = case n' of
    n | n == Raw.scancodeUnknown -> ScancodeUnknown
    n | n == Raw.scancodeA -> ScancodeA
    n | n == Raw.scancodeB -> ScancodeB
    n | n == Raw.scancodeC -> ScancodeC
    n | n == Raw.scancodeD -> ScancodeD
    n | n == Raw.scancodeE -> ScancodeE
    n | n == Raw.scancodeF -> ScancodeF
    n | n == Raw.scancodeG -> ScancodeG
    n | n == Raw.scancodeH -> ScancodeH
    n | n == Raw.scancodeI -> ScancodeI
    n | n == Raw.scancodeJ -> ScancodeJ
    n | n == Raw.scancodeK -> ScancodeK
    n | n == Raw.scancodeL -> ScancodeL
    n | n == Raw.scancodeM -> ScancodeM
    n | n == Raw.scancodeN -> ScancodeN
    n | n == Raw.scancodeO -> ScancodeO
    n | n == Raw.scancodeP -> ScancodeP
    n | n == Raw.scancodeQ -> ScancodeQ
    n | n == Raw.scancodeR -> ScancodeR
    n | n == Raw.scancodeS -> ScancodeS
    n | n == Raw.scancodeT -> ScancodeT
    n | n == Raw.scancodeU -> ScancodeU
    n | n == Raw.scancodeV -> ScancodeV
    n | n == Raw.scancodeW -> ScancodeW
    n | n == Raw.scancodeX -> ScancodeX
    n | n == Raw.scancodeY -> ScancodeY
    n | n == Raw.scancodeZ -> ScancodeZ
    n | n == Raw.scancode1 -> Scancode1
    n | n == Raw.scancode2 -> Scancode2
    n | n == Raw.scancode3 -> Scancode3
    n | n == Raw.scancode4 -> Scancode4
    n | n == Raw.scancode5 -> Scancode5
    n | n == Raw.scancode6 -> Scancode6
    n | n == Raw.scancode7 -> Scancode7
    n | n == Raw.scancode8 -> Scancode8
    n | n == Raw.scancode9 -> Scancode9
    n | n == Raw.scancode0 -> Scancode0
    n | n == Raw.scancodeReturn -> ScancodeReturn
    n | n == Raw.scancodeEscape -> ScancodeEscape
    n | n == Raw.scancodeBackspace -> ScancodeBackspace
    n | n == Raw.scancodeTab -> ScancodeTab
    n | n == Raw.scancodeSpace -> ScancodeSpace
    n | n == Raw.scancodeMinus -> ScancodeMinus
    n | n == Raw.scancodeEquals -> ScancodeEquals
    n | n == Raw.scancodeLeftBracket -> ScancodeLeftBracket
    n | n == Raw.scancodeRightBracket -> ScancodeRightBracket
    n | n == Raw.scancodeBackslash -> ScancodeBackslash
    n | n == Raw.scancodeNonUSHash -> ScancodeNonUSHash
    n | n == Raw.scancodeSemicolon -> ScancodeSemicolon
    n | n == Raw.scancodeApostrophe -> ScancodeApostrophe
    n | n == Raw.scancodeGrave -> ScancodeGrave
    n | n == Raw.scancodeComma -> ScancodeComma
    n | n == Raw.scancodePeriod -> ScancodePeriod
    n | n == Raw.scancodeSlash -> ScancodeSlash
    n | n == Raw.scancodeCapsLock -> ScancodeCapsLock
    n | n == Raw.scancodeF1 -> ScancodeF1
    n | n == Raw.scancodeF2 -> ScancodeF2
    n | n == Raw.scancodeF3 -> ScancodeF3
    n | n == Raw.scancodeF4 -> ScancodeF4
    n | n == Raw.scancodeF5 -> ScancodeF5
    n | n == Raw.scancodeF6 -> ScancodeF6
    n | n == Raw.scancodeF7 -> ScancodeF7
    n | n == Raw.scancodeF8 -> ScancodeF8
    n | n == Raw.scancodeF9 -> ScancodeF9
    n | n == Raw.scancodeF10 -> ScancodeF10
    n | n == Raw.scancodeF11 -> ScancodeF11
    n | n == Raw.scancodeF12 -> ScancodeF12
    n | n == Raw.scancodePrintScreen -> ScancodePrintScreen
    n | n == Raw.scancodeScrollLock -> ScancodeScrollLock
    n | n == Raw.scancodePause -> ScancodePause
    n | n == Raw.scancodeInsert -> ScancodeInsert
    n | n == Raw.scancodeHome -> ScancodeHome
    n | n == Raw.scancodePageUp -> ScancodePageUp
    n | n == Raw.scancodeDelete -> ScancodeDelete
    n | n == Raw.scancodeEnd -> ScancodeEnd
    n | n == Raw.scancodePageDown -> ScancodePageDown
    n | n == Raw.scancodeRight -> ScancodeRight
    n | n == Raw.scancodeLeft -> ScancodeLeft
    n | n == Raw.scancodeDown -> ScancodeDown
    n | n == Raw.scancodeUp -> ScancodeUp
    n | n == Raw.scancodeNumLockClear -> ScancodeNumLockClear
    n | n == Raw.scancodeKPDivide -> ScancodeKPDivide
    n | n == Raw.scancodeKPMultiply -> ScancodeKPMultiply
    n | n == Raw.scancodeKPMinus -> ScancodeKPMinus
    n | n == Raw.scancodeKPPlus -> ScancodeKPPlus
    n | n == Raw.scancodeKPEnter -> ScancodeKPEnter
    n | n == Raw.scancodeKP1 -> ScancodeKP1
    n | n == Raw.scancodeKP2 -> ScancodeKP2
    n | n == Raw.scancodeKP3 -> ScancodeKP3
    n | n == Raw.scancodeKP4 -> ScancodeKP4
    n | n == Raw.scancodeKP5 -> ScancodeKP5
    n | n == Raw.scancodeKP6 -> ScancodeKP6
    n | n == Raw.scancodeKP7 -> ScancodeKP7
    n | n == Raw.scancodeKP8 -> ScancodeKP8
    n | n == Raw.scancodeKP9 -> ScancodeKP9
    n | n == Raw.scancodeKP0 -> ScancodeKP0
    n | n == Raw.scancodeKPPeriod -> ScancodeKPPeriod
    n | n == Raw.scancodeNonUSBackslash -> ScancodeNonUSBackslash
    n | n == Raw.scancodeApplication -> ScancodeApplication
    n | n == Raw.scancodePower -> ScancodePower
    n | n == Raw.scancodeKPEquals -> ScancodeKPEquals
    n | n == Raw.scancodeF13 -> ScancodeF13
    n | n == Raw.scancodeF14 -> ScancodeF14
    n | n == Raw.scancodeF15 -> ScancodeF15
    n | n == Raw.scancodeF16 -> ScancodeF16
    n | n == Raw.scancodeF17 -> ScancodeF17
    n | n == Raw.scancodeF18 -> ScancodeF18
    n | n == Raw.scancodeF19 -> ScancodeF19
    n | n == Raw.scancodeF20 -> ScancodeF20
    n | n == Raw.scancodeF21 -> ScancodeF21
    n | n == Raw.scancodeF22 -> ScancodeF22
    n | n == Raw.scancodeF23 -> ScancodeF23
    n | n == Raw.scancodeF24 -> ScancodeF24
    n | n == Raw.scancodeExecute -> ScancodeExecute
    n | n == Raw.scancodeHelp -> ScancodeHelp
    n | n == Raw.scancodeMenu -> ScancodeMenu
    n | n == Raw.scancodeSelect -> ScancodeSelect
    n | n == Raw.scancodeStop -> ScancodeStop
    n | n == Raw.scancodeAgain -> ScancodeAgain
    n | n == Raw.scancodeUndo -> ScancodeUndo
    n | n == Raw.scancodeCut -> ScancodeCut
    n | n == Raw.scancodeCopy -> ScancodeCopy
    n | n == Raw.scancodePaste -> ScancodePaste
    n | n == Raw.scancodeFind -> ScancodeFind
    n | n == Raw.scancodeMute -> ScancodeMute
    n | n == Raw.scancodeVolumeUp -> ScancodeVolumeUp
    n | n == Raw.scancodeVolumeDown -> ScancodeVolumeDown
    n | n == Raw.scancodeKPComma -> ScancodeKPComma
    n | n == Raw.scancodeEqualsAs400 -> ScancodeEqualsAs400
    n | n == Raw.scancodeInternational1 -> ScancodeInternational1
    n | n == Raw.scancodeInternational2 -> ScancodeInternational2
    n | n == Raw.scancodeInternational3 -> ScancodeInternational3
    n | n == Raw.scancodeInternational4 -> ScancodeInternational4
    n | n == Raw.scancodeInternational5 -> ScancodeInternational5
    n | n == Raw.scancodeInternational6 -> ScancodeInternational6
    n | n == Raw.scancodeInternational7 -> ScancodeInternational7
    n | n == Raw.scancodeInternational8 -> ScancodeInternational8
    n | n == Raw.scancodeInternational9 -> ScancodeInternational9
    n | n == Raw.scancodeLang1 -> ScancodeLang1
    n | n == Raw.scancodeLang2 -> ScancodeLang2
    n | n == Raw.scancodeLang3 -> ScancodeLang3
    n | n == Raw.scancodeLang4 -> ScancodeLang4
    n | n == Raw.scancodeLang5 -> ScancodeLang5
    n | n == Raw.scancodeLang6 -> ScancodeLang6
    n | n == Raw.scancodeLang7 -> ScancodeLang7
    n | n == Raw.scancodeLang8 -> ScancodeLang8
    n | n == Raw.scancodeLang9 -> ScancodeLang9
    n | n == Raw.scancodeAltErase -> ScancodeAltErase
    n | n == Raw.scancodeSysReq -> ScancodeSysReq
    n | n == Raw.scancodeCancel -> ScancodeCancel
    n | n == Raw.scancodeClear -> ScancodeClear
    n | n == Raw.scancodePrior -> ScancodePrior
    n | n == Raw.scancodeReturn2 -> ScancodeReturn2
    n | n == Raw.scancodeSeparator -> ScancodeSeparator
    n | n == Raw.scancodeOut -> ScancodeOut
    n | n == Raw.scancodeOper -> ScancodeOper
    n | n == Raw.scancodeClearAgain -> ScancodeClearAgain
    n | n == Raw.scancodeCrSel -> ScancodeCrSel
    n | n == Raw.scancodeExSel -> ScancodeExSel
    n | n == Raw.scancodeKP00 -> ScancodeKP00
    n | n == Raw.scancodeKP000 -> ScancodeKP000
    n | n == Raw.scancodeThousandsSeparator -> ScancodeThousandsSeparator
    n | n == Raw.scancodeDecimalSeparator -> ScancodeDecimalSeparator
    n | n == Raw.scancodeCurrencyUnit -> ScancodeCurrencyUnit
    n | n == Raw.scancodeCurrencySubunit -> ScancodeCurrencySubunit
    n | n == Raw.scancodeLeftParen -> ScancodeLeftParen
    n | n == Raw.scancodeRightParen -> ScancodeRightParen
    n | n == Raw.scancodeLeftBrace -> ScancodeLeftBrace
    n | n == Raw.scancodeRightBrace -> ScancodeRightBrace
    n | n == Raw.scancodeKPTab -> ScancodeKPTab
    n | n == Raw.scancodeKPBackspace -> ScancodeKPBackspace
    n | n == Raw.scancodeKPA -> ScancodeKPA
    n | n == Raw.scancodeKPB -> ScancodeKPB
    n | n == Raw.scancodeKPC -> ScancodeKPC
    n | n == Raw.scancodeKPD -> ScancodeKPD
    n | n == Raw.scancodeKPE -> ScancodeKPE
    n | n == Raw.scancodeKPF -> ScancodeKPF
    n | n == Raw.scancodeKPXOR -> ScancodeKPXOR
    n | n == Raw.scancodeKPPower -> ScancodeKPPower
    n | n == Raw.scancodeKPPercent -> ScancodeKPPercent
    n | n == Raw.scancodeKPLess -> ScancodeKPLess
    n | n == Raw.scancodeKPGreater -> ScancodeKPGreater
    n | n == Raw.scancodeKPAmpersand -> ScancodeKPAmpersand
    n | n == Raw.scancodeKPDBLAmpersand -> ScancodeKPDBLAmpersand
    n | n == Raw.scancodeKPVerticalBar -> ScancodeKPVerticalBar
    n | n == Raw.scancodeKPDBLVerticalBar -> ScancodeKPDBLVerticalBar
    n | n == Raw.scancodeKPColon -> ScancodeKPColon
    n | n == Raw.scancodeKPHash -> ScancodeKPHash
    n | n == Raw.scancodeKPSpace -> ScancodeKPSpace
    n | n == Raw.scancodeKPAt -> ScancodeKPAt
    n | n == Raw.scancodeKPExclam -> ScancodeKPExclam
    n | n == Raw.scancodeKPMemStore -> ScancodeKPMemStore
    n | n == Raw.scancodeKPMemRecall -> ScancodeKPMemRecall
    n | n == Raw.scancodeKPMemClear -> ScancodeKPMemClear
    n | n == Raw.scancodeKPMemAdd -> ScancodeKPMemAdd
    n | n == Raw.scancodeKPMemSubtract -> ScancodeKPMemSubtract
    n | n == Raw.scancodeKPMemMultiply -> ScancodeKPMemMultiply
    n | n == Raw.scancodeKPMemDivide -> ScancodeKPMemDivide
    n | n == Raw.scancodeKPPlusMinus -> ScancodeKPPlusMinus
    n | n == Raw.scancodeKPClear -> ScancodeKPClear
    n | n == Raw.scancodeKPClearEntry -> ScancodeKPClearEntry
    n | n == Raw.scancodeKPBinary -> ScancodeKPBinary
    n | n == Raw.scancodeKPOctal -> ScancodeKPOctal
    n | n == Raw.scancodeKPDecimal -> ScancodeKPDecimal
    n | n == Raw.scancodeKPHexadecimal -> ScancodeKPHexadecimal
    n | n == Raw.scancodeLCtrl -> ScancodeLCtrl
    n | n == Raw.scancodeLShift -> ScancodeLShift
    n | n == Raw.scancodeLAlt -> ScancodeLAlt
    n | n == Raw.scancodeLGUI -> ScancodeLGUI
    n | n == Raw.scancodeRCtrl -> ScancodeRCtrl
    n | n == Raw.scancodeRShift -> ScancodeRShift
    n | n == Raw.scancodeRAlt -> ScancodeRAlt
    n | n == Raw.scancodeRGUI -> ScancodeRGUI
    n | n == Raw.scancodeMode -> ScancodeMode
    n | n == Raw.scancodeAudioNext -> ScancodeAudioNext
    n | n == Raw.scancodeAudioPrev -> ScancodeAudioPrev
    n | n == Raw.scancodeAudioStop -> ScancodeAudioStop
    n | n == Raw.scancodeAudioPlay -> ScancodeAudioPlay
    n | n == Raw.scancodeAudioMute -> ScancodeAudioMute
    n | n == Raw.scancodeMediaSelect -> ScancodeMediaSelect
    n | n == Raw.scancodeWWW -> ScancodeWWW
    n | n == Raw.scancodeMail -> ScancodeMail
    n | n == Raw.scancodeCalculator -> ScancodeCalculator
    n | n == Raw.scancodeComputer -> ScancodeComputer
    n | n == Raw.scancodeACSearch -> ScancodeACSearch
    n | n == Raw.scancodeACHome -> ScancodeACHome
    n | n == Raw.scancodeACBack -> ScancodeACBack
    n | n == Raw.scancodeACForward -> ScancodeACForward
    n | n == Raw.scancodeACStop -> ScancodeACStop
    n | n == Raw.scancodeACRefresh -> ScancodeACRefresh
    n | n == Raw.scancodeACBookmarks -> ScancodeACBookmarks
    n | n == Raw.scancodeBrightnessDown -> ScancodeBrightnessDown
    n | n == Raw.scancodeBrightnessUp -> ScancodeBrightnessUp
    n | n == Raw.scancodeDisplaySwitch -> ScancodeDisplaySwitch
    n | n == Raw.scancodeKBDIllumToggle -> ScancodeKBDIllumToggle
    n | n == Raw.scancodeKBDIllumDown -> ScancodeKBDIllumDown
    n | n == Raw.scancodeKBDIllumUp -> ScancodeKBDIllumUp
    n | n == Raw.scancodeEject -> ScancodeEject
    n | n == Raw.scancodeSleep -> ScancodeSleep
    n | n == Raw.scancodeApp1 -> ScancodeApp1
    n | n == Raw.scancodeApp2 -> ScancodeApp2
    n | n == Raw.scancodeNum -> ScancodeNum
    _ -> error "fromNumber: not numbered"

instance ToNumber Scancode Word32 where
  toNumber ScancodeUnknown = Raw.scancodeUnknown
  toNumber ScancodeA = Raw.scancodeA
  toNumber ScancodeB = Raw.scancodeB
  toNumber ScancodeC = Raw.scancodeC
  toNumber ScancodeD = Raw.scancodeD
  toNumber ScancodeE = Raw.scancodeE
  toNumber ScancodeF = Raw.scancodeF
  toNumber ScancodeG = Raw.scancodeG
  toNumber ScancodeH = Raw.scancodeH
  toNumber ScancodeI = Raw.scancodeI
  toNumber ScancodeJ = Raw.scancodeJ
  toNumber ScancodeK = Raw.scancodeK
  toNumber ScancodeL = Raw.scancodeL
  toNumber ScancodeM = Raw.scancodeM
  toNumber ScancodeN = Raw.scancodeN
  toNumber ScancodeO = Raw.scancodeO
  toNumber ScancodeP = Raw.scancodeP
  toNumber ScancodeQ = Raw.scancodeQ
  toNumber ScancodeR = Raw.scancodeR
  toNumber ScancodeS = Raw.scancodeS
  toNumber ScancodeT = Raw.scancodeT
  toNumber ScancodeU = Raw.scancodeU
  toNumber ScancodeV = Raw.scancodeV
  toNumber ScancodeW = Raw.scancodeW
  toNumber ScancodeX = Raw.scancodeX
  toNumber ScancodeY = Raw.scancodeY
  toNumber ScancodeZ = Raw.scancodeZ
  toNumber Scancode1 = Raw.scancode1
  toNumber Scancode2 = Raw.scancode2
  toNumber Scancode3 = Raw.scancode3
  toNumber Scancode4 = Raw.scancode4
  toNumber Scancode5 = Raw.scancode5
  toNumber Scancode6 = Raw.scancode6
  toNumber Scancode7 = Raw.scancode7
  toNumber Scancode8 = Raw.scancode8
  toNumber Scancode9 = Raw.scancode9
  toNumber Scancode0 = Raw.scancode0
  toNumber ScancodeReturn = Raw.scancodeReturn
  toNumber ScancodeEscape = Raw.scancodeEscape
  toNumber ScancodeBackspace = Raw.scancodeBackspace
  toNumber ScancodeTab = Raw.scancodeTab
  toNumber ScancodeSpace = Raw.scancodeSpace
  toNumber ScancodeMinus = Raw.scancodeMinus
  toNumber ScancodeEquals = Raw.scancodeEquals
  toNumber ScancodeLeftBracket = Raw.scancodeLeftBracket
  toNumber ScancodeRightBracket = Raw.scancodeRightBracket
  toNumber ScancodeBackslash = Raw.scancodeBackslash
  toNumber ScancodeNonUSHash = Raw.scancodeNonUSHash
  toNumber ScancodeSemicolon = Raw.scancodeSemicolon
  toNumber ScancodeApostrophe = Raw.scancodeApostrophe
  toNumber ScancodeGrave = Raw.scancodeGrave
  toNumber ScancodeComma = Raw.scancodeComma
  toNumber ScancodePeriod = Raw.scancodePeriod
  toNumber ScancodeSlash = Raw.scancodeSlash
  toNumber ScancodeCapsLock = Raw.scancodeCapsLock
  toNumber ScancodeF1 = Raw.scancodeF1
  toNumber ScancodeF2 = Raw.scancodeF2
  toNumber ScancodeF3 = Raw.scancodeF3
  toNumber ScancodeF4 = Raw.scancodeF4
  toNumber ScancodeF5 = Raw.scancodeF5
  toNumber ScancodeF6 = Raw.scancodeF6
  toNumber ScancodeF7 = Raw.scancodeF7
  toNumber ScancodeF8 = Raw.scancodeF8
  toNumber ScancodeF9 = Raw.scancodeF9
  toNumber ScancodeF10 = Raw.scancodeF10
  toNumber ScancodeF11 = Raw.scancodeF11
  toNumber ScancodeF12 = Raw.scancodeF12
  toNumber ScancodePrintScreen = Raw.scancodePrintScreen
  toNumber ScancodeScrollLock = Raw.scancodeScrollLock
  toNumber ScancodePause = Raw.scancodePause
  toNumber ScancodeInsert = Raw.scancodeInsert
  toNumber ScancodeHome = Raw.scancodeHome
  toNumber ScancodePageUp = Raw.scancodePageUp
  toNumber ScancodeDelete = Raw.scancodeDelete
  toNumber ScancodeEnd = Raw.scancodeEnd
  toNumber ScancodePageDown = Raw.scancodePageDown
  toNumber ScancodeRight = Raw.scancodeRight
  toNumber ScancodeLeft = Raw.scancodeLeft
  toNumber ScancodeDown = Raw.scancodeDown
  toNumber ScancodeUp = Raw.scancodeUp
  toNumber ScancodeNumLockClear = Raw.scancodeNumLockClear
  toNumber ScancodeKPDivide = Raw.scancodeKPDivide
  toNumber ScancodeKPMultiply = Raw.scancodeKPMultiply
  toNumber ScancodeKPMinus = Raw.scancodeKPMinus
  toNumber ScancodeKPPlus = Raw.scancodeKPPlus
  toNumber ScancodeKPEnter = Raw.scancodeKPEnter
  toNumber ScancodeKP1 = Raw.scancodeKP1
  toNumber ScancodeKP2 = Raw.scancodeKP2
  toNumber ScancodeKP3 = Raw.scancodeKP3
  toNumber ScancodeKP4 = Raw.scancodeKP4
  toNumber ScancodeKP5 = Raw.scancodeKP5
  toNumber ScancodeKP6 = Raw.scancodeKP6
  toNumber ScancodeKP7 = Raw.scancodeKP7
  toNumber ScancodeKP8 = Raw.scancodeKP8
  toNumber ScancodeKP9 = Raw.scancodeKP9
  toNumber ScancodeKP0 = Raw.scancodeKP0
  toNumber ScancodeKPPeriod = Raw.scancodeKPPeriod
  toNumber ScancodeNonUSBackslash = Raw.scancodeNonUSBackslash
  toNumber ScancodeApplication = Raw.scancodeApplication
  toNumber ScancodePower = Raw.scancodePower
  toNumber ScancodeKPEquals = Raw.scancodeKPEquals
  toNumber ScancodeF13 = Raw.scancodeF13
  toNumber ScancodeF14 = Raw.scancodeF14
  toNumber ScancodeF15 = Raw.scancodeF15
  toNumber ScancodeF16 = Raw.scancodeF16
  toNumber ScancodeF17 = Raw.scancodeF17
  toNumber ScancodeF18 = Raw.scancodeF18
  toNumber ScancodeF19 = Raw.scancodeF19
  toNumber ScancodeF20 = Raw.scancodeF20
  toNumber ScancodeF21 = Raw.scancodeF21
  toNumber ScancodeF22 = Raw.scancodeF22
  toNumber ScancodeF23 = Raw.scancodeF23
  toNumber ScancodeF24 = Raw.scancodeF24
  toNumber ScancodeExecute = Raw.scancodeExecute
  toNumber ScancodeHelp = Raw.scancodeHelp
  toNumber ScancodeMenu = Raw.scancodeMenu
  toNumber ScancodeSelect = Raw.scancodeSelect
  toNumber ScancodeStop = Raw.scancodeStop
  toNumber ScancodeAgain = Raw.scancodeAgain
  toNumber ScancodeUndo = Raw.scancodeUndo
  toNumber ScancodeCut = Raw.scancodeCut
  toNumber ScancodeCopy = Raw.scancodeCopy
  toNumber ScancodePaste = Raw.scancodePaste
  toNumber ScancodeFind = Raw.scancodeFind
  toNumber ScancodeMute = Raw.scancodeMute
  toNumber ScancodeVolumeUp = Raw.scancodeVolumeUp
  toNumber ScancodeVolumeDown = Raw.scancodeVolumeDown
  toNumber ScancodeKPComma = Raw.scancodeKPComma
  toNumber ScancodeEqualsAs400 = Raw.scancodeEqualsAs400
  toNumber ScancodeInternational1 = Raw.scancodeInternational1
  toNumber ScancodeInternational2 = Raw.scancodeInternational2
  toNumber ScancodeInternational3 = Raw.scancodeInternational3
  toNumber ScancodeInternational4 = Raw.scancodeInternational4
  toNumber ScancodeInternational5 = Raw.scancodeInternational5
  toNumber ScancodeInternational6 = Raw.scancodeInternational6
  toNumber ScancodeInternational7 = Raw.scancodeInternational7
  toNumber ScancodeInternational8 = Raw.scancodeInternational8
  toNumber ScancodeInternational9 = Raw.scancodeInternational9
  toNumber ScancodeLang1 = Raw.scancodeLang1
  toNumber ScancodeLang2 = Raw.scancodeLang2
  toNumber ScancodeLang3 = Raw.scancodeLang3
  toNumber ScancodeLang4 = Raw.scancodeLang4
  toNumber ScancodeLang5 = Raw.scancodeLang5
  toNumber ScancodeLang6 = Raw.scancodeLang6
  toNumber ScancodeLang7 = Raw.scancodeLang7
  toNumber ScancodeLang8 = Raw.scancodeLang8
  toNumber ScancodeLang9 = Raw.scancodeLang9
  toNumber ScancodeAltErase = Raw.scancodeAltErase
  toNumber ScancodeSysReq = Raw.scancodeSysReq
  toNumber ScancodeCancel = Raw.scancodeCancel
  toNumber ScancodeClear = Raw.scancodeClear
  toNumber ScancodePrior = Raw.scancodePrior
  toNumber ScancodeReturn2 = Raw.scancodeReturn2
  toNumber ScancodeSeparator = Raw.scancodeSeparator
  toNumber ScancodeOut = Raw.scancodeOut
  toNumber ScancodeOper = Raw.scancodeOper
  toNumber ScancodeClearAgain = Raw.scancodeClearAgain
  toNumber ScancodeCrSel = Raw.scancodeCrSel
  toNumber ScancodeExSel = Raw.scancodeExSel
  toNumber ScancodeKP00 = Raw.scancodeKP00
  toNumber ScancodeKP000 = Raw.scancodeKP000
  toNumber ScancodeThousandsSeparator = Raw.scancodeThousandsSeparator
  toNumber ScancodeDecimalSeparator = Raw.scancodeDecimalSeparator
  toNumber ScancodeCurrencyUnit = Raw.scancodeCurrencyUnit
  toNumber ScancodeCurrencySubunit = Raw.scancodeCurrencySubunit
  toNumber ScancodeLeftParen = Raw.scancodeLeftParen
  toNumber ScancodeRightParen = Raw.scancodeRightParen
  toNumber ScancodeLeftBrace = Raw.scancodeLeftBrace
  toNumber ScancodeRightBrace = Raw.scancodeRightBrace
  toNumber ScancodeKPTab = Raw.scancodeKPTab
  toNumber ScancodeKPBackspace = Raw.scancodeKPBackspace
  toNumber ScancodeKPA = Raw.scancodeKPA
  toNumber ScancodeKPB = Raw.scancodeKPB
  toNumber ScancodeKPC = Raw.scancodeKPC
  toNumber ScancodeKPD = Raw.scancodeKPD
  toNumber ScancodeKPE = Raw.scancodeKPE
  toNumber ScancodeKPF = Raw.scancodeKPF
  toNumber ScancodeKPXOR = Raw.scancodeKPXOR
  toNumber ScancodeKPPower = Raw.scancodeKPPower
  toNumber ScancodeKPPercent = Raw.scancodeKPPercent
  toNumber ScancodeKPLess = Raw.scancodeKPLess
  toNumber ScancodeKPGreater = Raw.scancodeKPGreater
  toNumber ScancodeKPAmpersand = Raw.scancodeKPAmpersand
  toNumber ScancodeKPDBLAmpersand = Raw.scancodeKPDBLAmpersand
  toNumber ScancodeKPVerticalBar = Raw.scancodeKPVerticalBar
  toNumber ScancodeKPDBLVerticalBar = Raw.scancodeKPDBLVerticalBar
  toNumber ScancodeKPColon = Raw.scancodeKPColon
  toNumber ScancodeKPHash = Raw.scancodeKPHash
  toNumber ScancodeKPSpace = Raw.scancodeKPSpace
  toNumber ScancodeKPAt = Raw.scancodeKPAt
  toNumber ScancodeKPExclam = Raw.scancodeKPExclam
  toNumber ScancodeKPMemStore = Raw.scancodeKPMemStore
  toNumber ScancodeKPMemRecall = Raw.scancodeKPMemRecall
  toNumber ScancodeKPMemClear = Raw.scancodeKPMemClear
  toNumber ScancodeKPMemAdd = Raw.scancodeKPMemAdd
  toNumber ScancodeKPMemSubtract = Raw.scancodeKPMemSubtract
  toNumber ScancodeKPMemMultiply = Raw.scancodeKPMemMultiply
  toNumber ScancodeKPMemDivide = Raw.scancodeKPMemDivide
  toNumber ScancodeKPPlusMinus = Raw.scancodeKPPlusMinus
  toNumber ScancodeKPClear = Raw.scancodeKPClear
  toNumber ScancodeKPClearEntry = Raw.scancodeKPClearEntry
  toNumber ScancodeKPBinary = Raw.scancodeKPBinary
  toNumber ScancodeKPOctal = Raw.scancodeKPOctal
  toNumber ScancodeKPDecimal = Raw.scancodeKPDecimal
  toNumber ScancodeKPHexadecimal = Raw.scancodeKPHexadecimal
  toNumber ScancodeLCtrl = Raw.scancodeLCtrl
  toNumber ScancodeLShift = Raw.scancodeLShift
  toNumber ScancodeLAlt = Raw.scancodeLAlt
  toNumber ScancodeLGUI = Raw.scancodeLGUI
  toNumber ScancodeRCtrl = Raw.scancodeRCtrl
  toNumber ScancodeRShift = Raw.scancodeRShift
  toNumber ScancodeRAlt = Raw.scancodeRAlt
  toNumber ScancodeRGUI = Raw.scancodeRGUI
  toNumber ScancodeMode = Raw.scancodeMode
  toNumber ScancodeAudioNext = Raw.scancodeAudioNext
  toNumber ScancodeAudioPrev = Raw.scancodeAudioPrev
  toNumber ScancodeAudioStop = Raw.scancodeAudioStop
  toNumber ScancodeAudioPlay = Raw.scancodeAudioPlay
  toNumber ScancodeAudioMute = Raw.scancodeAudioMute
  toNumber ScancodeMediaSelect = Raw.scancodeMediaSelect
  toNumber ScancodeWWW = Raw.scancodeWWW
  toNumber ScancodeMail = Raw.scancodeMail
  toNumber ScancodeCalculator = Raw.scancodeCalculator
  toNumber ScancodeComputer = Raw.scancodeComputer
  toNumber ScancodeACSearch = Raw.scancodeACSearch
  toNumber ScancodeACHome = Raw.scancodeACHome
  toNumber ScancodeACBack = Raw.scancodeACBack
  toNumber ScancodeACForward = Raw.scancodeACForward
  toNumber ScancodeACStop = Raw.scancodeACStop
  toNumber ScancodeACRefresh = Raw.scancodeACRefresh
  toNumber ScancodeACBookmarks = Raw.scancodeACBookmarks
  toNumber ScancodeBrightnessDown = Raw.scancodeBrightnessDown
  toNumber ScancodeBrightnessUp = Raw.scancodeBrightnessUp
  toNumber ScancodeDisplaySwitch = Raw.scancodeDisplaySwitch
  toNumber ScancodeKBDIllumToggle = Raw.scancodeKBDIllumToggle
  toNumber ScancodeKBDIllumDown = Raw.scancodeKBDIllumDown
  toNumber ScancodeKBDIllumUp = Raw.scancodeKBDIllumUp
  toNumber ScancodeEject = Raw.scancodeEject
  toNumber ScancodeSleep = Raw.scancodeSleep
  toNumber ScancodeApp1 = Raw.scancodeApp1
  toNumber ScancodeApp2 = Raw.scancodeApp2
  toNumber ScancodeNum = Raw.scancodeNum
