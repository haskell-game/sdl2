{-# LINE 1 "src/SDL/Raw/Enum.hsc" #-}
module SDL.Raw.Enum (
{-# LINE 2 "src/SDL/Raw/Enum.hsc" #-}
	-- * Enumerations

	-- ** Audio Status
	AudioStatus,
	audioStatusStopped,
	audioStatusPlaying,
	audioStatusPaused,

        -- ** Audio Allowed Changes
        audioAllowFrequencyChange,
        audioAllowFormatChange,
        audioAllowChannelsChange,
        audioAllowAnyChange,

	-- ** Blend Mode
	BlendMode,
	blendModeNone,
	blendModeBlend,
	blendModeAdd,
	blendModeMod,

	-- ** Event Action
	EventAction,
	eventActionAddEvent,
	eventActionPeekEvent,
	eventActionGetEvent,

	-- ** Game Controller Axis
	GameControllerAxis,
	gameControllerAxisInvalid,
	gameControllerAxisLeftX,
	gameControllerAxisLeftY,
	gameControllerAxisRightX,
	gameControllerAxisRightY,
	gameControllerAxisTriggerLeft,
	gameControllerAxisTriggerRight,
	gameControllerAxisMax,

	-- ** Game Controller Button
	GameControllerButton,
	gameControllerButtonInvalid,
	gameControllerButtonA,
	gameControllerButtonB,
	gameControllerButtonX,
	gameControllerButtonY,
	gameControllerButtonBack,
	gameControllerButtonGuide,
	gameControllerButtonStart,
	gameControllerButtonLeftStick,
	gameControllerButtonRightStick,
	gameControllerButtonLeftShoulder,
	gameControllerButtonRightShoulder,
	gameControllerButtonDPadUp,
	gameControllerButtonDPadDown,
	gameControllerButtonDPadLeft,
	gameControllerButtonDPadRight,
	gameControllerButtonMax,

	-- ** OpenGL Attribute
	GLattr,
	glAttrRedSize,
	glAttrGreenSize,
	glAttrBlueSize,
	glAttrAlphaSize,
	glAttrBufferSize,
	glAttrDoubleBuffer,
	glAttrDepthSize,
	glAttrStencilSize,
	glAttrAccumRedSize,
	glAttrAccumGreenSize,
	glAttrAccumBlueSize,
	glAttrAccumAlphaSize,
	glAttrStereo,
	glAttrMultiSampleBuffers,
	glAttrMultiSampleSamples,
	glAttrAcceleratedVisual,
	glAttrRetainedBacking,
	glAttrContextMajorVersion,
	glAttrContextMinorVersion,
	glAttrContextEGL,
	glAttrContextFlags,
	glAttrContextProfileMask,
	glAttrShareWithCurrentContext,
	glAttrFramebufferSRGBCapable,

        -- ** OpenGL Swap Interval
        swapIntervalImmediate,
        swapIntervalVsync,
        swapIntervalLateSwapTearing,

	-- ** Hint Priority
	HintPriority,
	hintPriorityDefault,
	hintPriorityNormal,
	hintPriorityOverride,

	-- ** Key Modifier
	Keymod,
	keymodNone,
	keymodLShift,
	keymodRShift,
	keymodLCtrl,
	keymodRCtrl,
	keymodLAlt,
	keymodRAlt,
	keymodLGUI,
	keymodRGUI,
	keymodNum,
	keymodCaps,
	keymodMode,
	keymodReserved,

	-- ** Log Priority
	LogPriority,
	logPriorityVerbose,
	logPriorityDebug,
	logPriorityInfo,
	logPriorityWarn,
	logPriorityError,
	logPriorityCritical,
	logPriorityPriorities,

	-- ** Power State
	PowerState,
	powerStateUnknown,
	powerStateOnBattery,
	powerStateNoBattery,
	powerStateCharging,
	powerStateCharged,

	-- ** Renderer Flip
	RendererFlip,
	rendererFlipNone,
	rendererFlipHorizontal,
	rendererFlipVertical,

	-- ** Scancode
	Scancode,
	scancodeUnknown,
	scancodeA,
	scancodeB,
	scancodeC,
	scancodeD,
	scancodeE,
	scancodeF,
	scancodeG,
	scancodeH,
	scancodeI,
	scancodeJ,
	scancodeK,
	scancodeL,
	scancodeM,
	scancodeN,
	scancodeO,
	scancodeP,
	scancodeQ,
	scancodeR,
	scancodeS,
	scancodeT,
	scancodeU,
	scancodeV,
	scancodeW,
	scancodeX,
	scancodeY,
	scancodeZ,
	scancode1,
	scancode2,
	scancode3,
	scancode4,
	scancode5,
	scancode6,
	scancode7,
	scancode8,
	scancode9,
	scancode0,
	scancodeReturn,
	scancodeEscape,
	scancodeBackspace,
	scancodeTab,
	scancodeSpace,
	scancodeMinus,
	scancodeEquals,
	scancodeLeftBracket,
	scancodeRightBracket,
	scancodeBackslash,
	scancodeNonUSHash,
	scancodeSemicolon,
	scancodeApostrophe,
	scancodeGrave,
	scancodeComma,
	scancodePeriod,
	scancodeSlash,
	scancodeCapsLock,
	scancodeF1,
	scancodeF2,
	scancodeF3,
	scancodeF4,
	scancodeF5,
	scancodeF6,
	scancodeF7,
	scancodeF8,
	scancodeF9,
	scancodeF10,
	scancodeF11,
	scancodeF12,
	scancodePrintScreen,
	scancodeScrollLock,
	scancodePause,
	scancodeInsert,
	scancodeHome,
	scancodePageUp,
	scancodeDelete,
	scancodeEnd,
	scancodePageDown,
	scancodeRight,
	scancodeLeft,
	scancodeDown,
	scancodeUp,
	scancodeNumLockClear,
	scancodeKPDivide,
	scancodeKPMultiply,
	scancodeKPMinus,
	scancodeKPPlus,
	scancodeKPEnter,
	scancodeKP1,
	scancodeKP2,
	scancodeKP3,
	scancodeKP4,
	scancodeKP5,
	scancodeKP6,
	scancodeKP7,
	scancodeKP8,
	scancodeKP9,
	scancodeKP0,
	scancodeKPPeriod,
	scancodeNonUSBackslash,
	scancodeApplication,
	scancodePower,
	scancodeKPEquals,
	scancodeF13,
	scancodeF14,
	scancodeF15,
	scancodeF16,
	scancodeF17,
	scancodeF18,
	scancodeF19,
	scancodeF20,
	scancodeF21,
	scancodeF22,
	scancodeF23,
	scancodeF24,
	scancodeExecute,
	scancodeHelp,
	scancodeMenu,
	scancodeSelect,
	scancodeStop,
	scancodeAgain,
	scancodeUndo,
	scancodeCut,
	scancodeCopy,
	scancodePaste,
	scancodeFind,
	scancodeMute,
	scancodeVolumeUp,
	scancodeVolumeDown,
	scancodeKPComma,
	scancodeEqualsAs400,
	scancodeInternational1,
	scancodeInternational2,
	scancodeInternational3,
	scancodeInternational4,
	scancodeInternational5,
	scancodeInternational6,
	scancodeInternational7,
	scancodeInternational8,
	scancodeInternational9,
	scancodeLang1,
	scancodeLang2,
	scancodeLang3,
	scancodeLang4,
	scancodeLang5,
	scancodeLang6,
	scancodeLang7,
	scancodeLang8,
	scancodeLang9,
	scancodeAltErase,
	scancodeSysReq,
	scancodeCancel,
	scancodeClear,
	scancodePrior,
	scancodeReturn2,
	scancodeSeparator,
	scancodeOut,
	scancodeOper,
	scancodeClearAgain,
	scancodeCrSel,
	scancodeExSel,
	scancodeKP00,
	scancodeKP000,
	scancodeThousandsSeparator,
	scancodeDecimalSeparator,
	scancodeCurrencyUnit,
	scancodeCurrencySubunit,
	scancodeLeftParen,
	scancodeRightParen,
	scancodeLeftBrace,
	scancodeRightBrace,
	scancodeKPTab,
	scancodeKPBackspace,
	scancodeKPA,
	scancodeKPB,
	scancodeKPC,
	scancodeKPD,
	scancodeKPE,
	scancodeKPF,
	scancodeKPXOR,
	scancodeKPPower,
	scancodeKPPercent,
	scancodeKPLess,
	scancodeKPGreater,
	scancodeKPAmpersand,
	scancodeKPDBLAmpersand,
	scancodeKPVerticalBar,
	scancodeKPDBLVerticalBar,
	scancodeKPColon,
	scancodeKPHash,
	scancodeKPSpace,
	scancodeKPAt,
	scancodeKPExclam,
	scancodeKPMemStore,
	scancodeKPMemRecall,
	scancodeKPMemClear,
	scancodeKPMemAdd,
	scancodeKPMemSubtract,
	scancodeKPMemMultiply,
	scancodeKPMemDivide,
	scancodeKPPlusMinus,
	scancodeKPClear,
	scancodeKPClearEntry,
	scancodeKPBinary,
	scancodeKPOctal,
	scancodeKPDecimal,
	scancodeKPHexadecimal,
	scancodeLCtrl,
	scancodeLShift,
	scancodeLAlt,
	scancodeLGUI,
	scancodeRCtrl,
	scancodeRShift,
	scancodeRAlt,
	scancodeRGUI,
	scancodeMode,
	scancodeAudioNext,
	scancodeAudioPrev,
	scancodeAudioStop,
	scancodeAudioPlay,
	scancodeAudioMute,
	scancodeMediaSelect,
	scancodeWWW,
	scancodeMail,
	scancodeCalculator,
	scancodeComputer,
	scancodeACSearch,
	scancodeACHome,
	scancodeACBack,
	scancodeACForward,
	scancodeACStop,
	scancodeACRefresh,
	scancodeACBookmarks,
	scancodeBrightnessDown,
	scancodeBrightnessUp,
	scancodeDisplaySwitch,
	scancodeKBDIllumToggle,
	scancodeKBDIllumDown,
	scancodeKBDIllumUp,
	scancodeEject,
	scancodeSleep,
	scancodeApp1,
	scancodeApp2,
	scancodeNum,

	-- ** System Cursor
	SystemCursor,
	systemCursorArrow,
	systemCursorIBeam,
	systemCursorWait,
	systemCursorCrosshair,
	systemCursorWaitArrow,
	systemCursorSizeNWSE,
	systemCursorSizeNESW,
	systemCursorSizeWE,
	systemCursorSizeNS,
	systemCursorSizeAll,
	systemCursorNo,
	systemCursorHand,
	systemCursorNum,

	-- ** Thread Priority
	ThreadPriority,
	threadPriorityLow,
	threadPriorityNormal,
	threadPriorityHigh,

	-- * Miscellaneous Enumerations
	-- | These enumerations are not used directly by any SDL function, thus they have a polymorphic type.

	-- ** Button
	buttonLeft,
	buttonMiddle,
	buttonRight,
	buttonX1,
	buttonX2,
	buttonLMask,
	buttonMMask,
	buttonRMask,
	buttonX1Mask,
	buttonX2Mask,

	-- ** Event Type
	eventTypeFirstEvent,
	eventTypeQuit,
	eventTypeAppTerminating,
	eventTypeAppLowMemory,
	eventTypeAppWillEnterBackground,
	eventTypeAppDidEnterBackground,
	eventTypeAppWillEnterForeground,
	eventTypeAppDidEnterForeground,
	eventTypeWindowEvent,
	eventTypeSysWMEvent,
	eventTypeKeyDown,
	eventTypeKeyUp,
	eventTypeTextEditing,
	eventTypeTextInput,
	eventTypeMouseMotion,
	eventTypeMouseButtonDown,
	eventTypeMouseButtonUp,
	eventTypeMouseWheel,
	eventTypeJoyAxisMotion,
	eventTypeJoyBallMotion,
	eventTypeJoyHatMotion,
	eventTypeJoyButtonDown,
	eventTypeJoyButtonUp,
	eventTypeJoyDeviceAdded,
	eventTypeJoyDeviceRemoved,
	eventTypeControllerAxisMotion,
	eventTypeControllerButtonDown,
	eventTypeControllerButtonUp,
	eventTypeControllerDeviceAdded,
	eventTypeControllerDeviceRemoved,
	eventTypeControllerDeviceRemapped,
	eventTypeFingerDown,
	eventTypeFingerUp,
	eventTypeFingerMotion,
	eventTypeDollarGesture,
	eventTypeDollarRecord,
	eventTypeMultiGesture,
	eventTypeClipboardUpdate,
	eventTypeDropFile,
	eventTypeUserEvent,
	eventTypeLastEvent,

	-- ** Initialization Flag
	initFlagTimer,
	initFlagAudio,
	initFlagVideo,
	initFlagJoystick,
	initFlagHaptic,
	initFlagGameController,
	initFlagEvents,
	initFlagNoParachute,
	initFlagEverything,

	-- ** Joystick Hat Position
	joystickHatCentered,
	joystickHatUp,
	joystickHatRight,
	joystickHatDown,
	joystickHatLeft,
	joystickHatRightUp,
	joystickHatRightDown,
	joystickHatLeftUp,
	joystickHatLeftDown,

	-- ** Log Category
	logCategoryApplication,
	logCategoryError,
	logCategoryAssert,
	logCategorySystem,
	logCategoryAudio,
	logCategoryVideo,
	logCategoryRender,
	logCategoryInput,
	logCategoryTest,
	logCategoryCustom,

	-- ** Message Box Flags
	messageBoxFlagError,
	messageBoxFlagWarning,
	messageBoxFlagInformation,

	-- ** Message Box Button Flags
	messageBoxButtonFlagReturnKeyDefault,
	messageBoxButtonFlagEscapeKeyDefault,

	-- ** OpenGL Profile
	glProfileCore,
	glProfileCompatibility,
	glProfileES,

	-- ** OpenGL Context Flag
	glContextFlagDebug,
	glContextFlagForwardCompatible,
	glContextFlagRobustAccess,
	glContextFlagResetIsolation,

	-- ** Pixel Formats
	pixelFormatUnknown,
	pixelFormatIndex1LSB,
	pixelFormatIndex1MSB,
	pixelFormatIndex4LSB,
	pixelFormatIndex4MSB,
	pixelFormatIndex8,
	pixelFormatRGB332,
	pixelFormatRGB444,
	pixelFormatRGB555,
	pixelFormatBGR555,
	pixelFormatARGB4444,
	pixelFormatRGBA4444,
	pixelFormatABGR4444,
	pixelFormatBGRA4444,
	pixelFormatARGB1555,
	pixelFormatRGBA5551,
	pixelFormatABGR1555,
	pixelFormatBGRA5551,
	pixelFormatRGB565,
	pixelFormatBGR565,
	pixelFormatRGB24,
	pixelFormatBGR24,
	pixelFormatRGB888,
	pixelFormatRGBX8888,
	pixelFormatBGR888,
	pixelFormatBGRX8888,
	pixelFormatARGB8888,
	pixelFormatRGBA8888,
	pixelFormatABGR8888,
	pixelFormatBGRA8888,
	pixelFormatARGB2101010,
	pixelFormatYV12,
	pixelFormatIYUV,
	pixelFormatYUY2,
	pixelFormatUYVY,
	pixelFormatYVYU,

	-- ** Renderer Flags
	rendererFlagSoftware,
	rendererFlagAccelerated,
	rendererFlagPresentVSync,
	rendererFlagTargetTexture,

	-- ** Texture Access
	textureAccessStatic,
	textureAccessStreaming,
	textureAccessTarget,

	-- ** Texture Modulate
	textureModulateNone,
	textureModulateColor,
	textureModulateAlpha,

	-- ** Window Event
	windowEventNone,
	windowEventShown,
	windowEventHidden,
	windowEventExposed,
	windowEventMoved,
	windowEventResized,
	windowEventSizeChanged,
	windowEventMinimized,
	windowEventMaximized,
	windowEventRestored,
	windowEventEnter,
	windowEventLeave,
	windowEventFocusGained,
	windowEventFocusLost,
	windowEventClose,

	-- ** Window Flags
	windowFlagFullscreen,
	windowFlagOpenGL,
	windowFlagShown,
	windowFlagHidden,
	windowFlagBorderless,
	windowFlagResizable,
	windowFlagMinimized,
	windowFlagMaximized,
	windowFlagInputGrabbed,
	windowFlagInputFocus,
	windowFlagMouseFocus,
	windowFlagFullscreenDesktop,
	windowFlagForeign,
	windowFlagAllowHighDPI,

	-- ** Window Positioning
	windowPosUndefined,
	windowPosCentered
) where


{-# LINE 610 "src/SDL/Raw/Enum.hsc" #-}

import Data.Int
import Data.Word

type AudioStatus = (Word32)
{-# LINE 615 "src/SDL/Raw/Enum.hsc" #-}

audioStatusStopped :: AudioStatus
audioStatusPlaying :: AudioStatus
audioStatusPaused :: AudioStatus

audioStatusStopped = (0)
{-# LINE 621 "src/SDL/Raw/Enum.hsc" #-}
audioStatusPlaying = (1)
{-# LINE 622 "src/SDL/Raw/Enum.hsc" #-}
audioStatusPaused = (2)
{-# LINE 623 "src/SDL/Raw/Enum.hsc" #-}

audioAllowFrequencyChange :: Num a => a
audioAllowFormatChange :: Num a => a
audioAllowChannelsChange :: Num a => a
audioAllowAnyChange :: Num a => a

audioAllowFrequencyChange = (1)
{-# LINE 630 "src/SDL/Raw/Enum.hsc" #-}
audioAllowFormatChange = (2)
{-# LINE 631 "src/SDL/Raw/Enum.hsc" #-}
audioAllowChannelsChange = (4)
{-# LINE 632 "src/SDL/Raw/Enum.hsc" #-}
audioAllowAnyChange = (7)
{-# LINE 633 "src/SDL/Raw/Enum.hsc" #-}

type BlendMode = (Word32)
{-# LINE 635 "src/SDL/Raw/Enum.hsc" #-}

blendModeNone :: BlendMode
blendModeBlend :: BlendMode
blendModeAdd :: BlendMode
blendModeMod :: BlendMode

blendModeNone = (0)
{-# LINE 642 "src/SDL/Raw/Enum.hsc" #-}
blendModeBlend = (1)
{-# LINE 643 "src/SDL/Raw/Enum.hsc" #-}
blendModeAdd = (2)
{-# LINE 644 "src/SDL/Raw/Enum.hsc" #-}
blendModeMod = (4)
{-# LINE 645 "src/SDL/Raw/Enum.hsc" #-}

type EventAction = (Word32)
{-# LINE 647 "src/SDL/Raw/Enum.hsc" #-}

eventActionAddEvent :: EventAction
eventActionPeekEvent :: EventAction
eventActionGetEvent :: EventAction

eventActionAddEvent = (0)
{-# LINE 653 "src/SDL/Raw/Enum.hsc" #-}
eventActionPeekEvent = (1)
{-# LINE 654 "src/SDL/Raw/Enum.hsc" #-}
eventActionGetEvent = (2)
{-# LINE 655 "src/SDL/Raw/Enum.hsc" #-}

type GameControllerAxis = (Int32)
{-# LINE 657 "src/SDL/Raw/Enum.hsc" #-}

gameControllerAxisInvalid :: GameControllerAxis
gameControllerAxisLeftX :: GameControllerAxis
gameControllerAxisLeftY :: GameControllerAxis
gameControllerAxisRightX :: GameControllerAxis
gameControllerAxisRightY :: GameControllerAxis
gameControllerAxisTriggerLeft :: GameControllerAxis
gameControllerAxisTriggerRight :: GameControllerAxis
gameControllerAxisMax :: GameControllerAxis

gameControllerAxisInvalid = (-1)
{-# LINE 668 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisLeftX = (0)
{-# LINE 669 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisLeftY = (1)
{-# LINE 670 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisRightX = (2)
{-# LINE 671 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisRightY = (3)
{-# LINE 672 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisTriggerLeft = (4)
{-# LINE 673 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisTriggerRight = (5)
{-# LINE 674 "src/SDL/Raw/Enum.hsc" #-}
gameControllerAxisMax = (6)
{-# LINE 675 "src/SDL/Raw/Enum.hsc" #-}

type GameControllerButton = (Int32)
{-# LINE 677 "src/SDL/Raw/Enum.hsc" #-}

gameControllerButtonInvalid :: GameControllerButton
gameControllerButtonA :: GameControllerButton
gameControllerButtonB :: GameControllerButton
gameControllerButtonX :: GameControllerButton
gameControllerButtonY :: GameControllerButton
gameControllerButtonBack :: GameControllerButton
gameControllerButtonGuide :: GameControllerButton
gameControllerButtonStart :: GameControllerButton
gameControllerButtonLeftStick :: GameControllerButton
gameControllerButtonRightStick :: GameControllerButton
gameControllerButtonLeftShoulder :: GameControllerButton
gameControllerButtonRightShoulder :: GameControllerButton
gameControllerButtonDPadUp :: GameControllerButton
gameControllerButtonDPadDown :: GameControllerButton
gameControllerButtonDPadLeft :: GameControllerButton
gameControllerButtonDPadRight :: GameControllerButton
gameControllerButtonMax :: GameControllerButton

gameControllerButtonInvalid = (-1)
{-# LINE 697 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonA = (0)
{-# LINE 698 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonB = (1)
{-# LINE 699 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonX = (2)
{-# LINE 700 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonY = (3)
{-# LINE 701 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonBack = (4)
{-# LINE 702 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonGuide = (5)
{-# LINE 703 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonStart = (6)
{-# LINE 704 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonLeftStick = (7)
{-# LINE 705 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonRightStick = (8)
{-# LINE 706 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonLeftShoulder = (9)
{-# LINE 707 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonRightShoulder = (10)
{-# LINE 708 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonDPadUp = (11)
{-# LINE 709 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonDPadDown = (12)
{-# LINE 710 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonDPadLeft = (13)
{-# LINE 711 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonDPadRight = (14)
{-# LINE 712 "src/SDL/Raw/Enum.hsc" #-}
gameControllerButtonMax = (15)
{-# LINE 713 "src/SDL/Raw/Enum.hsc" #-}

type GLattr = (Word32)
{-# LINE 715 "src/SDL/Raw/Enum.hsc" #-}

glAttrRedSize :: GLattr
glAttrGreenSize :: GLattr
glAttrBlueSize :: GLattr
glAttrAlphaSize :: GLattr
glAttrBufferSize :: GLattr
glAttrDoubleBuffer :: GLattr
glAttrDepthSize :: GLattr
glAttrStencilSize :: GLattr
glAttrAccumRedSize :: GLattr
glAttrAccumGreenSize :: GLattr
glAttrAccumBlueSize :: GLattr
glAttrAccumAlphaSize :: GLattr
glAttrStereo :: GLattr
glAttrMultiSampleBuffers :: GLattr
glAttrMultiSampleSamples :: GLattr
glAttrAcceleratedVisual :: GLattr
glAttrRetainedBacking :: GLattr
glAttrContextMajorVersion :: GLattr
glAttrContextMinorVersion :: GLattr
glAttrContextEGL :: GLattr
glAttrContextFlags :: GLattr
glAttrContextProfileMask :: GLattr
glAttrShareWithCurrentContext :: GLattr
glAttrFramebufferSRGBCapable :: GLattr

glAttrRedSize = (0)
{-# LINE 742 "src/SDL/Raw/Enum.hsc" #-}
glAttrGreenSize = (1)
{-# LINE 743 "src/SDL/Raw/Enum.hsc" #-}
glAttrBlueSize = (2)
{-# LINE 744 "src/SDL/Raw/Enum.hsc" #-}
glAttrAlphaSize = (3)
{-# LINE 745 "src/SDL/Raw/Enum.hsc" #-}
glAttrBufferSize = (4)
{-# LINE 746 "src/SDL/Raw/Enum.hsc" #-}
glAttrDoubleBuffer = (5)
{-# LINE 747 "src/SDL/Raw/Enum.hsc" #-}
glAttrDepthSize = (6)
{-# LINE 748 "src/SDL/Raw/Enum.hsc" #-}
glAttrStencilSize = (7)
{-# LINE 749 "src/SDL/Raw/Enum.hsc" #-}
glAttrAccumRedSize = (8)
{-# LINE 750 "src/SDL/Raw/Enum.hsc" #-}
glAttrAccumGreenSize = (9)
{-# LINE 751 "src/SDL/Raw/Enum.hsc" #-}
glAttrAccumBlueSize = (10)
{-# LINE 752 "src/SDL/Raw/Enum.hsc" #-}
glAttrAccumAlphaSize = (11)
{-# LINE 753 "src/SDL/Raw/Enum.hsc" #-}
glAttrStereo = (12)
{-# LINE 754 "src/SDL/Raw/Enum.hsc" #-}
glAttrMultiSampleBuffers = (13)
{-# LINE 755 "src/SDL/Raw/Enum.hsc" #-}
glAttrMultiSampleSamples = (14)
{-# LINE 756 "src/SDL/Raw/Enum.hsc" #-}
glAttrAcceleratedVisual = (15)
{-# LINE 757 "src/SDL/Raw/Enum.hsc" #-}
glAttrRetainedBacking = (16)
{-# LINE 758 "src/SDL/Raw/Enum.hsc" #-}
glAttrContextMajorVersion = (17)
{-# LINE 759 "src/SDL/Raw/Enum.hsc" #-}
glAttrContextMinorVersion = (18)
{-# LINE 760 "src/SDL/Raw/Enum.hsc" #-}
glAttrContextEGL = (19)
{-# LINE 761 "src/SDL/Raw/Enum.hsc" #-}
glAttrContextFlags = (20)
{-# LINE 762 "src/SDL/Raw/Enum.hsc" #-}
glAttrContextProfileMask = (21)
{-# LINE 763 "src/SDL/Raw/Enum.hsc" #-}
glAttrShareWithCurrentContext = (22)
{-# LINE 764 "src/SDL/Raw/Enum.hsc" #-}
glAttrFramebufferSRGBCapable = (23)
{-# LINE 765 "src/SDL/Raw/Enum.hsc" #-}

swapIntervalImmediate :: Num a => a
swapIntervalVsync :: Num a => a
swapIntervalLateSwapTearing :: Num a => a

swapIntervalImmediate = 0
swapIntervalVsync = 1
swapIntervalLateSwapTearing = -1

type HintPriority = (Word32)
{-# LINE 775 "src/SDL/Raw/Enum.hsc" #-}

hintPriorityDefault :: HintPriority
hintPriorityNormal :: HintPriority
hintPriorityOverride :: HintPriority

hintPriorityDefault = (0)
{-# LINE 781 "src/SDL/Raw/Enum.hsc" #-}
hintPriorityNormal = (1)
{-# LINE 782 "src/SDL/Raw/Enum.hsc" #-}
hintPriorityOverride = (2)
{-# LINE 783 "src/SDL/Raw/Enum.hsc" #-}

type Keymod = (Word32)
{-# LINE 785 "src/SDL/Raw/Enum.hsc" #-}

keymodNone :: Keymod
keymodLShift :: Keymod
keymodRShift :: Keymod
keymodLCtrl :: Keymod
keymodRCtrl :: Keymod
keymodLAlt :: Keymod
keymodRAlt :: Keymod
keymodLGUI :: Keymod
keymodRGUI :: Keymod
keymodNum :: Keymod
keymodCaps :: Keymod
keymodMode :: Keymod
keymodReserved :: Keymod

keymodNone = (0)
{-# LINE 801 "src/SDL/Raw/Enum.hsc" #-}
keymodLShift = (1)
{-# LINE 802 "src/SDL/Raw/Enum.hsc" #-}
keymodRShift = (2)
{-# LINE 803 "src/SDL/Raw/Enum.hsc" #-}
keymodLCtrl = (64)
{-# LINE 804 "src/SDL/Raw/Enum.hsc" #-}
keymodRCtrl = (128)
{-# LINE 805 "src/SDL/Raw/Enum.hsc" #-}
keymodLAlt = (256)
{-# LINE 806 "src/SDL/Raw/Enum.hsc" #-}
keymodRAlt = (512)
{-# LINE 807 "src/SDL/Raw/Enum.hsc" #-}
keymodLGUI = (1024)
{-# LINE 808 "src/SDL/Raw/Enum.hsc" #-}
keymodRGUI = (2048)
{-# LINE 809 "src/SDL/Raw/Enum.hsc" #-}
keymodNum = (4096)
{-# LINE 810 "src/SDL/Raw/Enum.hsc" #-}
keymodCaps = (8192)
{-# LINE 811 "src/SDL/Raw/Enum.hsc" #-}
keymodMode = (16384)
{-# LINE 812 "src/SDL/Raw/Enum.hsc" #-}
keymodReserved = (32768)
{-# LINE 813 "src/SDL/Raw/Enum.hsc" #-}

type LogPriority = (Word32)
{-# LINE 815 "src/SDL/Raw/Enum.hsc" #-}

logPriorityVerbose :: LogPriority
logPriorityDebug :: LogPriority
logPriorityInfo :: LogPriority
logPriorityWarn :: LogPriority
logPriorityError :: LogPriority
logPriorityCritical :: LogPriority
logPriorityPriorities :: LogPriority

logPriorityVerbose = (1)
{-# LINE 825 "src/SDL/Raw/Enum.hsc" #-}
logPriorityDebug = (2)
{-# LINE 826 "src/SDL/Raw/Enum.hsc" #-}
logPriorityInfo = (3)
{-# LINE 827 "src/SDL/Raw/Enum.hsc" #-}
logPriorityWarn = (4)
{-# LINE 828 "src/SDL/Raw/Enum.hsc" #-}
logPriorityError = (5)
{-# LINE 829 "src/SDL/Raw/Enum.hsc" #-}
logPriorityCritical = (6)
{-# LINE 830 "src/SDL/Raw/Enum.hsc" #-}
logPriorityPriorities = (7)
{-# LINE 831 "src/SDL/Raw/Enum.hsc" #-}

type PowerState = (Word32)
{-# LINE 833 "src/SDL/Raw/Enum.hsc" #-}

powerStateUnknown :: PowerState
powerStateOnBattery :: PowerState
powerStateNoBattery :: PowerState
powerStateCharging :: PowerState
powerStateCharged :: PowerState

powerStateUnknown = (0)
{-# LINE 841 "src/SDL/Raw/Enum.hsc" #-}
powerStateOnBattery = (1)
{-# LINE 842 "src/SDL/Raw/Enum.hsc" #-}
powerStateNoBattery = (2)
{-# LINE 843 "src/SDL/Raw/Enum.hsc" #-}
powerStateCharging = (3)
{-# LINE 844 "src/SDL/Raw/Enum.hsc" #-}
powerStateCharged = (4)
{-# LINE 845 "src/SDL/Raw/Enum.hsc" #-}

type RendererFlip = (Word32)
{-# LINE 847 "src/SDL/Raw/Enum.hsc" #-}

rendererFlipNone :: RendererFlip
rendererFlipHorizontal :: RendererFlip
rendererFlipVertical :: RendererFlip

rendererFlipNone = (0)
{-# LINE 853 "src/SDL/Raw/Enum.hsc" #-}
rendererFlipHorizontal = (1)
{-# LINE 854 "src/SDL/Raw/Enum.hsc" #-}
rendererFlipVertical = (2)
{-# LINE 855 "src/SDL/Raw/Enum.hsc" #-}

type Scancode = (Word32)
{-# LINE 857 "src/SDL/Raw/Enum.hsc" #-}

scancodeUnknown :: Scancode
scancodeA :: Scancode
scancodeB :: Scancode
scancodeC :: Scancode
scancodeD :: Scancode
scancodeE :: Scancode
scancodeF :: Scancode
scancodeG :: Scancode
scancodeH :: Scancode
scancodeI :: Scancode
scancodeJ :: Scancode
scancodeK :: Scancode
scancodeL :: Scancode
scancodeM :: Scancode
scancodeN :: Scancode
scancodeO :: Scancode
scancodeP :: Scancode
scancodeQ :: Scancode
scancodeR :: Scancode
scancodeS :: Scancode
scancodeT :: Scancode
scancodeU :: Scancode
scancodeV :: Scancode
scancodeW :: Scancode
scancodeX :: Scancode
scancodeY :: Scancode
scancodeZ :: Scancode
scancode1 :: Scancode
scancode2 :: Scancode
scancode3 :: Scancode
scancode4 :: Scancode
scancode5 :: Scancode
scancode6 :: Scancode
scancode7 :: Scancode
scancode8 :: Scancode
scancode9 :: Scancode
scancode0 :: Scancode
scancodeReturn :: Scancode
scancodeEscape :: Scancode
scancodeBackspace :: Scancode
scancodeTab :: Scancode
scancodeSpace :: Scancode
scancodeMinus :: Scancode
scancodeEquals :: Scancode
scancodeLeftBracket :: Scancode
scancodeRightBracket :: Scancode
scancodeBackslash :: Scancode
scancodeNonUSHash :: Scancode
scancodeSemicolon :: Scancode
scancodeApostrophe :: Scancode
scancodeGrave :: Scancode
scancodeComma :: Scancode
scancodePeriod :: Scancode
scancodeSlash :: Scancode
scancodeCapsLock :: Scancode
scancodeF1 :: Scancode
scancodeF2 :: Scancode
scancodeF3 :: Scancode
scancodeF4 :: Scancode
scancodeF5 :: Scancode
scancodeF6 :: Scancode
scancodeF7 :: Scancode
scancodeF8 :: Scancode
scancodeF9 :: Scancode
scancodeF10 :: Scancode
scancodeF11 :: Scancode
scancodeF12 :: Scancode
scancodePrintScreen :: Scancode
scancodeScrollLock :: Scancode
scancodePause :: Scancode
scancodeInsert :: Scancode
scancodeHome :: Scancode
scancodePageUp :: Scancode
scancodeDelete :: Scancode
scancodeEnd :: Scancode
scancodePageDown :: Scancode
scancodeRight :: Scancode
scancodeLeft :: Scancode
scancodeDown :: Scancode
scancodeUp :: Scancode
scancodeNumLockClear :: Scancode
scancodeKPDivide :: Scancode
scancodeKPMultiply :: Scancode
scancodeKPMinus :: Scancode
scancodeKPPlus :: Scancode
scancodeKPEnter :: Scancode
scancodeKP1 :: Scancode
scancodeKP2 :: Scancode
scancodeKP3 :: Scancode
scancodeKP4 :: Scancode
scancodeKP5 :: Scancode
scancodeKP6 :: Scancode
scancodeKP7 :: Scancode
scancodeKP8 :: Scancode
scancodeKP9 :: Scancode
scancodeKP0 :: Scancode
scancodeKPPeriod :: Scancode
scancodeNonUSBackslash :: Scancode
scancodeApplication :: Scancode
scancodePower :: Scancode
scancodeKPEquals :: Scancode
scancodeF13 :: Scancode
scancodeF14 :: Scancode
scancodeF15 :: Scancode
scancodeF16 :: Scancode
scancodeF17 :: Scancode
scancodeF18 :: Scancode
scancodeF19 :: Scancode
scancodeF20 :: Scancode
scancodeF21 :: Scancode
scancodeF22 :: Scancode
scancodeF23 :: Scancode
scancodeF24 :: Scancode
scancodeExecute :: Scancode
scancodeHelp :: Scancode
scancodeMenu :: Scancode
scancodeSelect :: Scancode
scancodeStop :: Scancode
scancodeAgain :: Scancode
scancodeUndo :: Scancode
scancodeCut :: Scancode
scancodeCopy :: Scancode
scancodePaste :: Scancode
scancodeFind :: Scancode
scancodeMute :: Scancode
scancodeVolumeUp :: Scancode
scancodeVolumeDown :: Scancode
scancodeKPComma :: Scancode
scancodeEqualsAs400 :: Scancode
scancodeInternational1 :: Scancode
scancodeInternational2 :: Scancode
scancodeInternational3 :: Scancode
scancodeInternational4 :: Scancode
scancodeInternational5 :: Scancode
scancodeInternational6 :: Scancode
scancodeInternational7 :: Scancode
scancodeInternational8 :: Scancode
scancodeInternational9 :: Scancode
scancodeLang1 :: Scancode
scancodeLang2 :: Scancode
scancodeLang3 :: Scancode
scancodeLang4 :: Scancode
scancodeLang5 :: Scancode
scancodeLang6 :: Scancode
scancodeLang7 :: Scancode
scancodeLang8 :: Scancode
scancodeLang9 :: Scancode
scancodeAltErase :: Scancode
scancodeSysReq :: Scancode
scancodeCancel :: Scancode
scancodeClear :: Scancode
scancodePrior :: Scancode
scancodeReturn2 :: Scancode
scancodeSeparator :: Scancode
scancodeOut :: Scancode
scancodeOper :: Scancode
scancodeClearAgain :: Scancode
scancodeCrSel :: Scancode
scancodeExSel :: Scancode
scancodeKP00 :: Scancode
scancodeKP000 :: Scancode
scancodeThousandsSeparator :: Scancode
scancodeDecimalSeparator :: Scancode
scancodeCurrencyUnit :: Scancode
scancodeCurrencySubunit :: Scancode
scancodeLeftParen :: Scancode
scancodeRightParen :: Scancode
scancodeLeftBrace :: Scancode
scancodeRightBrace :: Scancode
scancodeKPTab :: Scancode
scancodeKPBackspace :: Scancode
scancodeKPA :: Scancode
scancodeKPB :: Scancode
scancodeKPC :: Scancode
scancodeKPD :: Scancode
scancodeKPE :: Scancode
scancodeKPF :: Scancode
scancodeKPXOR :: Scancode
scancodeKPPower :: Scancode
scancodeKPPercent :: Scancode
scancodeKPLess :: Scancode
scancodeKPGreater :: Scancode
scancodeKPAmpersand :: Scancode
scancodeKPDBLAmpersand :: Scancode
scancodeKPVerticalBar :: Scancode
scancodeKPDBLVerticalBar :: Scancode
scancodeKPColon :: Scancode
scancodeKPHash :: Scancode
scancodeKPSpace :: Scancode
scancodeKPAt :: Scancode
scancodeKPExclam :: Scancode
scancodeKPMemStore :: Scancode
scancodeKPMemRecall :: Scancode
scancodeKPMemClear :: Scancode
scancodeKPMemAdd :: Scancode
scancodeKPMemSubtract :: Scancode
scancodeKPMemMultiply :: Scancode
scancodeKPMemDivide :: Scancode
scancodeKPPlusMinus :: Scancode
scancodeKPClear :: Scancode
scancodeKPClearEntry :: Scancode
scancodeKPBinary :: Scancode
scancodeKPOctal :: Scancode
scancodeKPDecimal :: Scancode
scancodeKPHexadecimal :: Scancode
scancodeLCtrl :: Scancode
scancodeLShift :: Scancode
scancodeLAlt :: Scancode
scancodeLGUI :: Scancode
scancodeRCtrl :: Scancode
scancodeRShift :: Scancode
scancodeRAlt :: Scancode
scancodeRGUI :: Scancode
scancodeMode :: Scancode
scancodeAudioNext :: Scancode
scancodeAudioPrev :: Scancode
scancodeAudioStop :: Scancode
scancodeAudioPlay :: Scancode
scancodeAudioMute :: Scancode
scancodeMediaSelect :: Scancode
scancodeWWW :: Scancode
scancodeMail :: Scancode
scancodeCalculator :: Scancode
scancodeComputer :: Scancode
scancodeACSearch :: Scancode
scancodeACHome :: Scancode
scancodeACBack :: Scancode
scancodeACForward :: Scancode
scancodeACStop :: Scancode
scancodeACRefresh :: Scancode
scancodeACBookmarks :: Scancode
scancodeBrightnessDown :: Scancode
scancodeBrightnessUp :: Scancode
scancodeDisplaySwitch :: Scancode
scancodeKBDIllumToggle :: Scancode
scancodeKBDIllumDown :: Scancode
scancodeKBDIllumUp :: Scancode
scancodeEject :: Scancode
scancodeSleep :: Scancode
scancodeApp1 :: Scancode
scancodeApp2 :: Scancode
scancodeNum :: Scancode

scancodeUnknown = (0)
{-# LINE 1102 "src/SDL/Raw/Enum.hsc" #-}
scancodeA = (4)
{-# LINE 1103 "src/SDL/Raw/Enum.hsc" #-}
scancodeB = (5)
{-# LINE 1104 "src/SDL/Raw/Enum.hsc" #-}
scancodeC = (6)
{-# LINE 1105 "src/SDL/Raw/Enum.hsc" #-}
scancodeD = (7)
{-# LINE 1106 "src/SDL/Raw/Enum.hsc" #-}
scancodeE = (8)
{-# LINE 1107 "src/SDL/Raw/Enum.hsc" #-}
scancodeF = (9)
{-# LINE 1108 "src/SDL/Raw/Enum.hsc" #-}
scancodeG = (10)
{-# LINE 1109 "src/SDL/Raw/Enum.hsc" #-}
scancodeH = (11)
{-# LINE 1110 "src/SDL/Raw/Enum.hsc" #-}
scancodeI = (12)
{-# LINE 1111 "src/SDL/Raw/Enum.hsc" #-}
scancodeJ = (13)
{-# LINE 1112 "src/SDL/Raw/Enum.hsc" #-}
scancodeK = (14)
{-# LINE 1113 "src/SDL/Raw/Enum.hsc" #-}
scancodeL = (15)
{-# LINE 1114 "src/SDL/Raw/Enum.hsc" #-}
scancodeM = (16)
{-# LINE 1115 "src/SDL/Raw/Enum.hsc" #-}
scancodeN = (17)
{-# LINE 1116 "src/SDL/Raw/Enum.hsc" #-}
scancodeO = (18)
{-# LINE 1117 "src/SDL/Raw/Enum.hsc" #-}
scancodeP = (19)
{-# LINE 1118 "src/SDL/Raw/Enum.hsc" #-}
scancodeQ = (20)
{-# LINE 1119 "src/SDL/Raw/Enum.hsc" #-}
scancodeR = (21)
{-# LINE 1120 "src/SDL/Raw/Enum.hsc" #-}
scancodeS = (22)
{-# LINE 1121 "src/SDL/Raw/Enum.hsc" #-}
scancodeT = (23)
{-# LINE 1122 "src/SDL/Raw/Enum.hsc" #-}
scancodeU = (24)
{-# LINE 1123 "src/SDL/Raw/Enum.hsc" #-}
scancodeV = (25)
{-# LINE 1124 "src/SDL/Raw/Enum.hsc" #-}
scancodeW = (26)
{-# LINE 1125 "src/SDL/Raw/Enum.hsc" #-}
scancodeX = (27)
{-# LINE 1126 "src/SDL/Raw/Enum.hsc" #-}
scancodeY = (28)
{-# LINE 1127 "src/SDL/Raw/Enum.hsc" #-}
scancodeZ = (29)
{-# LINE 1128 "src/SDL/Raw/Enum.hsc" #-}
scancode1 = (30)
{-# LINE 1129 "src/SDL/Raw/Enum.hsc" #-}
scancode2 = (31)
{-# LINE 1130 "src/SDL/Raw/Enum.hsc" #-}
scancode3 = (32)
{-# LINE 1131 "src/SDL/Raw/Enum.hsc" #-}
scancode4 = (33)
{-# LINE 1132 "src/SDL/Raw/Enum.hsc" #-}
scancode5 = (34)
{-# LINE 1133 "src/SDL/Raw/Enum.hsc" #-}
scancode6 = (35)
{-# LINE 1134 "src/SDL/Raw/Enum.hsc" #-}
scancode7 = (36)
{-# LINE 1135 "src/SDL/Raw/Enum.hsc" #-}
scancode8 = (37)
{-# LINE 1136 "src/SDL/Raw/Enum.hsc" #-}
scancode9 = (38)
{-# LINE 1137 "src/SDL/Raw/Enum.hsc" #-}
scancode0 = (39)
{-# LINE 1138 "src/SDL/Raw/Enum.hsc" #-}
scancodeReturn = (40)
{-# LINE 1139 "src/SDL/Raw/Enum.hsc" #-}
scancodeEscape = (41)
{-# LINE 1140 "src/SDL/Raw/Enum.hsc" #-}
scancodeBackspace = (42)
{-# LINE 1141 "src/SDL/Raw/Enum.hsc" #-}
scancodeTab = (43)
{-# LINE 1142 "src/SDL/Raw/Enum.hsc" #-}
scancodeSpace = (44)
{-# LINE 1143 "src/SDL/Raw/Enum.hsc" #-}
scancodeMinus = (45)
{-# LINE 1144 "src/SDL/Raw/Enum.hsc" #-}
scancodeEquals = (46)
{-# LINE 1145 "src/SDL/Raw/Enum.hsc" #-}
scancodeLeftBracket = (47)
{-# LINE 1146 "src/SDL/Raw/Enum.hsc" #-}
scancodeRightBracket = (48)
{-# LINE 1147 "src/SDL/Raw/Enum.hsc" #-}
scancodeBackslash = (49)
{-# LINE 1148 "src/SDL/Raw/Enum.hsc" #-}
scancodeNonUSHash = (50)
{-# LINE 1149 "src/SDL/Raw/Enum.hsc" #-}
scancodeSemicolon = (51)
{-# LINE 1150 "src/SDL/Raw/Enum.hsc" #-}
scancodeApostrophe = (52)
{-# LINE 1151 "src/SDL/Raw/Enum.hsc" #-}
scancodeGrave = (53)
{-# LINE 1152 "src/SDL/Raw/Enum.hsc" #-}
scancodeComma = (54)
{-# LINE 1153 "src/SDL/Raw/Enum.hsc" #-}
scancodePeriod = (55)
{-# LINE 1154 "src/SDL/Raw/Enum.hsc" #-}
scancodeSlash = (56)
{-# LINE 1155 "src/SDL/Raw/Enum.hsc" #-}
scancodeCapsLock = (57)
{-# LINE 1156 "src/SDL/Raw/Enum.hsc" #-}
scancodeF1 = (58)
{-# LINE 1157 "src/SDL/Raw/Enum.hsc" #-}
scancodeF2 = (59)
{-# LINE 1158 "src/SDL/Raw/Enum.hsc" #-}
scancodeF3 = (60)
{-# LINE 1159 "src/SDL/Raw/Enum.hsc" #-}
scancodeF4 = (61)
{-# LINE 1160 "src/SDL/Raw/Enum.hsc" #-}
scancodeF5 = (62)
{-# LINE 1161 "src/SDL/Raw/Enum.hsc" #-}
scancodeF6 = (63)
{-# LINE 1162 "src/SDL/Raw/Enum.hsc" #-}
scancodeF7 = (64)
{-# LINE 1163 "src/SDL/Raw/Enum.hsc" #-}
scancodeF8 = (65)
{-# LINE 1164 "src/SDL/Raw/Enum.hsc" #-}
scancodeF9 = (66)
{-# LINE 1165 "src/SDL/Raw/Enum.hsc" #-}
scancodeF10 = (67)
{-# LINE 1166 "src/SDL/Raw/Enum.hsc" #-}
scancodeF11 = (68)
{-# LINE 1167 "src/SDL/Raw/Enum.hsc" #-}
scancodeF12 = (69)
{-# LINE 1168 "src/SDL/Raw/Enum.hsc" #-}
scancodePrintScreen = (70)
{-# LINE 1169 "src/SDL/Raw/Enum.hsc" #-}
scancodeScrollLock = (71)
{-# LINE 1170 "src/SDL/Raw/Enum.hsc" #-}
scancodePause = (72)
{-# LINE 1171 "src/SDL/Raw/Enum.hsc" #-}
scancodeInsert = (73)
{-# LINE 1172 "src/SDL/Raw/Enum.hsc" #-}
scancodeHome = (74)
{-# LINE 1173 "src/SDL/Raw/Enum.hsc" #-}
scancodePageUp = (75)
{-# LINE 1174 "src/SDL/Raw/Enum.hsc" #-}
scancodeDelete = (76)
{-# LINE 1175 "src/SDL/Raw/Enum.hsc" #-}
scancodeEnd = (77)
{-# LINE 1176 "src/SDL/Raw/Enum.hsc" #-}
scancodePageDown = (78)
{-# LINE 1177 "src/SDL/Raw/Enum.hsc" #-}
scancodeRight = (79)
{-# LINE 1178 "src/SDL/Raw/Enum.hsc" #-}
scancodeLeft = (80)
{-# LINE 1179 "src/SDL/Raw/Enum.hsc" #-}
scancodeDown = (81)
{-# LINE 1180 "src/SDL/Raw/Enum.hsc" #-}
scancodeUp = (82)
{-# LINE 1181 "src/SDL/Raw/Enum.hsc" #-}
scancodeNumLockClear = (83)
{-# LINE 1182 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPDivide = (84)
{-# LINE 1183 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMultiply = (85)
{-# LINE 1184 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMinus = (86)
{-# LINE 1185 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPPlus = (87)
{-# LINE 1186 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPEnter = (88)
{-# LINE 1187 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP1 = (89)
{-# LINE 1188 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP2 = (90)
{-# LINE 1189 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP3 = (91)
{-# LINE 1190 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP4 = (92)
{-# LINE 1191 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP5 = (93)
{-# LINE 1192 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP6 = (94)
{-# LINE 1193 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP7 = (95)
{-# LINE 1194 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP8 = (96)
{-# LINE 1195 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP9 = (97)
{-# LINE 1196 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP0 = (98)
{-# LINE 1197 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPPeriod = (99)
{-# LINE 1198 "src/SDL/Raw/Enum.hsc" #-}
scancodeNonUSBackslash = (100)
{-# LINE 1199 "src/SDL/Raw/Enum.hsc" #-}
scancodeApplication = (101)
{-# LINE 1200 "src/SDL/Raw/Enum.hsc" #-}
scancodePower = (102)
{-# LINE 1201 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPEquals = (103)
{-# LINE 1202 "src/SDL/Raw/Enum.hsc" #-}
scancodeF13 = (104)
{-# LINE 1203 "src/SDL/Raw/Enum.hsc" #-}
scancodeF14 = (105)
{-# LINE 1204 "src/SDL/Raw/Enum.hsc" #-}
scancodeF15 = (106)
{-# LINE 1205 "src/SDL/Raw/Enum.hsc" #-}
scancodeF16 = (107)
{-# LINE 1206 "src/SDL/Raw/Enum.hsc" #-}
scancodeF17 = (108)
{-# LINE 1207 "src/SDL/Raw/Enum.hsc" #-}
scancodeF18 = (109)
{-# LINE 1208 "src/SDL/Raw/Enum.hsc" #-}
scancodeF19 = (110)
{-# LINE 1209 "src/SDL/Raw/Enum.hsc" #-}
scancodeF20 = (111)
{-# LINE 1210 "src/SDL/Raw/Enum.hsc" #-}
scancodeF21 = (112)
{-# LINE 1211 "src/SDL/Raw/Enum.hsc" #-}
scancodeF22 = (113)
{-# LINE 1212 "src/SDL/Raw/Enum.hsc" #-}
scancodeF23 = (114)
{-# LINE 1213 "src/SDL/Raw/Enum.hsc" #-}
scancodeF24 = (115)
{-# LINE 1214 "src/SDL/Raw/Enum.hsc" #-}
scancodeExecute = (116)
{-# LINE 1215 "src/SDL/Raw/Enum.hsc" #-}
scancodeHelp = (117)
{-# LINE 1216 "src/SDL/Raw/Enum.hsc" #-}
scancodeMenu = (118)
{-# LINE 1217 "src/SDL/Raw/Enum.hsc" #-}
scancodeSelect = (119)
{-# LINE 1218 "src/SDL/Raw/Enum.hsc" #-}
scancodeStop = (120)
{-# LINE 1219 "src/SDL/Raw/Enum.hsc" #-}
scancodeAgain = (121)
{-# LINE 1220 "src/SDL/Raw/Enum.hsc" #-}
scancodeUndo = (122)
{-# LINE 1221 "src/SDL/Raw/Enum.hsc" #-}
scancodeCut = (123)
{-# LINE 1222 "src/SDL/Raw/Enum.hsc" #-}
scancodeCopy = (124)
{-# LINE 1223 "src/SDL/Raw/Enum.hsc" #-}
scancodePaste = (125)
{-# LINE 1224 "src/SDL/Raw/Enum.hsc" #-}
scancodeFind = (126)
{-# LINE 1225 "src/SDL/Raw/Enum.hsc" #-}
scancodeMute = (127)
{-# LINE 1226 "src/SDL/Raw/Enum.hsc" #-}
scancodeVolumeUp = (128)
{-# LINE 1227 "src/SDL/Raw/Enum.hsc" #-}
scancodeVolumeDown = (129)
{-# LINE 1228 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPComma = (133)
{-# LINE 1229 "src/SDL/Raw/Enum.hsc" #-}
scancodeEqualsAs400 = (134)
{-# LINE 1230 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational1 = (135)
{-# LINE 1231 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational2 = (136)
{-# LINE 1232 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational3 = (137)
{-# LINE 1233 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational4 = (138)
{-# LINE 1234 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational5 = (139)
{-# LINE 1235 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational6 = (140)
{-# LINE 1236 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational7 = (141)
{-# LINE 1237 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational8 = (142)
{-# LINE 1238 "src/SDL/Raw/Enum.hsc" #-}
scancodeInternational9 = (143)
{-# LINE 1239 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang1 = (144)
{-# LINE 1240 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang2 = (145)
{-# LINE 1241 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang3 = (146)
{-# LINE 1242 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang4 = (147)
{-# LINE 1243 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang5 = (148)
{-# LINE 1244 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang6 = (149)
{-# LINE 1245 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang7 = (150)
{-# LINE 1246 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang8 = (151)
{-# LINE 1247 "src/SDL/Raw/Enum.hsc" #-}
scancodeLang9 = (152)
{-# LINE 1248 "src/SDL/Raw/Enum.hsc" #-}
scancodeAltErase = (153)
{-# LINE 1249 "src/SDL/Raw/Enum.hsc" #-}
scancodeSysReq = (154)
{-# LINE 1250 "src/SDL/Raw/Enum.hsc" #-}
scancodeCancel = (155)
{-# LINE 1251 "src/SDL/Raw/Enum.hsc" #-}
scancodeClear = (156)
{-# LINE 1252 "src/SDL/Raw/Enum.hsc" #-}
scancodePrior = (157)
{-# LINE 1253 "src/SDL/Raw/Enum.hsc" #-}
scancodeReturn2 = (158)
{-# LINE 1254 "src/SDL/Raw/Enum.hsc" #-}
scancodeSeparator = (159)
{-# LINE 1255 "src/SDL/Raw/Enum.hsc" #-}
scancodeOut = (160)
{-# LINE 1256 "src/SDL/Raw/Enum.hsc" #-}
scancodeOper = (161)
{-# LINE 1257 "src/SDL/Raw/Enum.hsc" #-}
scancodeClearAgain = (162)
{-# LINE 1258 "src/SDL/Raw/Enum.hsc" #-}
scancodeCrSel = (163)
{-# LINE 1259 "src/SDL/Raw/Enum.hsc" #-}
scancodeExSel = (164)
{-# LINE 1260 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP00 = (176)
{-# LINE 1261 "src/SDL/Raw/Enum.hsc" #-}
scancodeKP000 = (177)
{-# LINE 1262 "src/SDL/Raw/Enum.hsc" #-}
scancodeThousandsSeparator = (178)
{-# LINE 1263 "src/SDL/Raw/Enum.hsc" #-}
scancodeDecimalSeparator = (179)
{-# LINE 1264 "src/SDL/Raw/Enum.hsc" #-}
scancodeCurrencyUnit = (180)
{-# LINE 1265 "src/SDL/Raw/Enum.hsc" #-}
scancodeCurrencySubunit = (181)
{-# LINE 1266 "src/SDL/Raw/Enum.hsc" #-}
scancodeLeftParen = (182)
{-# LINE 1267 "src/SDL/Raw/Enum.hsc" #-}
scancodeRightParen = (183)
{-# LINE 1268 "src/SDL/Raw/Enum.hsc" #-}
scancodeLeftBrace = (184)
{-# LINE 1269 "src/SDL/Raw/Enum.hsc" #-}
scancodeRightBrace = (185)
{-# LINE 1270 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPTab = (186)
{-# LINE 1271 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPBackspace = (187)
{-# LINE 1272 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPA = (188)
{-# LINE 1273 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPB = (189)
{-# LINE 1274 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPC = (190)
{-# LINE 1275 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPD = (191)
{-# LINE 1276 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPE = (192)
{-# LINE 1277 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPF = (193)
{-# LINE 1278 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPXOR = (194)
{-# LINE 1279 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPPower = (195)
{-# LINE 1280 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPPercent = (196)
{-# LINE 1281 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPLess = (197)
{-# LINE 1282 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPGreater = (198)
{-# LINE 1283 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPAmpersand = (199)
{-# LINE 1284 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPDBLAmpersand = (200)
{-# LINE 1285 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPVerticalBar = (201)
{-# LINE 1286 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPDBLVerticalBar = (202)
{-# LINE 1287 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPColon = (203)
{-# LINE 1288 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPHash = (204)
{-# LINE 1289 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPSpace = (205)
{-# LINE 1290 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPAt = (206)
{-# LINE 1291 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPExclam = (207)
{-# LINE 1292 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemStore = (208)
{-# LINE 1293 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemRecall = (209)
{-# LINE 1294 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemClear = (210)
{-# LINE 1295 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemAdd = (211)
{-# LINE 1296 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemSubtract = (212)
{-# LINE 1297 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemMultiply = (213)
{-# LINE 1298 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPMemDivide = (214)
{-# LINE 1299 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPPlusMinus = (215)
{-# LINE 1300 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPClear = (216)
{-# LINE 1301 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPClearEntry = (217)
{-# LINE 1302 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPBinary = (218)
{-# LINE 1303 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPOctal = (219)
{-# LINE 1304 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPDecimal = (220)
{-# LINE 1305 "src/SDL/Raw/Enum.hsc" #-}
scancodeKPHexadecimal = (221)
{-# LINE 1306 "src/SDL/Raw/Enum.hsc" #-}
scancodeLCtrl = (224)
{-# LINE 1307 "src/SDL/Raw/Enum.hsc" #-}
scancodeLShift = (225)
{-# LINE 1308 "src/SDL/Raw/Enum.hsc" #-}
scancodeLAlt = (226)
{-# LINE 1309 "src/SDL/Raw/Enum.hsc" #-}
scancodeLGUI = (227)
{-# LINE 1310 "src/SDL/Raw/Enum.hsc" #-}
scancodeRCtrl = (228)
{-# LINE 1311 "src/SDL/Raw/Enum.hsc" #-}
scancodeRShift = (229)
{-# LINE 1312 "src/SDL/Raw/Enum.hsc" #-}
scancodeRAlt = (230)
{-# LINE 1313 "src/SDL/Raw/Enum.hsc" #-}
scancodeRGUI = (231)
{-# LINE 1314 "src/SDL/Raw/Enum.hsc" #-}
scancodeMode = (257)
{-# LINE 1315 "src/SDL/Raw/Enum.hsc" #-}
scancodeAudioNext = (258)
{-# LINE 1316 "src/SDL/Raw/Enum.hsc" #-}
scancodeAudioPrev = (259)
{-# LINE 1317 "src/SDL/Raw/Enum.hsc" #-}
scancodeAudioStop = (260)
{-# LINE 1318 "src/SDL/Raw/Enum.hsc" #-}
scancodeAudioPlay = (261)
{-# LINE 1319 "src/SDL/Raw/Enum.hsc" #-}
scancodeAudioMute = (262)
{-# LINE 1320 "src/SDL/Raw/Enum.hsc" #-}
scancodeMediaSelect = (263)
{-# LINE 1321 "src/SDL/Raw/Enum.hsc" #-}
scancodeWWW = (264)
{-# LINE 1322 "src/SDL/Raw/Enum.hsc" #-}
scancodeMail = (265)
{-# LINE 1323 "src/SDL/Raw/Enum.hsc" #-}
scancodeCalculator = (266)
{-# LINE 1324 "src/SDL/Raw/Enum.hsc" #-}
scancodeComputer = (267)
{-# LINE 1325 "src/SDL/Raw/Enum.hsc" #-}
scancodeACSearch = (268)
{-# LINE 1326 "src/SDL/Raw/Enum.hsc" #-}
scancodeACHome = (269)
{-# LINE 1327 "src/SDL/Raw/Enum.hsc" #-}
scancodeACBack = (270)
{-# LINE 1328 "src/SDL/Raw/Enum.hsc" #-}
scancodeACForward = (271)
{-# LINE 1329 "src/SDL/Raw/Enum.hsc" #-}
scancodeACStop = (272)
{-# LINE 1330 "src/SDL/Raw/Enum.hsc" #-}
scancodeACRefresh = (273)
{-# LINE 1331 "src/SDL/Raw/Enum.hsc" #-}
scancodeACBookmarks = (274)
{-# LINE 1332 "src/SDL/Raw/Enum.hsc" #-}
scancodeBrightnessDown = (275)
{-# LINE 1333 "src/SDL/Raw/Enum.hsc" #-}
scancodeBrightnessUp = (276)
{-# LINE 1334 "src/SDL/Raw/Enum.hsc" #-}
scancodeDisplaySwitch = (277)
{-# LINE 1335 "src/SDL/Raw/Enum.hsc" #-}
scancodeKBDIllumToggle = (278)
{-# LINE 1336 "src/SDL/Raw/Enum.hsc" #-}
scancodeKBDIllumDown = (279)
{-# LINE 1337 "src/SDL/Raw/Enum.hsc" #-}
scancodeKBDIllumUp = (280)
{-# LINE 1338 "src/SDL/Raw/Enum.hsc" #-}
scancodeEject = (281)
{-# LINE 1339 "src/SDL/Raw/Enum.hsc" #-}
scancodeSleep = (282)
{-# LINE 1340 "src/SDL/Raw/Enum.hsc" #-}
scancodeApp1 = (283)
{-# LINE 1341 "src/SDL/Raw/Enum.hsc" #-}
scancodeApp2 = (284)
{-# LINE 1342 "src/SDL/Raw/Enum.hsc" #-}
scancodeNum = (512)
{-# LINE 1343 "src/SDL/Raw/Enum.hsc" #-}

type SystemCursor = (Word32)
{-# LINE 1345 "src/SDL/Raw/Enum.hsc" #-}

systemCursorArrow :: SystemCursor
systemCursorIBeam :: SystemCursor
systemCursorWait :: SystemCursor
systemCursorCrosshair :: SystemCursor
systemCursorWaitArrow :: SystemCursor
systemCursorSizeNWSE :: SystemCursor
systemCursorSizeNESW :: SystemCursor
systemCursorSizeWE :: SystemCursor
systemCursorSizeNS :: SystemCursor
systemCursorSizeAll :: SystemCursor
systemCursorNo :: SystemCursor
systemCursorHand :: SystemCursor
systemCursorNum :: SystemCursor

systemCursorArrow = (0)
{-# LINE 1361 "src/SDL/Raw/Enum.hsc" #-}
systemCursorIBeam = (1)
{-# LINE 1362 "src/SDL/Raw/Enum.hsc" #-}
systemCursorWait = (2)
{-# LINE 1363 "src/SDL/Raw/Enum.hsc" #-}
systemCursorCrosshair = (3)
{-# LINE 1364 "src/SDL/Raw/Enum.hsc" #-}
systemCursorWaitArrow = (4)
{-# LINE 1365 "src/SDL/Raw/Enum.hsc" #-}
systemCursorSizeNWSE = (5)
{-# LINE 1366 "src/SDL/Raw/Enum.hsc" #-}
systemCursorSizeNESW = (6)
{-# LINE 1367 "src/SDL/Raw/Enum.hsc" #-}
systemCursorSizeWE = (7)
{-# LINE 1368 "src/SDL/Raw/Enum.hsc" #-}
systemCursorSizeNS = (8)
{-# LINE 1369 "src/SDL/Raw/Enum.hsc" #-}
systemCursorSizeAll = (9)
{-# LINE 1370 "src/SDL/Raw/Enum.hsc" #-}
systemCursorNo = (10)
{-# LINE 1371 "src/SDL/Raw/Enum.hsc" #-}
systemCursorHand = (11)
{-# LINE 1372 "src/SDL/Raw/Enum.hsc" #-}
systemCursorNum = (12)
{-# LINE 1373 "src/SDL/Raw/Enum.hsc" #-}

type ThreadPriority = (Word32)
{-# LINE 1375 "src/SDL/Raw/Enum.hsc" #-}

threadPriorityLow :: ThreadPriority
threadPriorityNormal :: ThreadPriority
threadPriorityHigh :: ThreadPriority

threadPriorityLow = (0)
{-# LINE 1381 "src/SDL/Raw/Enum.hsc" #-}
threadPriorityNormal = (1)
{-# LINE 1382 "src/SDL/Raw/Enum.hsc" #-}
threadPriorityHigh = (2)
{-# LINE 1383 "src/SDL/Raw/Enum.hsc" #-}

buttonLeft :: (Num a) => a
buttonMiddle :: (Num a) => a
buttonRight :: (Num a) => a
buttonX1 :: (Num a) => a
buttonX2 :: (Num a) => a
buttonLMask :: (Num a) => a
buttonMMask :: (Num a) => a
buttonRMask :: (Num a) => a
buttonX1Mask :: (Num a) => a
buttonX2Mask :: (Num a) => a

buttonLeft = (1)
{-# LINE 1396 "src/SDL/Raw/Enum.hsc" #-}
buttonMiddle = (2)
{-# LINE 1397 "src/SDL/Raw/Enum.hsc" #-}
buttonRight = (3)
{-# LINE 1398 "src/SDL/Raw/Enum.hsc" #-}
buttonX1 = (4)
{-# LINE 1399 "src/SDL/Raw/Enum.hsc" #-}
buttonX2 = (5)
{-# LINE 1400 "src/SDL/Raw/Enum.hsc" #-}
buttonLMask = (1)
{-# LINE 1401 "src/SDL/Raw/Enum.hsc" #-}
buttonMMask = (2)
{-# LINE 1402 "src/SDL/Raw/Enum.hsc" #-}
buttonRMask = (4)
{-# LINE 1403 "src/SDL/Raw/Enum.hsc" #-}
buttonX1Mask = (8)
{-# LINE 1404 "src/SDL/Raw/Enum.hsc" #-}
buttonX2Mask = (16)
{-# LINE 1405 "src/SDL/Raw/Enum.hsc" #-}

eventTypeFirstEvent :: (Num a) => a
eventTypeQuit :: (Num a) => a
eventTypeAppTerminating :: (Num a) => a
eventTypeAppLowMemory :: (Num a) => a
eventTypeAppWillEnterBackground :: (Num a) => a
eventTypeAppDidEnterBackground :: (Num a) => a
eventTypeAppWillEnterForeground :: (Num a) => a
eventTypeAppDidEnterForeground :: (Num a) => a
eventTypeWindowEvent :: (Num a) => a
eventTypeSysWMEvent :: (Num a) => a
eventTypeKeyDown :: (Num a) => a
eventTypeKeyUp :: (Num a) => a
eventTypeTextEditing :: (Num a) => a
eventTypeTextInput :: (Num a) => a
eventTypeMouseMotion :: (Num a) => a
eventTypeMouseButtonDown :: (Num a) => a
eventTypeMouseButtonUp :: (Num a) => a
eventTypeMouseWheel :: (Num a) => a
eventTypeJoyAxisMotion :: (Num a) => a
eventTypeJoyBallMotion :: (Num a) => a
eventTypeJoyHatMotion :: (Num a) => a
eventTypeJoyButtonDown :: (Num a) => a
eventTypeJoyButtonUp :: (Num a) => a
eventTypeJoyDeviceAdded :: (Num a) => a
eventTypeJoyDeviceRemoved :: (Num a) => a
eventTypeControllerAxisMotion :: (Num a) => a
eventTypeControllerButtonDown :: (Num a) => a
eventTypeControllerButtonUp :: (Num a) => a
eventTypeControllerDeviceAdded :: (Num a) => a
eventTypeControllerDeviceRemoved :: (Num a) => a
eventTypeControllerDeviceRemapped :: (Num a) => a
eventTypeFingerDown :: (Num a) => a
eventTypeFingerUp :: (Num a) => a
eventTypeFingerMotion :: (Num a) => a
eventTypeDollarGesture :: (Num a) => a
eventTypeDollarRecord :: (Num a) => a
eventTypeMultiGesture :: (Num a) => a
eventTypeClipboardUpdate :: (Num a) => a
eventTypeDropFile :: (Num a) => a
eventTypeUserEvent :: (Num a) => a
eventTypeLastEvent :: (Num a) => a

eventTypeFirstEvent = (0)
{-# LINE 1449 "src/SDL/Raw/Enum.hsc" #-}
eventTypeQuit = (256)
{-# LINE 1450 "src/SDL/Raw/Enum.hsc" #-}
eventTypeAppTerminating = (257)
{-# LINE 1451 "src/SDL/Raw/Enum.hsc" #-}
eventTypeAppLowMemory = (258)
{-# LINE 1452 "src/SDL/Raw/Enum.hsc" #-}
eventTypeAppWillEnterBackground = (259)
{-# LINE 1453 "src/SDL/Raw/Enum.hsc" #-}
eventTypeAppDidEnterBackground = (260)
{-# LINE 1454 "src/SDL/Raw/Enum.hsc" #-}
eventTypeAppWillEnterForeground = (261)
{-# LINE 1455 "src/SDL/Raw/Enum.hsc" #-}
eventTypeAppDidEnterForeground = (262)
{-# LINE 1456 "src/SDL/Raw/Enum.hsc" #-}
eventTypeWindowEvent = (512)
{-# LINE 1457 "src/SDL/Raw/Enum.hsc" #-}
eventTypeSysWMEvent = (513)
{-# LINE 1458 "src/SDL/Raw/Enum.hsc" #-}
eventTypeKeyDown = (768)
{-# LINE 1459 "src/SDL/Raw/Enum.hsc" #-}
eventTypeKeyUp = (769)
{-# LINE 1460 "src/SDL/Raw/Enum.hsc" #-}
eventTypeTextEditing = (770)
{-# LINE 1461 "src/SDL/Raw/Enum.hsc" #-}
eventTypeTextInput = (771)
{-# LINE 1462 "src/SDL/Raw/Enum.hsc" #-}
eventTypeMouseMotion = (1024)
{-# LINE 1463 "src/SDL/Raw/Enum.hsc" #-}
eventTypeMouseButtonDown = (1025)
{-# LINE 1464 "src/SDL/Raw/Enum.hsc" #-}
eventTypeMouseButtonUp = (1026)
{-# LINE 1465 "src/SDL/Raw/Enum.hsc" #-}
eventTypeMouseWheel = (1027)
{-# LINE 1466 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyAxisMotion = (1536)
{-# LINE 1467 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyBallMotion = (1537)
{-# LINE 1468 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyHatMotion = (1538)
{-# LINE 1469 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyButtonDown = (1539)
{-# LINE 1470 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyButtonUp = (1540)
{-# LINE 1471 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyDeviceAdded = (1541)
{-# LINE 1472 "src/SDL/Raw/Enum.hsc" #-}
eventTypeJoyDeviceRemoved = (1542)
{-# LINE 1473 "src/SDL/Raw/Enum.hsc" #-}
eventTypeControllerAxisMotion = (1616)
{-# LINE 1474 "src/SDL/Raw/Enum.hsc" #-}
eventTypeControllerButtonDown = (1617)
{-# LINE 1475 "src/SDL/Raw/Enum.hsc" #-}
eventTypeControllerButtonUp = (1618)
{-# LINE 1476 "src/SDL/Raw/Enum.hsc" #-}
eventTypeControllerDeviceAdded = (1619)
{-# LINE 1477 "src/SDL/Raw/Enum.hsc" #-}
eventTypeControllerDeviceRemoved = (1620)
{-# LINE 1478 "src/SDL/Raw/Enum.hsc" #-}
eventTypeControllerDeviceRemapped = (1621)
{-# LINE 1479 "src/SDL/Raw/Enum.hsc" #-}
eventTypeFingerDown = (1792)
{-# LINE 1480 "src/SDL/Raw/Enum.hsc" #-}
eventTypeFingerUp = (1793)
{-# LINE 1481 "src/SDL/Raw/Enum.hsc" #-}
eventTypeFingerMotion = (1794)
{-# LINE 1482 "src/SDL/Raw/Enum.hsc" #-}
eventTypeDollarGesture = (2048)
{-# LINE 1483 "src/SDL/Raw/Enum.hsc" #-}
eventTypeDollarRecord = (2049)
{-# LINE 1484 "src/SDL/Raw/Enum.hsc" #-}
eventTypeMultiGesture = (2050)
{-# LINE 1485 "src/SDL/Raw/Enum.hsc" #-}
eventTypeClipboardUpdate = (2304)
{-# LINE 1486 "src/SDL/Raw/Enum.hsc" #-}
eventTypeDropFile = (4096)
{-# LINE 1487 "src/SDL/Raw/Enum.hsc" #-}
eventTypeUserEvent = (32768)
{-# LINE 1488 "src/SDL/Raw/Enum.hsc" #-}
eventTypeLastEvent = (65535)
{-# LINE 1489 "src/SDL/Raw/Enum.hsc" #-}

initFlagTimer :: (Num a) => a
initFlagAudio :: (Num a) => a
initFlagVideo :: (Num a) => a
initFlagJoystick :: (Num a) => a
initFlagHaptic :: (Num a) => a
initFlagGameController :: (Num a) => a
initFlagEvents :: (Num a) => a
initFlagNoParachute :: (Num a) => a
initFlagEverything :: (Num a) => a

initFlagTimer = (1)
{-# LINE 1501 "src/SDL/Raw/Enum.hsc" #-}
initFlagAudio = (16)
{-# LINE 1502 "src/SDL/Raw/Enum.hsc" #-}
initFlagVideo = (32)
{-# LINE 1503 "src/SDL/Raw/Enum.hsc" #-}
initFlagJoystick = (512)
{-# LINE 1504 "src/SDL/Raw/Enum.hsc" #-}
initFlagHaptic = (4096)
{-# LINE 1505 "src/SDL/Raw/Enum.hsc" #-}
initFlagGameController = (8192)
{-# LINE 1506 "src/SDL/Raw/Enum.hsc" #-}
initFlagEvents = (16384)
{-# LINE 1507 "src/SDL/Raw/Enum.hsc" #-}
initFlagNoParachute = (1048576)
{-# LINE 1508 "src/SDL/Raw/Enum.hsc" #-}
initFlagEverything = (29233)
{-# LINE 1509 "src/SDL/Raw/Enum.hsc" #-}

joystickHatCentered :: (Num a) => a
joystickHatUp :: (Num a) => a
joystickHatRight :: (Num a) => a
joystickHatDown :: (Num a) => a
joystickHatLeft :: (Num a) => a
joystickHatRightUp :: (Num a) => a
joystickHatRightDown :: (Num a) => a
joystickHatLeftUp :: (Num a) => a
joystickHatLeftDown :: (Num a) => a

joystickHatCentered = (0)
{-# LINE 1521 "src/SDL/Raw/Enum.hsc" #-}
joystickHatUp = (1)
{-# LINE 1522 "src/SDL/Raw/Enum.hsc" #-}
joystickHatRight = (2)
{-# LINE 1523 "src/SDL/Raw/Enum.hsc" #-}
joystickHatDown = (4)
{-# LINE 1524 "src/SDL/Raw/Enum.hsc" #-}
joystickHatLeft = (8)
{-# LINE 1525 "src/SDL/Raw/Enum.hsc" #-}
joystickHatRightUp = (3)
{-# LINE 1526 "src/SDL/Raw/Enum.hsc" #-}
joystickHatRightDown = (6)
{-# LINE 1527 "src/SDL/Raw/Enum.hsc" #-}
joystickHatLeftUp = (9)
{-# LINE 1528 "src/SDL/Raw/Enum.hsc" #-}
joystickHatLeftDown = (12)
{-# LINE 1529 "src/SDL/Raw/Enum.hsc" #-}

logCategoryApplication :: (Num a) => a
logCategoryError :: (Num a) => a
logCategoryAssert :: (Num a) => a
logCategorySystem :: (Num a) => a
logCategoryAudio :: (Num a) => a
logCategoryVideo :: (Num a) => a
logCategoryRender :: (Num a) => a
logCategoryInput :: (Num a) => a
logCategoryTest :: (Num a) => a
logCategoryCustom :: (Num a) => a

logCategoryApplication = (0)
{-# LINE 1542 "src/SDL/Raw/Enum.hsc" #-}
logCategoryError = (1)
{-# LINE 1543 "src/SDL/Raw/Enum.hsc" #-}
logCategoryAssert = (2)
{-# LINE 1544 "src/SDL/Raw/Enum.hsc" #-}
logCategorySystem = (3)
{-# LINE 1545 "src/SDL/Raw/Enum.hsc" #-}
logCategoryAudio = (4)
{-# LINE 1546 "src/SDL/Raw/Enum.hsc" #-}
logCategoryVideo = (5)
{-# LINE 1547 "src/SDL/Raw/Enum.hsc" #-}
logCategoryRender = (6)
{-# LINE 1548 "src/SDL/Raw/Enum.hsc" #-}
logCategoryInput = (7)
{-# LINE 1549 "src/SDL/Raw/Enum.hsc" #-}
logCategoryTest = (8)
{-# LINE 1550 "src/SDL/Raw/Enum.hsc" #-}
logCategoryCustom = (19)
{-# LINE 1551 "src/SDL/Raw/Enum.hsc" #-}

messageBoxFlagError :: (Num a) => a
messageBoxFlagWarning :: (Num a) => a
messageBoxFlagInformation :: (Num a) => a

messageBoxFlagError = (16)
{-# LINE 1557 "src/SDL/Raw/Enum.hsc" #-}
messageBoxFlagWarning = (32)
{-# LINE 1558 "src/SDL/Raw/Enum.hsc" #-}
messageBoxFlagInformation = (64)
{-# LINE 1559 "src/SDL/Raw/Enum.hsc" #-}

messageBoxButtonFlagReturnKeyDefault :: (Num a) => a
messageBoxButtonFlagEscapeKeyDefault :: (Num a) => a

messageBoxButtonFlagReturnKeyDefault = (1)
{-# LINE 1564 "src/SDL/Raw/Enum.hsc" #-}
messageBoxButtonFlagEscapeKeyDefault = (2)
{-# LINE 1565 "src/SDL/Raw/Enum.hsc" #-}

glProfileCore :: (Num a) => a
glProfileCompatibility :: (Num a) => a
glProfileES :: (Num a) => a

glProfileCore = (1)
{-# LINE 1571 "src/SDL/Raw/Enum.hsc" #-}
glProfileCompatibility = (2)
{-# LINE 1572 "src/SDL/Raw/Enum.hsc" #-}
glProfileES = (4)
{-# LINE 1573 "src/SDL/Raw/Enum.hsc" #-}

glContextFlagDebug :: (Num a) => a
glContextFlagForwardCompatible :: (Num a) => a
glContextFlagRobustAccess :: (Num a) => a
glContextFlagResetIsolation :: (Num a) => a

glContextFlagDebug = (1)
{-# LINE 1580 "src/SDL/Raw/Enum.hsc" #-}
glContextFlagForwardCompatible = (2)
{-# LINE 1581 "src/SDL/Raw/Enum.hsc" #-}
glContextFlagRobustAccess = (4)
{-# LINE 1582 "src/SDL/Raw/Enum.hsc" #-}
glContextFlagResetIsolation = (8)
{-# LINE 1583 "src/SDL/Raw/Enum.hsc" #-}

pixelFormatUnknown :: (Num a) => a
pixelFormatIndex1LSB :: (Num a) => a
pixelFormatIndex1MSB :: (Num a) => a
pixelFormatIndex4LSB :: (Num a) => a
pixelFormatIndex4MSB :: (Num a) => a
pixelFormatIndex8 :: (Num a) => a
pixelFormatRGB332 :: (Num a) => a
pixelFormatRGB444 :: (Num a) => a
pixelFormatRGB555 :: (Num a) => a
pixelFormatBGR555 :: (Num a) => a
pixelFormatARGB4444 :: (Num a) => a
pixelFormatRGBA4444 :: (Num a) => a
pixelFormatABGR4444 :: (Num a) => a
pixelFormatBGRA4444 :: (Num a) => a
pixelFormatARGB1555 :: (Num a) => a
pixelFormatRGBA5551 :: (Num a) => a
pixelFormatABGR1555 :: (Num a) => a
pixelFormatBGRA5551 :: (Num a) => a
pixelFormatRGB565 :: (Num a) => a
pixelFormatBGR565 :: (Num a) => a
pixelFormatRGB24 :: (Num a) => a
pixelFormatBGR24 :: (Num a) => a
pixelFormatRGB888 :: (Num a) => a
pixelFormatRGBX8888 :: (Num a) => a
pixelFormatBGR888 :: (Num a) => a
pixelFormatBGRX8888 :: (Num a) => a
pixelFormatARGB8888 :: (Num a) => a
pixelFormatRGBA8888 :: (Num a) => a
pixelFormatABGR8888 :: (Num a) => a
pixelFormatBGRA8888 :: (Num a) => a
pixelFormatARGB2101010 :: (Num a) => a
pixelFormatYV12 :: (Num a) => a
pixelFormatIYUV :: (Num a) => a
pixelFormatYUY2 :: (Num a) => a
pixelFormatUYVY :: (Num a) => a
pixelFormatYVYU :: (Num a) => a

pixelFormatUnknown = (0)
{-# LINE 1622 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatIndex1LSB = (286261504)
{-# LINE 1623 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatIndex1MSB = (287310080)
{-# LINE 1624 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatIndex4LSB = (303039488)
{-# LINE 1625 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatIndex4MSB = (304088064)
{-# LINE 1626 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatIndex8 = (318769153)
{-# LINE 1627 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGB332 = (336660481)
{-# LINE 1628 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGB444 = (353504258)
{-# LINE 1629 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGB555 = (353570562)
{-# LINE 1630 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGR555 = (357764866)
{-# LINE 1631 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatARGB4444 = (355602434)
{-# LINE 1632 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGBA4444 = (356651010)
{-# LINE 1633 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatABGR4444 = (359796738)
{-# LINE 1634 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGRA4444 = (360845314)
{-# LINE 1635 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatARGB1555 = (355667970)
{-# LINE 1636 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGBA5551 = (356782082)
{-# LINE 1637 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatABGR1555 = (359862274)
{-# LINE 1638 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGRA5551 = (360976386)
{-# LINE 1639 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGB565 = (353701890)
{-# LINE 1640 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGR565 = (357896194)
{-# LINE 1641 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGB24 = (386930691)
{-# LINE 1642 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGR24 = (390076419)
{-# LINE 1643 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGB888 = (370546692)
{-# LINE 1644 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGBX8888 = (371595268)
{-# LINE 1645 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGR888 = (374740996)
{-# LINE 1646 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGRX8888 = (375789572)
{-# LINE 1647 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatARGB8888 = (372645892)
{-# LINE 1648 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatRGBA8888 = (373694468)
{-# LINE 1649 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatABGR8888 = (376840196)
{-# LINE 1650 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatBGRA8888 = (377888772)
{-# LINE 1651 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatARGB2101010 = (372711428)
{-# LINE 1652 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatYV12 = (842094169)
{-# LINE 1653 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatIYUV = (1448433993)
{-# LINE 1654 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatYUY2 = (844715353)
{-# LINE 1655 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatUYVY = (1498831189)
{-# LINE 1656 "src/SDL/Raw/Enum.hsc" #-}
pixelFormatYVYU = (1431918169)
{-# LINE 1657 "src/SDL/Raw/Enum.hsc" #-}

rendererFlagSoftware :: (Num a) => a
rendererFlagAccelerated :: (Num a) => a
rendererFlagPresentVSync :: (Num a) => a
rendererFlagTargetTexture :: (Num a) => a

rendererFlagSoftware = (1)
{-# LINE 1664 "src/SDL/Raw/Enum.hsc" #-}
rendererFlagAccelerated = (2)
{-# LINE 1665 "src/SDL/Raw/Enum.hsc" #-}
rendererFlagPresentVSync = (4)
{-# LINE 1666 "src/SDL/Raw/Enum.hsc" #-}
rendererFlagTargetTexture = (8)
{-# LINE 1667 "src/SDL/Raw/Enum.hsc" #-}

textureAccessStatic :: (Num a) => a
textureAccessStreaming :: (Num a) => a
textureAccessTarget :: (Num a) => a

textureAccessStatic = (0)
{-# LINE 1673 "src/SDL/Raw/Enum.hsc" #-}
textureAccessStreaming = (1)
{-# LINE 1674 "src/SDL/Raw/Enum.hsc" #-}
textureAccessTarget = (2)
{-# LINE 1675 "src/SDL/Raw/Enum.hsc" #-}

textureModulateNone :: (Num a) => a
textureModulateColor :: (Num a) => a
textureModulateAlpha :: (Num a) => a

textureModulateNone = (0)
{-# LINE 1681 "src/SDL/Raw/Enum.hsc" #-}
textureModulateColor = (1)
{-# LINE 1682 "src/SDL/Raw/Enum.hsc" #-}
textureModulateAlpha = (2)
{-# LINE 1683 "src/SDL/Raw/Enum.hsc" #-}

windowEventNone :: (Num a) => a
windowEventShown :: (Num a) => a
windowEventHidden :: (Num a) => a
windowEventExposed :: (Num a) => a
windowEventMoved :: (Num a) => a
windowEventResized :: (Num a) => a
windowEventSizeChanged :: (Num a) => a
windowEventMinimized :: (Num a) => a
windowEventMaximized :: (Num a) => a
windowEventRestored :: (Num a) => a
windowEventEnter :: (Num a) => a
windowEventLeave :: (Num a) => a
windowEventFocusGained :: (Num a) => a
windowEventFocusLost :: (Num a) => a
windowEventClose :: (Num a) => a

windowEventNone = (0)
{-# LINE 1701 "src/SDL/Raw/Enum.hsc" #-}
windowEventShown = (1)
{-# LINE 1702 "src/SDL/Raw/Enum.hsc" #-}
windowEventHidden = (2)
{-# LINE 1703 "src/SDL/Raw/Enum.hsc" #-}
windowEventExposed = (3)
{-# LINE 1704 "src/SDL/Raw/Enum.hsc" #-}
windowEventMoved = (4)
{-# LINE 1705 "src/SDL/Raw/Enum.hsc" #-}
windowEventResized = (5)
{-# LINE 1706 "src/SDL/Raw/Enum.hsc" #-}
windowEventSizeChanged = (6)
{-# LINE 1707 "src/SDL/Raw/Enum.hsc" #-}
windowEventMinimized = (7)
{-# LINE 1708 "src/SDL/Raw/Enum.hsc" #-}
windowEventMaximized = (8)
{-# LINE 1709 "src/SDL/Raw/Enum.hsc" #-}
windowEventRestored = (9)
{-# LINE 1710 "src/SDL/Raw/Enum.hsc" #-}
windowEventEnter = (10)
{-# LINE 1711 "src/SDL/Raw/Enum.hsc" #-}
windowEventLeave = (11)
{-# LINE 1712 "src/SDL/Raw/Enum.hsc" #-}
windowEventFocusGained = (12)
{-# LINE 1713 "src/SDL/Raw/Enum.hsc" #-}
windowEventFocusLost = (13)
{-# LINE 1714 "src/SDL/Raw/Enum.hsc" #-}
windowEventClose = (14)
{-# LINE 1715 "src/SDL/Raw/Enum.hsc" #-}

windowFlagFullscreen :: (Num a) => a
windowFlagOpenGL :: (Num a) => a
windowFlagShown :: (Num a) => a
windowFlagHidden :: (Num a) => a
windowFlagBorderless :: (Num a) => a
windowFlagResizable :: (Num a) => a
windowFlagMinimized :: (Num a) => a
windowFlagMaximized :: (Num a) => a
windowFlagInputGrabbed :: (Num a) => a
windowFlagInputFocus :: (Num a) => a
windowFlagMouseFocus :: (Num a) => a
windowFlagFullscreenDesktop :: (Num a) => a
windowFlagForeign :: (Num a) => a
windowFlagAllowHighDPI :: (Num a) => a

windowFlagFullscreen = (1)
{-# LINE 1732 "src/SDL/Raw/Enum.hsc" #-}
windowFlagOpenGL = (2)
{-# LINE 1733 "src/SDL/Raw/Enum.hsc" #-}
windowFlagShown = (4)
{-# LINE 1734 "src/SDL/Raw/Enum.hsc" #-}
windowFlagHidden = (8)
{-# LINE 1735 "src/SDL/Raw/Enum.hsc" #-}
windowFlagBorderless = (16)
{-# LINE 1736 "src/SDL/Raw/Enum.hsc" #-}
windowFlagResizable = (32)
{-# LINE 1737 "src/SDL/Raw/Enum.hsc" #-}
windowFlagMinimized = (64)
{-# LINE 1738 "src/SDL/Raw/Enum.hsc" #-}
windowFlagMaximized = (128)
{-# LINE 1739 "src/SDL/Raw/Enum.hsc" #-}
windowFlagInputGrabbed = (256)
{-# LINE 1740 "src/SDL/Raw/Enum.hsc" #-}
windowFlagInputFocus = (512)
{-# LINE 1741 "src/SDL/Raw/Enum.hsc" #-}
windowFlagMouseFocus = (1024)
{-# LINE 1742 "src/SDL/Raw/Enum.hsc" #-}
windowFlagFullscreenDesktop = (4097)
{-# LINE 1743 "src/SDL/Raw/Enum.hsc" #-}
windowFlagForeign = (2048)
{-# LINE 1744 "src/SDL/Raw/Enum.hsc" #-}
windowFlagAllowHighDPI = (8192)
{-# LINE 1745 "src/SDL/Raw/Enum.hsc" #-}

windowPosUndefined :: (Num a) => a
windowPosCentered :: (Num a) => a

windowPosUndefined = (536805376)
{-# LINE 1750 "src/SDL/Raw/Enum.hsc" #-}
windowPosCentered = (805240832)
{-# LINE 1751 "src/SDL/Raw/Enum.hsc" #-}
