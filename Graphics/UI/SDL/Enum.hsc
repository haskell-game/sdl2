#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
#define PROVIDE_PATTERNS
#endif
module Graphics.UI.SDL.Enum (
	-- * Enumerations

	-- ** Audio Status
	AudioStatus,
	audioStatusStopped,
	audioStatusPlaying,
	audioStatusPaused,

#ifdef PROVIDE_PATTERNS
	pattern AudioStatusStopped,
	pattern AudioStatusPlaying,
	pattern AudioStatusPaused,
#endif

	-- ** Blend Mode
	BlendMode,
	blendModeNone,
	blendModeBlend,
	blendModeAdd,
	blendModeMod,

#ifdef PROVIDE_PATTERNS
	pattern BlendModeNone,
	pattern BlendModeBlend,
	pattern BlendModeAdd,
	pattern BlendModeMod,
#endif

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

	-- ** Hint Priority
	HintPriority,
	hintPriorityDefault,
	hintPriorityNormal,
	hintPriorityOverride,

	-- ** Initialization Flag
	InitFlag,
	initFlagTimer,
	initFlagAudio,
	initFlagVideo,
	initFlagJoystick,
	initFlagHaptic,
	initFlagGameController,
	initFlagEvents,
	initFlagNoParachute,
	initFlagEverything,

	-- ** Keycode
	Keycode,
	keycodeUnknown,
	keycodeReturn,
	keycodeEscape,
	keycodeBackspace,
	keycodeTab,
	keycodeSpace,
	keycodeExclaim,
	keycodeQuoteDbl,
	keycodeHash,
	keycodePercent,
	keycodeDollar,
	keycodeAmpersand,
	keycodeQuote,
	keycodeLeftParen,
	keycodeRightParen,
	keycodeAsterisk,
	keycodePlus,
	keycodeComma,
	keycodeMinus,
	keycodePeriod,
	keycodeSlash,
	keycode0,
	keycode1,
	keycode2,
	keycode3,
	keycode4,
	keycode5,
	keycode6,
	keycode7,
	keycode8,
	keycode9,
	keycodeColon,
	keycodeSemicolon,
	keycodeLess,
	keycodeEquals,
	keycodeGreater,
	keycodeQuestion,
	keycodeAt,
	keycodeLeftBracket,
	keycodeBackslash,
	keycodeRightBracket,
	keycodeCaret,
	keycodeUnderscore,
	keycodeBackquote,
	keycodeA,
	keycodeB,
	keycodeC,
	keycodeD,
	keycodeE,
	keycodeF,
	keycodeG,
	keycodeH,
	keycodeI,
	keycodeJ,
	keycodeK,
	keycodeL,
	keycodeM,
	keycodeN,
	keycodeO,
	keycodeP,
	keycodeQ,
	keycodeR,
	keycodeS,
	keycodeT,
	keycodeU,
	keycodeV,
	keycodeW,
	keycodeX,
	keycodeY,
	keycodeZ,
	keycodeCapsLock,
	keycodeF1,
	keycodeF2,
	keycodeF3,
	keycodeF4,
	keycodeF5,
	keycodeF6,
	keycodeF7,
	keycodeF8,
	keycodeF9,
	keycodeF10,
	keycodeF11,
	keycodeF12,
	keycodePrintScreen,
	keycodeScrollLock,
	keycodePause,
	keycodeInsert,
	keycodeHome,
	keycodePageUp,
	keycodeDelete,
	keycodeEnd,
	keycodePageDown,
	keycodeRight,
	keycodeLeft,
	keycodeDown,
	keycodeUp,
	keycodeNumLockClear,
	keycodeKPDivide,
	keycodeKPMultiply,
	keycodeKPMinus,
	keycodeKPPlus,
	keycodeKPEnter,
	keycodeKP1,
	keycodeKP2,
	keycodeKP3,
	keycodeKP4,
	keycodeKP5,
	keycodeKP6,
	keycodeKP7,
	keycodeKP8,
	keycodeKP9,
	keycodeKP0,
	keycodeKPPeriod,
	keycodeApplication,
	keycodePower,
	keycodeKPEquals,
	keycodeF13,
	keycodeF14,
	keycodeF15,
	keycodeF16,
	keycodeF17,
	keycodeF18,
	keycodeF19,
	keycodeF20,
	keycodeF21,
	keycodeF22,
	keycodeF23,
	keycodeF24,
	keycodeExecute,
	keycodeHelp,
	keycodeMenu,
	keycodeSelect,
	keycodeStop,
	keycodeAgain,
	keycodeUndo,
	keycodeCut,
	keycodeCopy,
	keycodePaste,
	keycodeFind,
	keycodeMute,
	keycodeVolumeUp,
	keycodeVolumeDown,
	keycodeKPComma,
	keycodeKPEqualsAS400,
	keycodeAltErase,
	keycodeSysReq,
	keycodeCancel,
	keycodeClear,
	keycodePrior,
	keycodeReturn2,
	keycodeSeparator,
	keycodeOut,
	keycodeOper,
	keycodeClearAgain,
	keycodeCrSel,
	keycodeExSel,
	keycodeKP00,
	keycodeKP000,
	keycodeThousandsSeparator,
	keycodeDecimalSeparator,
	keycodeCurrencyUnit,
	keycodeCurrencySubunit,
	keycodeKPLeftParen,
	keycodeKPRightParen,
	keycodeKPLeftBrace,
	keycodeKPRightBrace,
	keycodeKPTab,
	keycodeKPBackspace,
	keycodeKPA,
	keycodeKPB,
	keycodeKPC,
	keycodeKPD,
	keycodeKPE,
	keycodeKPF,
	keycodeKPXor,
	keycodeKPPower,
	keycodeKPPercent,
	keycodeKPLess,
	keycodeKPGreater,
	keycodeKPAmpersand,
	keycodeKPDblAmpersand,
	keycodeKPVecticalBar,
	keycodeKPDblVerticalBar,
	keycodeKPColon,
	keycodeKPHash,
	keycodeKPSpace,
	keycodeKPAt,
	keycodeKPExclam,
	keycodeKPMemStore,
	keycodeKPMemRecall,
	keycodeKPMemClear,
	keycodeKPMemAdd,
	keycodeKPMemSubtract,
	keycodeKPMemMultiply,
	keycodeKPMemDivide,
	keycodeKPPlusMinus,
	keycodeKPClear,
	keycodeKPClearEntry,
	keycodeKPBinary,
	keycodeKPOctal,
	keycodeKPDecimal,
	keycodeKPHexadecimal,
	keycodeLCtrl,
	keycodeLShift,
	keycodeLAlt,
	keycodeLGUI,
	keycodeRCtrl,
	keycodeRShift,
	keycodeRAlt,
	keycodeRGUI,
	keycodeMode,
	keycodeAudioNext,
	keycodeAudioPrev,
	keycodeAudioStop,
	keycodeAudioPlay,
	keycodeAudioMute,
	keycodeMediaSelect,
	keycodeWWW,
	keycodeMail,
	keycodeCalculator,
	keycodeComputer,
	keycodeACSearch,
	keycodeACHome,
	keycodeACBack,
	keycodeACForward,
	keycodeACStop,
	keycodeACRefresh,
	keycodeACBookmarks,
	keycodeBrightnessDown,
	keycodeBrightnessUp,
	keycodeDisplaySwitch,
	keycodeKbdIllumToggle,
	keycodeKbdIllumDown,
	keycodeKbdIllumUp,
	keycodeEject,
	keycodeSleep,

	-- ** Key Modifier
	Keymod,
	keymodNone,
	keymodLShift,
	keymodRShift,
	keymodShift,
	keymodLCtrl,
	keymodRCtrl,
	keymodCtrl,
	keymodLAlt,
	keymodRAlt,
	keymodAlt,
	keymodLGUI,
	keymodRGUI,
	keymodGUI,
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

	-- ** Audio Allowed Changes
	audioAllowFrequencyChange,
	audioAllowFormatChange,
	audioAllowChannelsChange,
	audioAllowAnyChange,

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

	-- ** Key States
	keyPressed,
	keyReleased,

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

#include "SDL.h"

import Data.Int
import Data.Word

type AudioStatus = (#type SDL_AudioStatus)

audioStatusStopped :: AudioStatus
audioStatusPlaying :: AudioStatus
audioStatusPaused :: AudioStatus

audioStatusStopped = (#const SDL_AUDIO_STOPPED)
audioStatusPlaying = (#const SDL_AUDIO_PLAYING)
audioStatusPaused = (#const SDL_AUDIO_PAUSED)

#ifdef PROVIDE_PATTERNS
pattern AudioStatusStopped = (#const SDL_AUDIO_STOPPED) :: AudioStatus
pattern AudioStatusPlaying = (#const SDL_AUDIO_PLAYING) :: AudioStatus
pattern AudioStatusPaused = (#const SDL_AUDIO_PAUSED) :: AudioStatus
#endif

type BlendMode = (#type SDL_BlendMode)

blendModeNone :: BlendMode
blendModeBlend :: BlendMode
blendModeAdd :: BlendMode
blendModeMod :: BlendMode

blendModeNone = (#const SDL_BLENDMODE_NONE)
blendModeBlend = (#const SDL_BLENDMODE_BLEND)
blendModeAdd = (#const SDL_BLENDMODE_ADD)
blendModeMod = (#const SDL_BLENDMODE_MOD)

#ifdef PROVIDE_PATTERNS
pattern BlendModeNone = (#const SDL_BLENDMODE_NONE) :: BlendMode
pattern BlendModeBlend = (#const SDL_BLENDMODE_BLEND) :: BlendMode
pattern BlendModeAdd = (#const SDL_BLENDMODE_ADD) :: BlendMode
pattern BlendModeMod = (#const SDL_BLENDMODE_MOD) :: BlendMode
#endif

type EventAction = (#type SDL_eventaction)

eventActionAddEvent :: EventAction
eventActionPeekEvent :: EventAction
eventActionGetEvent :: EventAction

eventActionAddEvent = (#const SDL_ADDEVENT)
eventActionPeekEvent = (#const SDL_PEEKEVENT)
eventActionGetEvent = (#const SDL_GETEVENT)

type GameControllerAxis = (#type SDL_GameControllerAxis)

gameControllerAxisInvalid :: GameControllerAxis
gameControllerAxisLeftX :: GameControllerAxis
gameControllerAxisLeftY :: GameControllerAxis
gameControllerAxisRightX :: GameControllerAxis
gameControllerAxisRightY :: GameControllerAxis
gameControllerAxisTriggerLeft :: GameControllerAxis
gameControllerAxisTriggerRight :: GameControllerAxis
gameControllerAxisMax :: GameControllerAxis

gameControllerAxisInvalid = (#const SDL_CONTROLLER_AXIS_INVALID)
gameControllerAxisLeftX = (#const SDL_CONTROLLER_AXIS_LEFTX)
gameControllerAxisLeftY = (#const SDL_CONTROLLER_AXIS_LEFTY)
gameControllerAxisRightX = (#const SDL_CONTROLLER_AXIS_RIGHTX)
gameControllerAxisRightY = (#const SDL_CONTROLLER_AXIS_RIGHTY)
gameControllerAxisTriggerLeft = (#const SDL_CONTROLLER_AXIS_TRIGGERLEFT)
gameControllerAxisTriggerRight = (#const SDL_CONTROLLER_AXIS_TRIGGERRIGHT)
gameControllerAxisMax = (#const SDL_CONTROLLER_AXIS_MAX)

type GameControllerButton = (#type SDL_GameControllerButton)

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

gameControllerButtonInvalid = (#const SDL_CONTROLLER_BUTTON_INVALID)
gameControllerButtonA = (#const SDL_CONTROLLER_BUTTON_A)
gameControllerButtonB = (#const SDL_CONTROLLER_BUTTON_B)
gameControllerButtonX = (#const SDL_CONTROLLER_BUTTON_X)
gameControllerButtonY = (#const SDL_CONTROLLER_BUTTON_Y)
gameControllerButtonBack = (#const SDL_CONTROLLER_BUTTON_BACK)
gameControllerButtonGuide = (#const SDL_CONTROLLER_BUTTON_GUIDE)
gameControllerButtonStart = (#const SDL_CONTROLLER_BUTTON_START)
gameControllerButtonLeftStick = (#const SDL_CONTROLLER_BUTTON_LEFTSTICK)
gameControllerButtonRightStick = (#const SDL_CONTROLLER_BUTTON_RIGHTSTICK)
gameControllerButtonLeftShoulder = (#const SDL_CONTROLLER_BUTTON_LEFTSHOULDER)
gameControllerButtonRightShoulder = (#const SDL_CONTROLLER_BUTTON_RIGHTSHOULDER)
gameControllerButtonDPadUp = (#const SDL_CONTROLLER_BUTTON_DPAD_UP)
gameControllerButtonDPadDown = (#const SDL_CONTROLLER_BUTTON_DPAD_DOWN)
gameControllerButtonDPadLeft = (#const SDL_CONTROLLER_BUTTON_DPAD_LEFT)
gameControllerButtonDPadRight = (#const SDL_CONTROLLER_BUTTON_DPAD_RIGHT)
gameControllerButtonMax = (#const SDL_CONTROLLER_BUTTON_MAX)

type GLattr = (#type SDL_GLattr)

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

glAttrRedSize = (#const SDL_GL_RED_SIZE)
glAttrGreenSize = (#const SDL_GL_GREEN_SIZE)
glAttrBlueSize = (#const SDL_GL_BLUE_SIZE)
glAttrAlphaSize = (#const SDL_GL_ALPHA_SIZE)
glAttrBufferSize = (#const SDL_GL_BUFFER_SIZE)
glAttrDoubleBuffer = (#const SDL_GL_DOUBLEBUFFER)
glAttrDepthSize = (#const SDL_GL_DEPTH_SIZE)
glAttrStencilSize = (#const SDL_GL_STENCIL_SIZE)
glAttrAccumRedSize = (#const SDL_GL_ACCUM_RED_SIZE)
glAttrAccumGreenSize = (#const SDL_GL_ACCUM_GREEN_SIZE)
glAttrAccumBlueSize = (#const SDL_GL_ACCUM_BLUE_SIZE)
glAttrAccumAlphaSize = (#const SDL_GL_ACCUM_ALPHA_SIZE)
glAttrStereo = (#const SDL_GL_STEREO)
glAttrMultiSampleBuffers = (#const SDL_GL_MULTISAMPLEBUFFERS)
glAttrMultiSampleSamples = (#const SDL_GL_MULTISAMPLESAMPLES)
glAttrAcceleratedVisual = (#const SDL_GL_ACCELERATED_VISUAL)
glAttrRetainedBacking = (#const SDL_GL_RETAINED_BACKING)
glAttrContextMajorVersion = (#const SDL_GL_CONTEXT_MAJOR_VERSION)
glAttrContextMinorVersion = (#const SDL_GL_CONTEXT_MINOR_VERSION)
glAttrContextEGL = (#const SDL_GL_CONTEXT_EGL)
glAttrContextFlags = (#const SDL_GL_CONTEXT_FLAGS)
glAttrContextProfileMask = (#const SDL_GL_CONTEXT_PROFILE_MASK)
glAttrShareWithCurrentContext = (#const SDL_GL_SHARE_WITH_CURRENT_CONTEXT)
glAttrFramebufferSRGBCapable = (#const SDL_GL_FRAMEBUFFER_SRGB_CAPABLE)

type HintPriority = (#type SDL_HintPriority)

hintPriorityDefault :: HintPriority
hintPriorityNormal :: HintPriority
hintPriorityOverride :: HintPriority

hintPriorityDefault = (#const SDL_HINT_DEFAULT)
hintPriorityNormal = (#const SDL_HINT_NORMAL)
hintPriorityOverride = (#const SDL_HINT_OVERRIDE)

type InitFlag = Word32

initFlagTimer :: InitFlag
initFlagAudio :: InitFlag
initFlagVideo :: InitFlag
initFlagJoystick :: InitFlag
initFlagHaptic :: InitFlag
initFlagGameController :: InitFlag
initFlagEvents :: InitFlag
initFlagNoParachute :: InitFlag
initFlagEverything :: InitFlag

initFlagTimer = (#const SDL_INIT_TIMER)
initFlagAudio = (#const SDL_INIT_AUDIO)
initFlagVideo = (#const SDL_INIT_VIDEO)
initFlagJoystick = (#const SDL_INIT_JOYSTICK)
initFlagHaptic = (#const SDL_INIT_HAPTIC)
initFlagGameController = (#const SDL_INIT_GAMECONTROLLER)
initFlagEvents = (#const SDL_INIT_EVENTS)
initFlagNoParachute = (#const SDL_INIT_NOPARACHUTE)
initFlagEverything = (#const SDL_INIT_EVERYTHING)

type Keycode = (#type SDL_Keycode)

keycodeUnknown :: Keycode
keycodeReturn :: Keycode
keycodeEscape :: Keycode
keycodeBackspace :: Keycode
keycodeTab :: Keycode
keycodeSpace :: Keycode
keycodeExclaim :: Keycode
keycodeQuoteDbl :: Keycode
keycodeHash :: Keycode
keycodePercent :: Keycode
keycodeDollar :: Keycode
keycodeAmpersand :: Keycode
keycodeQuote :: Keycode
keycodeLeftParen :: Keycode
keycodeRightParen :: Keycode
keycodeAsterisk :: Keycode
keycodePlus :: Keycode
keycodeComma :: Keycode
keycodeMinus :: Keycode
keycodePeriod :: Keycode
keycodeSlash :: Keycode
keycode0 :: Keycode
keycode1 :: Keycode
keycode2 :: Keycode
keycode3 :: Keycode
keycode4 :: Keycode
keycode5 :: Keycode
keycode6 :: Keycode
keycode7 :: Keycode
keycode8 :: Keycode
keycode9 :: Keycode
keycodeColon :: Keycode
keycodeSemicolon :: Keycode
keycodeLess :: Keycode
keycodeEquals :: Keycode
keycodeGreater :: Keycode
keycodeQuestion :: Keycode
keycodeAt :: Keycode
keycodeLeftBracket :: Keycode
keycodeBackslash :: Keycode
keycodeRightBracket :: Keycode
keycodeCaret :: Keycode
keycodeUnderscore :: Keycode
keycodeBackquote :: Keycode
keycodeA :: Keycode
keycodeB :: Keycode
keycodeC :: Keycode
keycodeD :: Keycode
keycodeE :: Keycode
keycodeF :: Keycode
keycodeG :: Keycode
keycodeH :: Keycode
keycodeI :: Keycode
keycodeJ :: Keycode
keycodeK :: Keycode
keycodeL :: Keycode
keycodeM :: Keycode
keycodeN :: Keycode
keycodeO :: Keycode
keycodeP :: Keycode
keycodeQ :: Keycode
keycodeR :: Keycode
keycodeS :: Keycode
keycodeT :: Keycode
keycodeU :: Keycode
keycodeV :: Keycode
keycodeW :: Keycode
keycodeX :: Keycode
keycodeY :: Keycode
keycodeZ :: Keycode
keycodeCapsLock :: Keycode
keycodeF1 :: Keycode
keycodeF2 :: Keycode
keycodeF3 :: Keycode
keycodeF4 :: Keycode
keycodeF5 :: Keycode
keycodeF6 :: Keycode
keycodeF7 :: Keycode
keycodeF8 :: Keycode
keycodeF9 :: Keycode
keycodeF10 :: Keycode
keycodeF11 :: Keycode
keycodeF12 :: Keycode
keycodePrintScreen :: Keycode
keycodeScrollLock :: Keycode
keycodePause :: Keycode
keycodeInsert :: Keycode
keycodeHome :: Keycode
keycodePageUp :: Keycode
keycodeDelete :: Keycode
keycodeEnd :: Keycode
keycodePageDown :: Keycode
keycodeRight :: Keycode
keycodeLeft :: Keycode
keycodeDown :: Keycode
keycodeUp :: Keycode
keycodeNumLockClear :: Keycode
keycodeKPDivide :: Keycode
keycodeKPMultiply :: Keycode
keycodeKPMinus :: Keycode
keycodeKPPlus :: Keycode
keycodeKPEnter :: Keycode
keycodeKP1 :: Keycode
keycodeKP2 :: Keycode
keycodeKP3 :: Keycode
keycodeKP4 :: Keycode
keycodeKP5 :: Keycode
keycodeKP6 :: Keycode
keycodeKP7 :: Keycode
keycodeKP8 :: Keycode
keycodeKP9 :: Keycode
keycodeKP0 :: Keycode
keycodeKPPeriod :: Keycode
keycodeApplication :: Keycode
keycodePower :: Keycode
keycodeKPEquals :: Keycode
keycodeF13 :: Keycode
keycodeF14 :: Keycode
keycodeF15 :: Keycode
keycodeF16 :: Keycode
keycodeF17 :: Keycode
keycodeF18 :: Keycode
keycodeF19 :: Keycode
keycodeF20 :: Keycode
keycodeF21 :: Keycode
keycodeF22 :: Keycode
keycodeF23 :: Keycode
keycodeF24 :: Keycode
keycodeExecute :: Keycode
keycodeHelp :: Keycode
keycodeMenu :: Keycode
keycodeSelect :: Keycode
keycodeStop :: Keycode
keycodeAgain :: Keycode
keycodeUndo :: Keycode
keycodeCut :: Keycode
keycodeCopy :: Keycode
keycodePaste :: Keycode
keycodeFind :: Keycode
keycodeMute :: Keycode
keycodeVolumeUp :: Keycode
keycodeVolumeDown :: Keycode
keycodeKPComma :: Keycode
keycodeKPEqualsAS400 :: Keycode
keycodeAltErase :: Keycode
keycodeSysReq :: Keycode
keycodeCancel :: Keycode
keycodeClear :: Keycode
keycodePrior :: Keycode
keycodeReturn2 :: Keycode
keycodeSeparator :: Keycode
keycodeOut :: Keycode
keycodeOper :: Keycode
keycodeClearAgain :: Keycode
keycodeCrSel :: Keycode
keycodeExSel :: Keycode
keycodeKP00 :: Keycode
keycodeKP000 :: Keycode
keycodeThousandsSeparator :: Keycode
keycodeDecimalSeparator :: Keycode
keycodeCurrencyUnit :: Keycode
keycodeCurrencySubunit :: Keycode
keycodeKPLeftParen :: Keycode
keycodeKPRightParen :: Keycode
keycodeKPLeftBrace :: Keycode
keycodeKPRightBrace :: Keycode
keycodeKPTab :: Keycode
keycodeKPBackspace :: Keycode
keycodeKPA :: Keycode
keycodeKPB :: Keycode
keycodeKPC :: Keycode
keycodeKPD :: Keycode
keycodeKPE :: Keycode
keycodeKPF :: Keycode
keycodeKPXor :: Keycode
keycodeKPPower :: Keycode
keycodeKPPercent :: Keycode
keycodeKPLess :: Keycode
keycodeKPGreater :: Keycode
keycodeKPAmpersand :: Keycode
keycodeKPDblAmpersand :: Keycode
keycodeKPVecticalBar :: Keycode
keycodeKPDblVerticalBar :: Keycode
keycodeKPColon :: Keycode
keycodeKPHash :: Keycode
keycodeKPSpace :: Keycode
keycodeKPAt :: Keycode
keycodeKPExclam :: Keycode
keycodeKPMemStore :: Keycode
keycodeKPMemRecall :: Keycode
keycodeKPMemClear :: Keycode
keycodeKPMemAdd :: Keycode
keycodeKPMemSubtract :: Keycode
keycodeKPMemMultiply :: Keycode
keycodeKPMemDivide :: Keycode
keycodeKPPlusMinus :: Keycode
keycodeKPClear :: Keycode
keycodeKPClearEntry :: Keycode
keycodeKPBinary :: Keycode
keycodeKPOctal :: Keycode
keycodeKPDecimal :: Keycode
keycodeKPHexadecimal :: Keycode
keycodeLCtrl :: Keycode
keycodeLShift :: Keycode
keycodeLAlt :: Keycode
keycodeLGUI :: Keycode
keycodeRCtrl :: Keycode
keycodeRShift :: Keycode
keycodeRAlt :: Keycode
keycodeRGUI :: Keycode
keycodeMode :: Keycode
keycodeAudioNext :: Keycode
keycodeAudioPrev :: Keycode
keycodeAudioStop :: Keycode
keycodeAudioPlay :: Keycode
keycodeAudioMute :: Keycode
keycodeMediaSelect :: Keycode
keycodeWWW :: Keycode
keycodeMail :: Keycode
keycodeCalculator :: Keycode
keycodeComputer :: Keycode
keycodeACSearch :: Keycode
keycodeACHome :: Keycode
keycodeACBack :: Keycode
keycodeACForward :: Keycode
keycodeACStop :: Keycode
keycodeACRefresh :: Keycode
keycodeACBookmarks :: Keycode
keycodeBrightnessDown :: Keycode
keycodeBrightnessUp :: Keycode
keycodeDisplaySwitch :: Keycode
keycodeKbdIllumToggle :: Keycode
keycodeKbdIllumDown :: Keycode
keycodeKbdIllumUp :: Keycode
keycodeEject :: Keycode
keycodeSleep :: Keycode

keycodeUnknown = (#const SDLK_UNKNOWN)
keycodeReturn = (#const SDLK_RETURN)
keycodeEscape = (#const SDLK_ESCAPE)
keycodeBackspace = (#const SDLK_BACKSPACE)
keycodeTab = (#const SDLK_TAB)
keycodeSpace = (#const SDLK_SPACE)
keycodeExclaim = (#const SDLK_EXCLAIM)
keycodeQuoteDbl = (#const SDLK_QUOTEDBL)
keycodeHash = (#const SDLK_HASH)
keycodePercent = (#const SDLK_PERCENT)
keycodeDollar = (#const SDLK_DOLLAR)
keycodeAmpersand = (#const SDLK_AMPERSAND)
keycodeQuote = (#const SDLK_QUOTE)
keycodeLeftParen = (#const SDLK_LEFTPAREN)
keycodeRightParen = (#const SDLK_RIGHTPAREN)
keycodeAsterisk = (#const SDLK_ASTERISK)
keycodePlus = (#const SDLK_PLUS)
keycodeComma = (#const SDLK_COMMA)
keycodeMinus = (#const SDLK_MINUS)
keycodePeriod = (#const SDLK_PERIOD)
keycodeSlash = (#const SDLK_SLASH)
keycode0 = (#const SDLK_0)
keycode1 = (#const SDLK_1)
keycode2 = (#const SDLK_2)
keycode3 = (#const SDLK_3)
keycode4 = (#const SDLK_4)
keycode5 = (#const SDLK_5)
keycode6 = (#const SDLK_6)
keycode7 = (#const SDLK_7)
keycode8 = (#const SDLK_8)
keycode9 = (#const SDLK_9)
keycodeColon = (#const SDLK_COLON)
keycodeSemicolon = (#const SDLK_SEMICOLON)
keycodeLess = (#const SDLK_LESS)
keycodeEquals = (#const SDLK_EQUALS)
keycodeGreater = (#const SDLK_GREATER)
keycodeQuestion = (#const SDLK_QUESTION)
keycodeAt = (#const SDLK_AT)
keycodeLeftBracket = (#const SDLK_LEFTBRACKET)
keycodeBackslash = (#const SDLK_BACKSLASH)
keycodeRightBracket = (#const SDLK_RIGHTBRACKET)
keycodeCaret = (#const SDLK_CARET)
keycodeUnderscore = (#const SDLK_UNDERSCORE)
keycodeBackquote = (#const SDLK_BACKQUOTE)
keycodeA = (#const SDLK_a)
keycodeB = (#const SDLK_b)
keycodeC = (#const SDLK_c)
keycodeD = (#const SDLK_d)
keycodeE = (#const SDLK_e)
keycodeF = (#const SDLK_f)
keycodeG = (#const SDLK_g)
keycodeH = (#const SDLK_h)
keycodeI = (#const SDLK_i)
keycodeJ = (#const SDLK_j)
keycodeK = (#const SDLK_k)
keycodeL = (#const SDLK_l)
keycodeM = (#const SDLK_m)
keycodeN = (#const SDLK_n)
keycodeO = (#const SDLK_o)
keycodeP = (#const SDLK_p)
keycodeQ = (#const SDLK_q)
keycodeR = (#const SDLK_r)
keycodeS = (#const SDLK_s)
keycodeT = (#const SDLK_t)
keycodeU = (#const SDLK_u)
keycodeV = (#const SDLK_v)
keycodeW = (#const SDLK_w)
keycodeX = (#const SDLK_x)
keycodeY = (#const SDLK_y)
keycodeZ = (#const SDLK_z)
keycodeCapsLock = (#const SDLK_CAPSLOCK)
keycodeF1 = (#const SDLK_F1)
keycodeF2 = (#const SDLK_F2)
keycodeF3 = (#const SDLK_F3)
keycodeF4 = (#const SDLK_F4)
keycodeF5 = (#const SDLK_F5)
keycodeF6 = (#const SDLK_F6)
keycodeF7 = (#const SDLK_F7)
keycodeF8 = (#const SDLK_F8)
keycodeF9 = (#const SDLK_F9)
keycodeF10 = (#const SDLK_F10)
keycodeF11 = (#const SDLK_F11)
keycodeF12 = (#const SDLK_F12)
keycodePrintScreen = (#const SDLK_PRINTSCREEN)
keycodeScrollLock = (#const SDLK_SCROLLLOCK)
keycodePause = (#const SDLK_PAUSE)
keycodeInsert = (#const SDLK_INSERT)
keycodeHome = (#const SDLK_HOME)
keycodePageUp = (#const SDLK_PAGEUP)
keycodeDelete = (#const SDLK_DELETE)
keycodeEnd = (#const SDLK_END)
keycodePageDown = (#const SDLK_PAGEDOWN)
keycodeRight = (#const SDLK_RIGHT)
keycodeLeft = (#const SDLK_LEFT)
keycodeDown = (#const SDLK_DOWN)
keycodeUp = (#const SDLK_UP)
keycodeNumLockClear = (#const SDLK_NUMLOCKCLEAR)
keycodeKPDivide = (#const SDLK_KP_DIVIDE)
keycodeKPMultiply = (#const SDLK_KP_MULTIPLY)
keycodeKPMinus = (#const SDLK_KP_MINUS)
keycodeKPPlus = (#const SDLK_KP_PLUS)
keycodeKPEnter = (#const SDLK_KP_ENTER)
keycodeKP1 = (#const SDLK_KP_1)
keycodeKP2 = (#const SDLK_KP_2)
keycodeKP3 = (#const SDLK_KP_3)
keycodeKP4 = (#const SDLK_KP_4)
keycodeKP5 = (#const SDLK_KP_5)
keycodeKP6 = (#const SDLK_KP_6)
keycodeKP7 = (#const SDLK_KP_7)
keycodeKP8 = (#const SDLK_KP_8)
keycodeKP9 = (#const SDLK_KP_9)
keycodeKP0 = (#const SDLK_KP_0)
keycodeKPPeriod = (#const SDLK_KP_PERIOD)
keycodeApplication = (#const SDLK_APPLICATION)
keycodePower = (#const SDLK_POWER)
keycodeKPEquals = (#const SDLK_KP_EQUALS)
keycodeF13 = (#const SDLK_F13)
keycodeF14 = (#const SDLK_F14)
keycodeF15 = (#const SDLK_F15)
keycodeF16 = (#const SDLK_F16)
keycodeF17 = (#const SDLK_F17)
keycodeF18 = (#const SDLK_F18)
keycodeF19 = (#const SDLK_F19)
keycodeF20 = (#const SDLK_F20)
keycodeF21 = (#const SDLK_F21)
keycodeF22 = (#const SDLK_F22)
keycodeF23 = (#const SDLK_F23)
keycodeF24 = (#const SDLK_F24)
keycodeExecute = (#const SDLK_EXECUTE)
keycodeHelp = (#const SDLK_HELP)
keycodeMenu = (#const SDLK_MENU)
keycodeSelect = (#const SDLK_SELECT)
keycodeStop = (#const SDLK_STOP)
keycodeAgain = (#const SDLK_AGAIN)
keycodeUndo = (#const SDLK_UNDO)
keycodeCut = (#const SDLK_CUT)
keycodeCopy = (#const SDLK_COPY)
keycodePaste = (#const SDLK_PASTE)
keycodeFind = (#const SDLK_FIND)
keycodeMute = (#const SDLK_MUTE)
keycodeVolumeUp = (#const SDLK_VOLUMEUP)
keycodeVolumeDown = (#const SDLK_VOLUMEDOWN)
keycodeKPComma = (#const SDLK_KP_COMMA)
keycodeKPEqualsAS400 = (#const SDLK_KP_EQUALSAS400)
keycodeAltErase = (#const SDLK_ALTERASE)
keycodeSysReq = (#const SDLK_SYSREQ)
keycodeCancel = (#const SDLK_CANCEL)
keycodeClear = (#const SDLK_CLEAR)
keycodePrior = (#const SDLK_PRIOR)
keycodeReturn2 = (#const SDLK_RETURN2)
keycodeSeparator = (#const SDLK_SEPARATOR)
keycodeOut = (#const SDLK_OUT)
keycodeOper = (#const SDLK_OPER)
keycodeClearAgain = (#const SDLK_CLEARAGAIN)
keycodeCrSel = (#const SDLK_CRSEL)
keycodeExSel = (#const SDLK_EXSEL)
keycodeKP00 = (#const SDLK_KP_00)
keycodeKP000 = (#const SDLK_KP_000)
keycodeThousandsSeparator = (#const SDLK_THOUSANDSSEPARATOR)
keycodeDecimalSeparator = (#const SDLK_DECIMALSEPARATOR)
keycodeCurrencyUnit = (#const SDLK_CURRENCYUNIT)
keycodeCurrencySubunit = (#const SDLK_CURRENCYSUBUNIT)
keycodeKPLeftParen = (#const SDLK_KP_LEFTPAREN)
keycodeKPRightParen = (#const SDLK_KP_RIGHTPAREN)
keycodeKPLeftBrace = (#const SDLK_KP_LEFTBRACE)
keycodeKPRightBrace = (#const SDLK_KP_RIGHTBRACE)
keycodeKPTab = (#const SDLK_KP_TAB)
keycodeKPBackspace = (#const SDLK_KP_BACKSPACE)
keycodeKPA = (#const SDLK_KP_A)
keycodeKPB = (#const SDLK_KP_B)
keycodeKPC = (#const SDLK_KP_C)
keycodeKPD = (#const SDLK_KP_D)
keycodeKPE = (#const SDLK_KP_E)
keycodeKPF = (#const SDLK_KP_F)
keycodeKPXor = (#const SDLK_KP_XOR)
keycodeKPPower = (#const SDLK_KP_POWER)
keycodeKPPercent = (#const SDLK_KP_PERCENT)
keycodeKPLess = (#const SDLK_KP_LESS)
keycodeKPGreater = (#const SDLK_KP_GREATER)
keycodeKPAmpersand = (#const SDLK_KP_AMPERSAND)
keycodeKPDblAmpersand = (#const SDLK_KP_DBLAMPERSAND)
keycodeKPVecticalBar = (#const SDLK_KP_VERTICALBAR)
keycodeKPDblVerticalBar = (#const SDLK_KP_DBLVERTICALBAR)
keycodeKPColon = (#const SDLK_KP_COLON)
keycodeKPHash = (#const SDLK_KP_HASH)
keycodeKPSpace = (#const SDLK_KP_SPACE)
keycodeKPAt = (#const SDLK_KP_AT)
keycodeKPExclam = (#const SDLK_KP_EXCLAM)
keycodeKPMemStore = (#const SDLK_KP_MEMSTORE)
keycodeKPMemRecall = (#const SDLK_KP_MEMRECALL)
keycodeKPMemClear = (#const SDLK_KP_MEMCLEAR)
keycodeKPMemAdd = (#const SDLK_KP_MEMADD)
keycodeKPMemSubtract = (#const SDLK_KP_MEMSUBTRACT)
keycodeKPMemMultiply = (#const SDLK_KP_MEMMULTIPLY)
keycodeKPMemDivide = (#const SDLK_KP_MEMDIVIDE)
keycodeKPPlusMinus = (#const SDLK_KP_PLUSMINUS)
keycodeKPClear = (#const SDLK_KP_CLEAR)
keycodeKPClearEntry = (#const SDLK_KP_CLEARENTRY)
keycodeKPBinary = (#const SDLK_KP_BINARY)
keycodeKPOctal = (#const SDLK_KP_OCTAL)
keycodeKPDecimal = (#const SDLK_KP_DECIMAL)
keycodeKPHexadecimal = (#const SDLK_KP_HEXADECIMAL)
keycodeLCtrl = (#const SDLK_LCTRL)
keycodeLShift = (#const SDLK_LSHIFT)
keycodeLAlt = (#const SDLK_LALT)
keycodeLGUI = (#const SDLK_LGUI)
keycodeRCtrl = (#const SDLK_RCTRL)
keycodeRShift = (#const SDLK_RSHIFT)
keycodeRAlt = (#const SDLK_RALT)
keycodeRGUI = (#const SDLK_RGUI)
keycodeMode = (#const SDLK_MODE)
keycodeAudioNext = (#const SDLK_AUDIONEXT)
keycodeAudioPrev = (#const SDLK_AUDIONEXT)
keycodeAudioStop = (#const SDLK_AUDIOSTOP)
keycodeAudioPlay = (#const SDLK_AUDIOPLAY)
keycodeAudioMute = (#const SDLK_AUDIOMUTE)
keycodeMediaSelect = (#const SDLK_MEDIASELECT)
keycodeWWW = (#const SDLK_WWW)
keycodeMail = (#const SDLK_MAIL)
keycodeCalculator = (#const SDLK_CALCULATOR)
keycodeComputer = (#const SDLK_COMPUTER)
keycodeACSearch = (#const SDLK_AC_SEARCH)
keycodeACHome = (#const SDLK_AC_HOME)
keycodeACBack = (#const SDLK_AC_BACK)
keycodeACForward = (#const SDLK_AC_FORWARD)
keycodeACStop = (#const SDLK_AC_STOP)
keycodeACRefresh = (#const SDLK_AC_REFRESH)
keycodeACBookmarks = (#const SDLK_AC_BOOKMARKS)
keycodeBrightnessDown = (#const SDLK_BRIGHTNESSDOWN)
keycodeBrightnessUp = (#const SDLK_BRIGHTNESSUP)
keycodeDisplaySwitch = (#const SDLK_DISPLAYSWITCH)
keycodeKbdIllumToggle = (#const SDLK_KBDILLUMTOGGLE)
keycodeKbdIllumDown = (#const SDLK_KBDILLUMDOWN)
keycodeKbdIllumUp = (#const SDLK_KBDILLUMUP)
keycodeEject = (#const SDLK_EJECT)
keycodeSleep = (#const SDLK_SLEEP)

type Keymod = (#type SDL_Keymod)

keymodNone :: Num a => a
keymodLShift :: Num a => a
keymodRShift :: Num a => a
keymodShift :: Num a => a
keymodLCtrl :: Num a => a
keymodRCtrl :: Num a => a
keymodCtrl :: Num a => a
keymodLAlt :: Num a => a
keymodRAlt :: Num a => a
keymodAlt :: Num a => a
keymodLGUI :: Num a => a
keymodRGUI :: Num a => a
keymodGUI :: Num a => a
keymodNum :: Num a => a
keymodCaps :: Num a => a
keymodMode :: Num a => a
keymodReserved :: Num a => a

keymodNone = (#const KMOD_NONE)
keymodLShift = (#const KMOD_LSHIFT)
keymodRShift = (#const KMOD_RSHIFT)
keymodShift = (#const KMOD_SHIFT)
keymodLCtrl = (#const KMOD_LCTRL)
keymodRCtrl = (#const KMOD_RCTRL)
keymodCtrl = (#const KMOD_CTRL)
keymodLAlt = (#const KMOD_LALT)
keymodRAlt = (#const KMOD_RALT)
keymodAlt = (#const KMOD_ALT)
keymodLGUI = (#const KMOD_LGUI)
keymodRGUI = (#const KMOD_RGUI)
keymodGUI = (#const KMOD_GUI)
keymodNum = (#const KMOD_NUM)
keymodCaps = (#const KMOD_CAPS)
keymodMode = (#const KMOD_MODE)
keymodReserved = (#const KMOD_RESERVED)

type LogPriority = (#type SDL_LogPriority)

logPriorityVerbose :: LogPriority
logPriorityDebug :: LogPriority
logPriorityInfo :: LogPriority
logPriorityWarn :: LogPriority
logPriorityError :: LogPriority
logPriorityCritical :: LogPriority
logPriorityPriorities :: LogPriority

logPriorityVerbose = (#const SDL_LOG_PRIORITY_VERBOSE)
logPriorityDebug = (#const SDL_LOG_PRIORITY_DEBUG)
logPriorityInfo = (#const SDL_LOG_PRIORITY_INFO)
logPriorityWarn = (#const SDL_LOG_PRIORITY_WARN)
logPriorityError = (#const SDL_LOG_PRIORITY_ERROR)
logPriorityCritical = (#const SDL_LOG_PRIORITY_CRITICAL)
logPriorityPriorities = (#const SDL_NUM_LOG_PRIORITIES)

type PowerState = (#type SDL_PowerState)

powerStateUnknown :: PowerState
powerStateOnBattery :: PowerState
powerStateNoBattery :: PowerState
powerStateCharging :: PowerState
powerStateCharged :: PowerState

powerStateUnknown = (#const SDL_POWERSTATE_UNKNOWN)
powerStateOnBattery = (#const SDL_POWERSTATE_ON_BATTERY)
powerStateNoBattery = (#const SDL_POWERSTATE_NO_BATTERY)
powerStateCharging = (#const SDL_POWERSTATE_CHARGING)
powerStateCharged = (#const SDL_POWERSTATE_CHARGED)

type RendererFlip = (#type SDL_RendererFlip)

rendererFlipNone :: RendererFlip
rendererFlipHorizontal :: RendererFlip
rendererFlipVertical :: RendererFlip

rendererFlipNone = (#const SDL_FLIP_NONE)
rendererFlipHorizontal = (#const SDL_FLIP_HORIZONTAL)
rendererFlipVertical = (#const SDL_FLIP_VERTICAL)

type Scancode = (#type SDL_Scancode)

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

scancodeUnknown = (#const SDL_SCANCODE_UNKNOWN)
scancodeA = (#const SDL_SCANCODE_A)
scancodeB = (#const SDL_SCANCODE_B)
scancodeC = (#const SDL_SCANCODE_C)
scancodeD = (#const SDL_SCANCODE_D)
scancodeE = (#const SDL_SCANCODE_E)
scancodeF = (#const SDL_SCANCODE_F)
scancodeG = (#const SDL_SCANCODE_G)
scancodeH = (#const SDL_SCANCODE_H)
scancodeI = (#const SDL_SCANCODE_I)
scancodeJ = (#const SDL_SCANCODE_J)
scancodeK = (#const SDL_SCANCODE_K)
scancodeL = (#const SDL_SCANCODE_L)
scancodeM = (#const SDL_SCANCODE_M)
scancodeN = (#const SDL_SCANCODE_N)
scancodeO = (#const SDL_SCANCODE_O)
scancodeP = (#const SDL_SCANCODE_P)
scancodeQ = (#const SDL_SCANCODE_Q)
scancodeR = (#const SDL_SCANCODE_R)
scancodeS = (#const SDL_SCANCODE_S)
scancodeT = (#const SDL_SCANCODE_T)
scancodeU = (#const SDL_SCANCODE_U)
scancodeV = (#const SDL_SCANCODE_V)
scancodeW = (#const SDL_SCANCODE_W)
scancodeX = (#const SDL_SCANCODE_X)
scancodeY = (#const SDL_SCANCODE_Y)
scancodeZ = (#const SDL_SCANCODE_Z)
scancode1 = (#const SDL_SCANCODE_1)
scancode2 = (#const SDL_SCANCODE_2)
scancode3 = (#const SDL_SCANCODE_3)
scancode4 = (#const SDL_SCANCODE_4)
scancode5 = (#const SDL_SCANCODE_5)
scancode6 = (#const SDL_SCANCODE_6)
scancode7 = (#const SDL_SCANCODE_7)
scancode8 = (#const SDL_SCANCODE_8)
scancode9 = (#const SDL_SCANCODE_9)
scancode0 = (#const SDL_SCANCODE_0)
scancodeReturn = (#const SDL_SCANCODE_RETURN)
scancodeEscape = (#const SDL_SCANCODE_ESCAPE)
scancodeBackspace = (#const SDL_SCANCODE_BACKSPACE)
scancodeTab = (#const SDL_SCANCODE_TAB)
scancodeSpace = (#const SDL_SCANCODE_SPACE)
scancodeMinus = (#const SDL_SCANCODE_MINUS)
scancodeEquals = (#const SDL_SCANCODE_EQUALS)
scancodeLeftBracket = (#const SDL_SCANCODE_LEFTBRACKET)
scancodeRightBracket = (#const SDL_SCANCODE_RIGHTBRACKET)
scancodeBackslash = (#const SDL_SCANCODE_BACKSLASH)
scancodeNonUSHash = (#const SDL_SCANCODE_NONUSHASH)
scancodeSemicolon = (#const SDL_SCANCODE_SEMICOLON)
scancodeApostrophe = (#const SDL_SCANCODE_APOSTROPHE)
scancodeGrave = (#const SDL_SCANCODE_GRAVE)
scancodeComma = (#const SDL_SCANCODE_COMMA)
scancodePeriod = (#const SDL_SCANCODE_PERIOD)
scancodeSlash = (#const SDL_SCANCODE_SLASH)
scancodeCapsLock = (#const SDL_SCANCODE_CAPSLOCK)
scancodeF1 = (#const SDL_SCANCODE_F1)
scancodeF2 = (#const SDL_SCANCODE_F2)
scancodeF3 = (#const SDL_SCANCODE_F3)
scancodeF4 = (#const SDL_SCANCODE_F4)
scancodeF5 = (#const SDL_SCANCODE_F5)
scancodeF6 = (#const SDL_SCANCODE_F6)
scancodeF7 = (#const SDL_SCANCODE_F7)
scancodeF8 = (#const SDL_SCANCODE_F8)
scancodeF9 = (#const SDL_SCANCODE_F9)
scancodeF10 = (#const SDL_SCANCODE_F10)
scancodeF11 = (#const SDL_SCANCODE_F11)
scancodeF12 = (#const SDL_SCANCODE_F12)
scancodePrintScreen = (#const SDL_SCANCODE_PRINTSCREEN)
scancodeScrollLock = (#const SDL_SCANCODE_SCROLLLOCK)
scancodePause = (#const SDL_SCANCODE_PAUSE)
scancodeInsert = (#const SDL_SCANCODE_INSERT)
scancodeHome = (#const SDL_SCANCODE_HOME)
scancodePageUp = (#const SDL_SCANCODE_PAGEUP)
scancodeDelete = (#const SDL_SCANCODE_DELETE)
scancodeEnd = (#const SDL_SCANCODE_END)
scancodePageDown = (#const SDL_SCANCODE_PAGEDOWN)
scancodeRight = (#const SDL_SCANCODE_RIGHT)
scancodeLeft = (#const SDL_SCANCODE_LEFT)
scancodeDown = (#const SDL_SCANCODE_DOWN)
scancodeUp = (#const SDL_SCANCODE_UP)
scancodeNumLockClear = (#const SDL_SCANCODE_NUMLOCKCLEAR)
scancodeKPDivide = (#const SDL_SCANCODE_KP_DIVIDE)
scancodeKPMultiply = (#const SDL_SCANCODE_KP_MULTIPLY)
scancodeKPMinus = (#const SDL_SCANCODE_KP_MINUS)
scancodeKPPlus = (#const SDL_SCANCODE_KP_PLUS)
scancodeKPEnter = (#const SDL_SCANCODE_KP_ENTER)
scancodeKP1 = (#const SDL_SCANCODE_KP_1)
scancodeKP2 = (#const SDL_SCANCODE_KP_2)
scancodeKP3 = (#const SDL_SCANCODE_KP_3)
scancodeKP4 = (#const SDL_SCANCODE_KP_4)
scancodeKP5 = (#const SDL_SCANCODE_KP_5)
scancodeKP6 = (#const SDL_SCANCODE_KP_6)
scancodeKP7 = (#const SDL_SCANCODE_KP_7)
scancodeKP8 = (#const SDL_SCANCODE_KP_8)
scancodeKP9 = (#const SDL_SCANCODE_KP_9)
scancodeKP0 = (#const SDL_SCANCODE_KP_0)
scancodeKPPeriod = (#const SDL_SCANCODE_KP_PERIOD)
scancodeNonUSBackslash = (#const SDL_SCANCODE_NONUSBACKSLASH)
scancodeApplication = (#const SDL_SCANCODE_APPLICATION)
scancodePower = (#const SDL_SCANCODE_POWER)
scancodeKPEquals = (#const SDL_SCANCODE_KP_EQUALS)
scancodeF13 = (#const SDL_SCANCODE_F13)
scancodeF14 = (#const SDL_SCANCODE_F14)
scancodeF15 = (#const SDL_SCANCODE_F15)
scancodeF16 = (#const SDL_SCANCODE_F16)
scancodeF17 = (#const SDL_SCANCODE_F17)
scancodeF18 = (#const SDL_SCANCODE_F18)
scancodeF19 = (#const SDL_SCANCODE_F19)
scancodeF20 = (#const SDL_SCANCODE_F20)
scancodeF21 = (#const SDL_SCANCODE_F21)
scancodeF22 = (#const SDL_SCANCODE_F22)
scancodeF23 = (#const SDL_SCANCODE_F23)
scancodeF24 = (#const SDL_SCANCODE_F24)
scancodeExecute = (#const SDL_SCANCODE_EXECUTE)
scancodeHelp = (#const SDL_SCANCODE_HELP)
scancodeMenu = (#const SDL_SCANCODE_MENU)
scancodeSelect = (#const SDL_SCANCODE_SELECT)
scancodeStop = (#const SDL_SCANCODE_STOP)
scancodeAgain = (#const SDL_SCANCODE_AGAIN)
scancodeUndo = (#const SDL_SCANCODE_UNDO)
scancodeCut = (#const SDL_SCANCODE_CUT)
scancodeCopy = (#const SDL_SCANCODE_COPY)
scancodePaste = (#const SDL_SCANCODE_PASTE)
scancodeFind = (#const SDL_SCANCODE_FIND)
scancodeMute = (#const SDL_SCANCODE_MUTE)
scancodeVolumeUp = (#const SDL_SCANCODE_VOLUMEUP)
scancodeVolumeDown = (#const SDL_SCANCODE_VOLUMEDOWN)
scancodeKPComma = (#const SDL_SCANCODE_KP_COMMA)
scancodeEqualsAs400 = (#const SDL_SCANCODE_KP_EQUALSAS400)
scancodeInternational1 = (#const SDL_SCANCODE_INTERNATIONAL1)
scancodeInternational2 = (#const SDL_SCANCODE_INTERNATIONAL2)
scancodeInternational3 = (#const SDL_SCANCODE_INTERNATIONAL3)
scancodeInternational4 = (#const SDL_SCANCODE_INTERNATIONAL4)
scancodeInternational5 = (#const SDL_SCANCODE_INTERNATIONAL5)
scancodeInternational6 = (#const SDL_SCANCODE_INTERNATIONAL6)
scancodeInternational7 = (#const SDL_SCANCODE_INTERNATIONAL7)
scancodeInternational8 = (#const SDL_SCANCODE_INTERNATIONAL8)
scancodeInternational9 = (#const SDL_SCANCODE_INTERNATIONAL9)
scancodeLang1 = (#const SDL_SCANCODE_LANG1)
scancodeLang2 = (#const SDL_SCANCODE_LANG2)
scancodeLang3 = (#const SDL_SCANCODE_LANG3)
scancodeLang4 = (#const SDL_SCANCODE_LANG4)
scancodeLang5 = (#const SDL_SCANCODE_LANG5)
scancodeLang6 = (#const SDL_SCANCODE_LANG6)
scancodeLang7 = (#const SDL_SCANCODE_LANG7)
scancodeLang8 = (#const SDL_SCANCODE_LANG8)
scancodeLang9 = (#const SDL_SCANCODE_LANG9)
scancodeAltErase = (#const SDL_SCANCODE_ALTERASE)
scancodeSysReq = (#const SDL_SCANCODE_SYSREQ)
scancodeCancel = (#const SDL_SCANCODE_CANCEL)
scancodeClear = (#const SDL_SCANCODE_CLEAR)
scancodePrior = (#const SDL_SCANCODE_PRIOR)
scancodeReturn2 = (#const SDL_SCANCODE_RETURN2)
scancodeSeparator = (#const SDL_SCANCODE_SEPARATOR)
scancodeOut = (#const SDL_SCANCODE_OUT)
scancodeOper = (#const SDL_SCANCODE_OPER)
scancodeClearAgain = (#const SDL_SCANCODE_CLEARAGAIN)
scancodeCrSel = (#const SDL_SCANCODE_CRSEL)
scancodeExSel = (#const SDL_SCANCODE_EXSEL)
scancodeKP00 = (#const SDL_SCANCODE_KP_00)
scancodeKP000 = (#const SDL_SCANCODE_KP_000)
scancodeThousandsSeparator = (#const SDL_SCANCODE_THOUSANDSSEPARATOR)
scancodeDecimalSeparator = (#const SDL_SCANCODE_DECIMALSEPARATOR)
scancodeCurrencyUnit = (#const SDL_SCANCODE_CURRENCYUNIT)
scancodeCurrencySubunit = (#const SDL_SCANCODE_CURRENCYSUBUNIT)
scancodeLeftParen = (#const SDL_SCANCODE_KP_LEFTPAREN)
scancodeRightParen = (#const SDL_SCANCODE_KP_RIGHTPAREN)
scancodeLeftBrace = (#const SDL_SCANCODE_KP_LEFTBRACE)
scancodeRightBrace = (#const SDL_SCANCODE_KP_RIGHTBRACE)
scancodeKPTab = (#const SDL_SCANCODE_KP_TAB)
scancodeKPBackspace = (#const SDL_SCANCODE_KP_BACKSPACE)
scancodeKPA = (#const SDL_SCANCODE_KP_A)
scancodeKPB = (#const SDL_SCANCODE_KP_B)
scancodeKPC = (#const SDL_SCANCODE_KP_C)
scancodeKPD = (#const SDL_SCANCODE_KP_D)
scancodeKPE = (#const SDL_SCANCODE_KP_E)
scancodeKPF = (#const SDL_SCANCODE_KP_F)
scancodeKPXOR = (#const SDL_SCANCODE_KP_XOR)
scancodeKPPower = (#const SDL_SCANCODE_KP_POWER)
scancodeKPPercent = (#const SDL_SCANCODE_KP_PERCENT)
scancodeKPLess = (#const SDL_SCANCODE_KP_LESS)
scancodeKPGreater = (#const SDL_SCANCODE_KP_GREATER)
scancodeKPAmpersand = (#const SDL_SCANCODE_KP_AMPERSAND)
scancodeKPDBLAmpersand = (#const SDL_SCANCODE_KP_DBLAMPERSAND)
scancodeKPVerticalBar = (#const SDL_SCANCODE_KP_VERTICALBAR)
scancodeKPDBLVerticalBar = (#const SDL_SCANCODE_KP_DBLVERTICALBAR)
scancodeKPColon = (#const SDL_SCANCODE_KP_COLON)
scancodeKPHash = (#const SDL_SCANCODE_KP_HASH)
scancodeKPSpace = (#const SDL_SCANCODE_KP_SPACE)
scancodeKPAt = (#const SDL_SCANCODE_KP_AT)
scancodeKPExclam = (#const SDL_SCANCODE_KP_EXCLAM)
scancodeKPMemStore = (#const SDL_SCANCODE_KP_MEMSTORE)
scancodeKPMemRecall = (#const SDL_SCANCODE_KP_MEMRECALL)
scancodeKPMemClear = (#const SDL_SCANCODE_KP_MEMCLEAR)
scancodeKPMemAdd = (#const SDL_SCANCODE_KP_MEMADD)
scancodeKPMemSubtract = (#const SDL_SCANCODE_KP_MEMSUBTRACT)
scancodeKPMemMultiply = (#const SDL_SCANCODE_KP_MEMMULTIPLY)
scancodeKPMemDivide = (#const SDL_SCANCODE_KP_MEMDIVIDE)
scancodeKPPlusMinus = (#const SDL_SCANCODE_KP_PLUSMINUS)
scancodeKPClear = (#const SDL_SCANCODE_KP_CLEAR)
scancodeKPClearEntry = (#const SDL_SCANCODE_KP_CLEARENTRY)
scancodeKPBinary = (#const SDL_SCANCODE_KP_BINARY)
scancodeKPOctal = (#const SDL_SCANCODE_KP_OCTAL)
scancodeKPDecimal = (#const SDL_SCANCODE_KP_DECIMAL)
scancodeKPHexadecimal = (#const SDL_SCANCODE_KP_HEXADECIMAL)
scancodeLCtrl = (#const SDL_SCANCODE_LCTRL)
scancodeLShift = (#const SDL_SCANCODE_LSHIFT)
scancodeLAlt = (#const SDL_SCANCODE_LALT)
scancodeLGUI = (#const SDL_SCANCODE_LGUI)
scancodeRCtrl = (#const SDL_SCANCODE_RCTRL)
scancodeRShift = (#const SDL_SCANCODE_RSHIFT)
scancodeRAlt = (#const SDL_SCANCODE_RALT)
scancodeRGUI = (#const SDL_SCANCODE_RGUI)
scancodeMode = (#const SDL_SCANCODE_MODE)
scancodeAudioNext = (#const SDL_SCANCODE_AUDIONEXT)
scancodeAudioPrev = (#const SDL_SCANCODE_AUDIOPREV)
scancodeAudioStop = (#const SDL_SCANCODE_AUDIOSTOP)
scancodeAudioPlay = (#const SDL_SCANCODE_AUDIOPLAY)
scancodeAudioMute = (#const SDL_SCANCODE_AUDIOMUTE)
scancodeMediaSelect = (#const SDL_SCANCODE_MEDIASELECT)
scancodeWWW = (#const SDL_SCANCODE_WWW)
scancodeMail = (#const SDL_SCANCODE_MAIL)
scancodeCalculator = (#const SDL_SCANCODE_CALCULATOR)
scancodeComputer = (#const SDL_SCANCODE_COMPUTER)
scancodeACSearch = (#const SDL_SCANCODE_AC_SEARCH)
scancodeACHome = (#const SDL_SCANCODE_AC_HOME)
scancodeACBack = (#const SDL_SCANCODE_AC_BACK)
scancodeACForward = (#const SDL_SCANCODE_AC_FORWARD)
scancodeACStop = (#const SDL_SCANCODE_AC_STOP)
scancodeACRefresh = (#const SDL_SCANCODE_AC_REFRESH)
scancodeACBookmarks = (#const SDL_SCANCODE_AC_BOOKMARKS)
scancodeBrightnessDown = (#const SDL_SCANCODE_BRIGHTNESSDOWN)
scancodeBrightnessUp = (#const SDL_SCANCODE_BRIGHTNESSUP)
scancodeDisplaySwitch = (#const SDL_SCANCODE_DISPLAYSWITCH)
scancodeKBDIllumToggle = (#const SDL_SCANCODE_KBDILLUMTOGGLE)
scancodeKBDIllumDown = (#const SDL_SCANCODE_KBDILLUMDOWN)
scancodeKBDIllumUp = (#const SDL_SCANCODE_KBDILLUMUP)
scancodeEject = (#const SDL_SCANCODE_EJECT)
scancodeSleep = (#const SDL_SCANCODE_SLEEP)
scancodeApp1 = (#const SDL_SCANCODE_APP1)
scancodeApp2 = (#const SDL_SCANCODE_APP2)
scancodeNum = (#const SDL_NUM_SCANCODES)

type SystemCursor = (#type SDL_SystemCursor)

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

systemCursorArrow = (#const SDL_SYSTEM_CURSOR_ARROW)
systemCursorIBeam = (#const SDL_SYSTEM_CURSOR_IBEAM)
systemCursorWait = (#const SDL_SYSTEM_CURSOR_WAIT)
systemCursorCrosshair = (#const SDL_SYSTEM_CURSOR_CROSSHAIR)
systemCursorWaitArrow = (#const SDL_SYSTEM_CURSOR_WAITARROW)
systemCursorSizeNWSE = (#const SDL_SYSTEM_CURSOR_SIZENWSE)
systemCursorSizeNESW = (#const SDL_SYSTEM_CURSOR_SIZENESW)
systemCursorSizeWE = (#const SDL_SYSTEM_CURSOR_SIZEWE)
systemCursorSizeNS = (#const SDL_SYSTEM_CURSOR_SIZENS)
systemCursorSizeAll = (#const SDL_SYSTEM_CURSOR_SIZEALL)
systemCursorNo = (#const SDL_SYSTEM_CURSOR_NO)
systemCursorHand = (#const SDL_SYSTEM_CURSOR_HAND)
systemCursorNum = (#const SDL_NUM_SYSTEM_CURSORS)

type ThreadPriority = (#type SDL_ThreadPriority)

threadPriorityLow :: ThreadPriority
threadPriorityNormal :: ThreadPriority
threadPriorityHigh :: ThreadPriority

threadPriorityLow = (#const SDL_THREAD_PRIORITY_LOW)
threadPriorityNormal = (#const SDL_THREAD_PRIORITY_NORMAL)
threadPriorityHigh = (#const SDL_THREAD_PRIORITY_HIGH)

audioAllowFrequencyChange :: Num a => a
audioAllowFormatChange :: Num a => a
audioAllowChannelsChange :: Num a => a
audioAllowAnyChange :: Num a => a

audioAllowFrequencyChange = (#const SDL_AUDIO_ALLOW_FREQUENCY_CHANGE)
audioAllowFormatChange = (#const SDL_AUDIO_ALLOW_FORMAT_CHANGE)
audioAllowChannelsChange = (#const SDL_AUDIO_ALLOW_CHANNELS_CHANGE)
audioAllowAnyChange = (#const SDL_AUDIO_ALLOW_ANY_CHANGE)

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

buttonLeft = (#const SDL_BUTTON_LEFT)
buttonMiddle = (#const SDL_BUTTON_MIDDLE)
buttonRight = (#const SDL_BUTTON_RIGHT)
buttonX1 = (#const SDL_BUTTON_X1)
buttonX2 = (#const SDL_BUTTON_X2)
buttonLMask = (#const SDL_BUTTON_LMASK)
buttonMMask = (#const SDL_BUTTON_MMASK)
buttonRMask = (#const SDL_BUTTON_RMASK)
buttonX1Mask = (#const SDL_BUTTON_X1MASK)
buttonX2Mask = (#const SDL_BUTTON_X2MASK)

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

eventTypeFirstEvent = (#const SDL_FIRSTEVENT)
eventTypeQuit = (#const SDL_QUIT)
eventTypeAppTerminating = (#const SDL_APP_TERMINATING)
eventTypeAppLowMemory = (#const SDL_APP_LOWMEMORY)
eventTypeAppWillEnterBackground = (#const SDL_APP_WILLENTERBACKGROUND)
eventTypeAppDidEnterBackground = (#const SDL_APP_DIDENTERBACKGROUND)
eventTypeAppWillEnterForeground = (#const SDL_APP_WILLENTERFOREGROUND)
eventTypeAppDidEnterForeground = (#const SDL_APP_DIDENTERFOREGROUND)
eventTypeWindowEvent = (#const SDL_WINDOWEVENT)
eventTypeSysWMEvent = (#const SDL_SYSWMEVENT)
eventTypeKeyDown = (#const SDL_KEYDOWN)
eventTypeKeyUp = (#const SDL_KEYUP)
eventTypeTextEditing = (#const SDL_TEXTEDITING)
eventTypeTextInput = (#const SDL_TEXTINPUT)
eventTypeMouseMotion = (#const SDL_MOUSEMOTION)
eventTypeMouseButtonDown = (#const SDL_MOUSEBUTTONDOWN)
eventTypeMouseButtonUp = (#const SDL_MOUSEBUTTONUP)
eventTypeMouseWheel = (#const SDL_MOUSEWHEEL)
eventTypeJoyAxisMotion = (#const SDL_JOYAXISMOTION)
eventTypeJoyBallMotion = (#const SDL_JOYBALLMOTION)
eventTypeJoyHatMotion = (#const SDL_JOYHATMOTION)
eventTypeJoyButtonDown = (#const SDL_JOYBUTTONDOWN)
eventTypeJoyButtonUp = (#const SDL_JOYBUTTONUP)
eventTypeJoyDeviceAdded = (#const SDL_JOYDEVICEADDED)
eventTypeJoyDeviceRemoved = (#const SDL_JOYDEVICEREMOVED)
eventTypeControllerAxisMotion = (#const SDL_CONTROLLERAXISMOTION)
eventTypeControllerButtonDown = (#const SDL_CONTROLLERBUTTONDOWN)
eventTypeControllerButtonUp = (#const SDL_CONTROLLERBUTTONUP)
eventTypeControllerDeviceAdded = (#const SDL_CONTROLLERDEVICEADDED)
eventTypeControllerDeviceRemoved = (#const SDL_CONTROLLERDEVICEREMOVED)
eventTypeControllerDeviceRemapped = (#const SDL_CONTROLLERDEVICEREMAPPED)
eventTypeFingerDown = (#const SDL_FINGERDOWN)
eventTypeFingerUp = (#const SDL_FINGERUP)
eventTypeFingerMotion = (#const SDL_FINGERMOTION)
eventTypeDollarGesture = (#const SDL_DOLLARGESTURE)
eventTypeDollarRecord = (#const SDL_DOLLARRECORD)
eventTypeMultiGesture = (#const SDL_MULTIGESTURE)
eventTypeClipboardUpdate = (#const SDL_CLIPBOARDUPDATE)
eventTypeDropFile = (#const SDL_DROPFILE)
eventTypeUserEvent = (#const SDL_USEREVENT)
eventTypeLastEvent = (#const SDL_LASTEVENT)

joystickHatCentered :: (Num a) => a
joystickHatUp :: (Num a) => a
joystickHatRight :: (Num a) => a
joystickHatDown :: (Num a) => a
joystickHatLeft :: (Num a) => a
joystickHatRightUp :: (Num a) => a
joystickHatRightDown :: (Num a) => a
joystickHatLeftUp :: (Num a) => a
joystickHatLeftDown :: (Num a) => a

joystickHatCentered = (#const SDL_HAT_CENTERED)
joystickHatUp = (#const SDL_HAT_UP)
joystickHatRight = (#const SDL_HAT_RIGHT)
joystickHatDown = (#const SDL_HAT_DOWN)
joystickHatLeft = (#const SDL_HAT_LEFT)
joystickHatRightUp = (#const SDL_HAT_RIGHTUP)
joystickHatRightDown = (#const SDL_HAT_RIGHTDOWN)
joystickHatLeftUp = (#const SDL_HAT_LEFTUP)
joystickHatLeftDown = (#const SDL_HAT_LEFTDOWN)

keyPressed :: (Num a) => a
keyReleased :: (Num a) => a

keyPressed = (#const SDL_PRESSED)
keyReleased = (#const SDL_RELEASED)

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

logCategoryApplication = (#const SDL_LOG_CATEGORY_APPLICATION)
logCategoryError = (#const SDL_LOG_CATEGORY_ERROR)
logCategoryAssert = (#const SDL_LOG_CATEGORY_ASSERT)
logCategorySystem = (#const SDL_LOG_CATEGORY_SYSTEM)
logCategoryAudio = (#const SDL_LOG_CATEGORY_AUDIO)
logCategoryVideo = (#const SDL_LOG_CATEGORY_VIDEO)
logCategoryRender = (#const SDL_LOG_CATEGORY_RENDER)
logCategoryInput = (#const SDL_LOG_CATEGORY_INPUT)
logCategoryTest = (#const SDL_LOG_CATEGORY_TEST)
logCategoryCustom = (#const SDL_LOG_CATEGORY_CUSTOM)

messageBoxFlagError :: (Num a) => a
messageBoxFlagWarning :: (Num a) => a
messageBoxFlagInformation :: (Num a) => a

messageBoxFlagError = (#const SDL_MESSAGEBOX_ERROR)
messageBoxFlagWarning = (#const SDL_MESSAGEBOX_WARNING)
messageBoxFlagInformation = (#const SDL_MESSAGEBOX_INFORMATION)

messageBoxButtonFlagReturnKeyDefault :: (Num a) => a
messageBoxButtonFlagEscapeKeyDefault :: (Num a) => a

messageBoxButtonFlagReturnKeyDefault = (#const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT)
messageBoxButtonFlagEscapeKeyDefault = (#const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT)

glProfileCore :: (Num a) => a
glProfileCompatibility :: (Num a) => a
glProfileES :: (Num a) => a

glProfileCore = (#const SDL_GL_CONTEXT_PROFILE_CORE)
glProfileCompatibility = (#const SDL_GL_CONTEXT_PROFILE_COMPATIBILITY)
glProfileES = (#const SDL_GL_CONTEXT_PROFILE_ES)

glContextFlagDebug :: (Num a) => a
glContextFlagForwardCompatible :: (Num a) => a
glContextFlagRobustAccess :: (Num a) => a
glContextFlagResetIsolation :: (Num a) => a

glContextFlagDebug = (#const SDL_GL_CONTEXT_DEBUG_FLAG)
glContextFlagForwardCompatible = (#const SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG)
glContextFlagRobustAccess = (#const SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG)
glContextFlagResetIsolation = (#const SDL_GL_CONTEXT_RESET_ISOLATION_FLAG)

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

pixelFormatUnknown = (#const SDL_PIXELFORMAT_UNKNOWN)
pixelFormatIndex1LSB = (#const SDL_PIXELFORMAT_INDEX1LSB)
pixelFormatIndex1MSB = (#const SDL_PIXELFORMAT_INDEX1MSB)
pixelFormatIndex4LSB = (#const SDL_PIXELFORMAT_INDEX4LSB)
pixelFormatIndex4MSB = (#const SDL_PIXELFORMAT_INDEX4MSB)
pixelFormatIndex8 = (#const SDL_PIXELFORMAT_INDEX8)
pixelFormatRGB332 = (#const SDL_PIXELFORMAT_RGB332)
pixelFormatRGB444 = (#const SDL_PIXELFORMAT_RGB444)
pixelFormatRGB555 = (#const SDL_PIXELFORMAT_RGB555)
pixelFormatBGR555 = (#const SDL_PIXELFORMAT_BGR555)
pixelFormatARGB4444 = (#const SDL_PIXELFORMAT_ARGB4444)
pixelFormatRGBA4444 = (#const SDL_PIXELFORMAT_RGBA4444)
pixelFormatABGR4444 = (#const SDL_PIXELFORMAT_ABGR4444)
pixelFormatBGRA4444 = (#const SDL_PIXELFORMAT_BGRA4444)
pixelFormatARGB1555 = (#const SDL_PIXELFORMAT_ARGB1555)
pixelFormatRGBA5551 = (#const SDL_PIXELFORMAT_RGBA5551)
pixelFormatABGR1555 = (#const SDL_PIXELFORMAT_ABGR1555)
pixelFormatBGRA5551 = (#const SDL_PIXELFORMAT_BGRA5551)
pixelFormatRGB565 = (#const SDL_PIXELFORMAT_RGB565)
pixelFormatBGR565 = (#const SDL_PIXELFORMAT_BGR565)
pixelFormatRGB24 = (#const SDL_PIXELFORMAT_RGB24)
pixelFormatBGR24 = (#const SDL_PIXELFORMAT_BGR24)
pixelFormatRGB888 = (#const SDL_PIXELFORMAT_RGB888)
pixelFormatRGBX8888 = (#const SDL_PIXELFORMAT_RGBX8888)
pixelFormatBGR888 = (#const SDL_PIXELFORMAT_BGR888)
pixelFormatBGRX8888 = (#const SDL_PIXELFORMAT_BGRX8888)
pixelFormatARGB8888 = (#const SDL_PIXELFORMAT_ARGB8888)
pixelFormatRGBA8888 = (#const SDL_PIXELFORMAT_RGBA8888)
pixelFormatABGR8888 = (#const SDL_PIXELFORMAT_ABGR8888)
pixelFormatBGRA8888 = (#const SDL_PIXELFORMAT_BGRA8888)
pixelFormatARGB2101010 = (#const SDL_PIXELFORMAT_ARGB2101010)
pixelFormatYV12 = (#const SDL_PIXELFORMAT_YV12)
pixelFormatIYUV = (#const SDL_PIXELFORMAT_IYUV)
pixelFormatYUY2 = (#const SDL_PIXELFORMAT_YUY2)
pixelFormatUYVY = (#const SDL_PIXELFORMAT_UYVY)
pixelFormatYVYU = (#const SDL_PIXELFORMAT_YVYU)

rendererFlagSoftware :: (Num a) => a
rendererFlagAccelerated :: (Num a) => a
rendererFlagPresentVSync :: (Num a) => a
rendererFlagTargetTexture :: (Num a) => a

rendererFlagSoftware = (#const SDL_RENDERER_SOFTWARE)
rendererFlagAccelerated = (#const SDL_RENDERER_ACCELERATED)
rendererFlagPresentVSync = (#const SDL_RENDERER_PRESENTVSYNC)
rendererFlagTargetTexture = (#const SDL_RENDERER_TARGETTEXTURE)

textureAccessStatic :: (Num a) => a
textureAccessStreaming :: (Num a) => a
textureAccessTarget :: (Num a) => a

textureAccessStatic = (#const SDL_TEXTUREACCESS_STATIC)
textureAccessStreaming = (#const SDL_TEXTUREACCESS_STREAMING)
textureAccessTarget = (#const SDL_TEXTUREACCESS_TARGET)

textureModulateNone :: (Num a) => a
textureModulateColor :: (Num a) => a
textureModulateAlpha :: (Num a) => a

textureModulateNone = (#const SDL_TEXTUREMODULATE_NONE)
textureModulateColor = (#const SDL_TEXTUREMODULATE_COLOR)
textureModulateAlpha = (#const SDL_TEXTUREMODULATE_ALPHA)

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

windowEventNone = (#const SDL_WINDOWEVENT_NONE)
windowEventShown = (#const SDL_WINDOWEVENT_SHOWN)
windowEventHidden = (#const SDL_WINDOWEVENT_HIDDEN)
windowEventExposed = (#const SDL_WINDOWEVENT_EXPOSED)
windowEventMoved = (#const SDL_WINDOWEVENT_MOVED)
windowEventResized = (#const SDL_WINDOWEVENT_RESIZED)
windowEventSizeChanged = (#const SDL_WINDOWEVENT_SIZE_CHANGED)
windowEventMinimized = (#const SDL_WINDOWEVENT_MINIMIZED)
windowEventMaximized = (#const SDL_WINDOWEVENT_MAXIMIZED)
windowEventRestored = (#const SDL_WINDOWEVENT_RESTORED)
windowEventEnter = (#const SDL_WINDOWEVENT_ENTER)
windowEventLeave = (#const SDL_WINDOWEVENT_LEAVE)
windowEventFocusGained = (#const SDL_WINDOWEVENT_FOCUS_GAINED)
windowEventFocusLost = (#const SDL_WINDOWEVENT_FOCUS_LOST)
windowEventClose = (#const SDL_WINDOWEVENT_CLOSE)

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

windowFlagFullscreen = (#const SDL_WINDOW_FULLSCREEN)
windowFlagOpenGL = (#const SDL_WINDOW_OPENGL)
windowFlagShown = (#const SDL_WINDOW_SHOWN)
windowFlagHidden = (#const SDL_WINDOW_HIDDEN)
windowFlagBorderless = (#const SDL_WINDOW_BORDERLESS)
windowFlagResizable = (#const SDL_WINDOW_RESIZABLE)
windowFlagMinimized = (#const SDL_WINDOW_MINIMIZED)
windowFlagMaximized = (#const SDL_WINDOW_MAXIMIZED)
windowFlagInputGrabbed = (#const SDL_WINDOW_INPUT_GRABBED)
windowFlagInputFocus = (#const SDL_WINDOW_INPUT_FOCUS)
windowFlagMouseFocus = (#const SDL_WINDOW_MOUSE_FOCUS)
windowFlagFullscreenDesktop = (#const SDL_WINDOW_FULLSCREEN_DESKTOP)
windowFlagForeign = (#const SDL_WINDOW_FOREIGN)
windowFlagAllowHighDPI = (#const SDL_WINDOW_ALLOW_HIGHDPI)

windowPosUndefined :: (Num a) => a
windowPosCentered :: (Num a) => a

windowPosUndefined = (#const SDL_WINDOWPOS_UNDEFINED)
windowPosCentered = (#const SDL_WINDOWPOS_CENTERED)
