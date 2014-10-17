{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.UI.SDL.Enum.Pattern (
	-- * Enumerations

	-- ** Audio Status
	AudioStatus,
	pattern AudioStatusStopped,
	pattern AudioStatusPlaying,
	pattern AudioStatusPaused,

	-- ** Blend Mode
	BlendMode,
	pattern BlendModeNone,
	pattern BlendModeBlend,
	pattern BlendModeAdd,
	pattern BlendModeMod,

	-- ** Event Action
	EventAction,
	pattern EventActionAddEvent,
	pattern EventActionPeekEvent,
	pattern EventActionGetEvent,

	-- ** Game Controller Axis
	GameControllerAxis,
	pattern GameControllerAxisInvalid,
	pattern GameControllerAxisLeftX,
	pattern GameControllerAxisLeftY,
	pattern GameControllerAxisRightX,
	pattern GameControllerAxisRightY,
	pattern GameControllerAxisTriggerLeft,
	pattern GameControllerAxisTriggerRight,
	pattern GameControllerAxisMax,

	-- ** Game Controller Button
	GameControllerButton,
	pattern GameControllerButtonInvalid,
	pattern GameControllerButtonA,
	pattern GameControllerButtonB,
	pattern GameControllerButtonX,
	pattern GameControllerButtonY,
	pattern GameControllerButtonBack,
	pattern GameControllerButtonGuide,
	pattern GameControllerButtonStart,
	pattern GameControllerButtonLeftStick,
	pattern GameControllerButtonRightStick,
	pattern GameControllerButtonLeftShoulder,
	pattern GameControllerButtonRightShoulder,
	pattern GameControllerButtonDPadUp,
	pattern GameControllerButtonDPadDown,
	pattern GameControllerButtonDPadLeft,
	pattern GameControllerButtonDPadRight,
	pattern GameControllerButtonMax,

	-- ** OpenGL Attribute
	GLattr,
	pattern GLAttrRedSize,
	pattern GLAttrGreenSize,
	pattern GLAttrBlueSize,
	pattern GLAttrAlphaSize,
	pattern GLAttrBufferSize,
	pattern GLAttrDoubleBuffer,
	pattern GLAttrDepthSize,
	pattern GLAttrStencilSize,
	pattern GLAttrAccumRedSize,
	pattern GLAttrAccumGreenSize,
	pattern GLAttrAccumBlueSize,
	pattern GLAttrAccumAlphaSize,
	pattern GLAttrStereo,
	pattern GLAttrMultiSampleBuffers,
	pattern GLAttrMultiSampleSamples,
	pattern GLAttrAcceleratedVisual,
	pattern GLAttrRetainedBacking,
	pattern GLAttrContextMajorVersion,
	pattern GLAttrContextMinorVersion,
	pattern GLAttrContextEGL,
	pattern GLAttrContextFlags,
	pattern GLAttrContextProfileMask,
	pattern GLAttrShareWithCurrentContext,
	pattern GLAttrFramebufferSRGBCapable,

	-- ** Hint Priority
	HintPriority,
	pattern HintPriorityDefault,
	pattern HintPriorityNormal,
	pattern HintPriorityOverride,

	-- ** Keycode
	Keycode,
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
	pattern KeycodeKPVecticalBar,
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
	pattern KeycodeSleep,

	-- ** Key Modifier
	Keymod,
	pattern KeymodNone,
	pattern KeymodLShift,
	pattern KeymodRShift,
	pattern KeymodLCtrl,
	pattern KeymodRCtrl,
	pattern KeymodLAlt,
	pattern KeymodRAlt,
	pattern KeymodLGUI,
	pattern KeymodRGUI,
	pattern KeymodNum,
	pattern KeymodCaps,
	pattern KeymodMode,
	pattern KeymodReserved,

	-- ** Log Priority
	LogPriority,
	pattern LogPriorityVerbose,
	pattern LogPriorityDebug,
	pattern LogPriorityInfo,
	pattern LogPriorityWarn,
	pattern LogPriorityError,
	pattern LogPriorityCritical,
	pattern LogPriorityPriorities,

	-- ** Power State
	PowerState,
	pattern PowerStateUnknown,
	pattern PowerStateOnBattery,
	pattern PowerStateNoBattery,
	pattern PowerStateCharging,
	pattern PowerStateCharged,

	-- ** Renderer Flip
	RendererFlip,
	pattern RendererFlipNone,
	pattern RendererFlipHorizontal,
	pattern RendererFlipVertical,

	-- ** Scancode
	Scancode,
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
	pattern ScancodeEqualsAs400,
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
	pattern ScancodeKPDBLAmpersand,
	pattern ScancodeKPVerticalBar,
	pattern ScancodeKPDBLVerticalBar,
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
	pattern ScancodeNum,

	-- ** System Cursor
	SystemCursor,
	pattern SystemCursorArrow,
	pattern SystemCursorIBeam,
	pattern SystemCursorWait,
	pattern SystemCursorCrosshair,
	pattern SystemCursorWaitArrow,
	pattern SystemCursorSizeNWSE,
	pattern SystemCursorSizeNESW,
	pattern SystemCursorSizeWE,
	pattern SystemCursorSizeNS,
	pattern SystemCursorSizeAll,
	pattern SystemCursorNo,
	pattern SystemCursorHand,
	pattern SystemCursorNum,

	-- ** Thread Priority
	ThreadPriority,
	pattern ThreadPriorityLow,
	pattern ThreadPriorityNormal,
	pattern ThreadPriorityHigh,

	-- * Miscellaneous Enumerations
	-- | These enumerations are not used directly by any SDL function, thus they have a polymorphic type.

	-- ** Audio Allowed Changes
	pattern AudioAllowFrequencyChange,
	pattern AudioAllowFormatChange,
	pattern AudioAllowChannelsChange,
	pattern AudioAllowAnyChange,

	-- ** Button
	pattern ButtonLeft,
	pattern ButtonMiddle,
	pattern ButtonRight,
	pattern ButtonX1,
	pattern ButtonX2,
	pattern ButtonLMask,
	pattern ButtonMMask,
	pattern ButtonRMask,
	pattern ButtonX1Mask,
	pattern ButtonX2Mask,

	-- ** Event Type
	pattern EventTypeFirstEvent,
	pattern EventTypeQuit,
	pattern EventTypeAppTerminating,
	pattern EventTypeAppLowMemory,
	pattern EventTypeAppWillEnterBackground,
	pattern EventTypeAppDidEnterBackground,
	pattern EventTypeAppWillEnterForeground,
	pattern EventTypeAppDidEnterForeground,
	pattern EventTypeWindowEvent,
	pattern EventTypeSysWMEvent,
	pattern EventTypeKeyDown,
	pattern EventTypeKeyUp,
	pattern EventTypeTextEditing,
	pattern EventTypeTextInput,
	pattern EventTypeMouseMotion,
	pattern EventTypeMouseButtonDown,
	pattern EventTypeMouseButtonUp,
	pattern EventTypeMouseWheel,
	pattern EventTypeJoyAxisMotion,
	pattern EventTypeJoyBallMotion,
	pattern EventTypeJoyHatMotion,
	pattern EventTypeJoyButtonDown,
	pattern EventTypeJoyButtonUp,
	pattern EventTypeJoyDeviceAdded,
	pattern EventTypeJoyDeviceRemoved,
	pattern EventTypeControllerAxisMotion,
	pattern EventTypeControllerButtonDown,
	pattern EventTypeControllerButtonUp,
	pattern EventTypeControllerDeviceAdded,
	pattern EventTypeControllerDeviceRemoved,
	pattern EventTypeControllerDeviceRemapped,
	pattern EventTypeFingerDown,
	pattern EventTypeFingerUp,
	pattern EventTypeFingerMotion,
	pattern EventTypeDollarGesture,
	pattern EventTypeDollarRecord,
	pattern EventTypeMultiGesture,
	pattern EventTypeClipboardUpdate,
	pattern EventTypeDropFile,
	pattern EventTypeUserEvent,
	pattern EventTypeLastEvent,

	-- ** Initialization Flag
	pattern InitFlagTimer,
	pattern InitFlagAudio,
	pattern InitFlagVideo,
	pattern InitFlagJoystick,
	pattern InitFlagHaptic,
	pattern InitFlagGameController,
	pattern InitFlagEvents,
	pattern InitFlagNoParachute,
	pattern InitFlagEverything,

	-- ** Joystick Hat Position
	pattern JoystickHatCentered,
	pattern JoystickHatUp,
	pattern JoystickHatRight,
	pattern JoystickHatDown,
	pattern JoystickHatLeft,
	pattern JoystickHatRightUp,
	pattern JoystickHatRightDown,
	pattern JoystickHatLeftUp,
	pattern JoystickHatLeftDown,

	-- ** Log Category
	pattern LogCategoryApplication,
	pattern LogCategoryError,
	pattern LogCategoryAssert,
	pattern LogCategorySystem,
	pattern LogCategoryAudio,
	pattern LogCategoryVideo,
	pattern LogCategoryRender,
	pattern LogCategoryInput,
	pattern LogCategoryTest,
	pattern LogCategoryCustom,

	-- ** Message Box Flags
	pattern MessageBoxFlagError,
	pattern MessageBoxFlagWarning,
	pattern MessageBoxFlagInformation,

	-- ** Message Box Button Flags
	pattern MessageBoxButtonFlagReturnKeyDefault,
	pattern MessageBoxButtonFlagEscapeKeyDefault,

	-- ** OpenGL Profile
	pattern GLProfileCore,
	pattern GLProfileCompatibility,
	pattern GLProfileES,

	-- ** OpenGL Context Flag
	pattern GLContextFlagDebug,
	pattern GLContextFlagForwardCompatible,
	pattern GLContextFlagRobustAccess,
	pattern GLContextFlagResetIsolation,

	-- ** Pixel Formats
	pattern PixelFormatUnknown,
	pattern PixelFormatIndex1LSB,
	pattern PixelFormatIndex1MSB,
	pattern PixelFormatIndex4LSB,
	pattern PixelFormatIndex4MSB,
	pattern PixelFormatIndex8,
	pattern PixelFormatRGB332,
	pattern PixelFormatRGB444,
	pattern PixelFormatRGB555,
	pattern PixelFormatBGR555,
	pattern PixelFormatARGB4444,
	pattern PixelFormatRGBA4444,
	pattern PixelFormatABGR4444,
	pattern PixelFormatBGRA4444,
	pattern PixelFormatARGB1555,
	pattern PixelFormatRGBA5551,
	pattern PixelFormatABGR1555,
	pattern PixelFormatBGRA5551,
	pattern PixelFormatRGB565,
	pattern PixelFormatBGR565,
	pattern PixelFormatRGB24,
	pattern PixelFormatBGR24,
	pattern PixelFormatRGB888,
	pattern PixelFormatRGBX8888,
	pattern PixelFormatBGR888,
	pattern PixelFormatBGRX8888,
	pattern PixelFormatARGB8888,
	pattern PixelFormatRGBA8888,
	pattern PixelFormatABGR8888,
	pattern PixelFormatBGRA8888,
	pattern PixelFormatARGB2101010,
	pattern PixelFormatYV12,
	pattern PixelFormatIYUV,
	pattern PixelFormatYUY2,
	pattern PixelFormatUYVY,
	pattern PixelFormatYVYU,

	-- ** Renderer Flags
	pattern RendererFlagSoftware,
	pattern RendererFlagAccelerated,
	pattern RendererFlagPresentVSync,
	pattern RendererFlagTargetTexture,

	-- ** Texture Access
	pattern TextureAccessStatic,
	pattern TextureAccessStreaming,
	pattern TextureAccessTarget,

	-- ** Texture Modulate
	pattern TextureModulateNone,
	pattern TextureModulateColor,
	pattern TextureModulateAlpha,

	-- ** Window Event
	pattern WindowEventNone,
	pattern WindowEventShown,
	pattern WindowEventHidden,
	pattern WindowEventExposed,
	pattern WindowEventMoved,
	pattern WindowEventResized,
	pattern WindowEventSizeChanged,
	pattern WindowEventMinimized,
	pattern WindowEventMaximized,
	pattern WindowEventRestored,
	pattern WindowEventEnter,
	pattern WindowEventLeave,
	pattern WindowEventFocusGained,
	pattern WindowEventFocusLost,
	pattern WindowEventClose,

	-- ** Window Flags
	pattern WindowFlagFullscreen,
	pattern WindowFlagOpenGL,
	pattern WindowFlagShown,
	pattern WindowFlagHidden,
	pattern WindowFlagBorderless,
	pattern WindowFlagResizable,
	pattern WindowFlagMinimized,
	pattern WindowFlagMaximized,
	pattern WindowFlagInputGrabbed,
	pattern WindowFlagInputFocus,
	pattern WindowFlagMouseFocus,
	pattern WindowFlagFullscreenDesktop,
	pattern WindowFlagForeign,
	pattern WindowFlagAllowHighDPI,

	-- ** Window Positioning
	pattern WindowPosUndefined,
	pattern WindowPosCentered
) where

#include "SDL.h"

import Data.Int
import Data.Word

type AudioStatus = (#type SDL_AudioStatus)

pattern AudioStatusStopped = (#const SDL_AUDIO_STOPPED) :: AudioStatus
pattern AudioStatusPlaying = (#const SDL_AUDIO_PLAYING) :: AudioStatus
pattern AudioStatusPaused = (#const SDL_AUDIO_PAUSED) :: AudioStatus

type BlendMode = (#type SDL_BlendMode)

pattern BlendModeNone = (#const SDL_BLENDMODE_NONE) :: BlendMode
pattern BlendModeBlend = (#const SDL_BLENDMODE_BLEND) :: BlendMode
pattern BlendModeAdd = (#const SDL_BLENDMODE_ADD) :: BlendMode
pattern BlendModeMod = (#const SDL_BLENDMODE_MOD) :: BlendMode

type EventAction = (#type SDL_eventaction)

pattern EventActionAddEvent = (#const SDL_ADDEVENT) :: EventAction
pattern EventActionPeekEvent = (#const SDL_PEEKEVENT) :: EventAction
pattern EventActionGetEvent = (#const SDL_GETEVENT) :: EventAction

type GameControllerAxis = (#type SDL_GameControllerAxis)

pattern GameControllerAxisInvalid = (#const SDL_CONTROLLER_AXIS_INVALID) :: GameControllerAxis
pattern GameControllerAxisLeftX = (#const SDL_CONTROLLER_AXIS_LEFTX) :: GameControllerAxis
pattern GameControllerAxisLeftY = (#const SDL_CONTROLLER_AXIS_LEFTY) :: GameControllerAxis
pattern GameControllerAxisRightX = (#const SDL_CONTROLLER_AXIS_RIGHTX) :: GameControllerAxis
pattern GameControllerAxisRightY = (#const SDL_CONTROLLER_AXIS_RIGHTY) :: GameControllerAxis
pattern GameControllerAxisTriggerLeft = (#const SDL_CONTROLLER_AXIS_TRIGGERLEFT) :: GameControllerAxis
pattern GameControllerAxisTriggerRight = (#const SDL_CONTROLLER_AXIS_TRIGGERRIGHT) :: GameControllerAxis
pattern GameControllerAxisMax = (#const SDL_CONTROLLER_AXIS_MAX) :: GameControllerAxis

type GameControllerButton = (#type SDL_GameControllerButton)

pattern GameControllerButtonInvalid = (#const SDL_CONTROLLER_BUTTON_INVALID) :: GameControllerButton
pattern GameControllerButtonA = (#const SDL_CONTROLLER_BUTTON_A) :: GameControllerButton
pattern GameControllerButtonB = (#const SDL_CONTROLLER_BUTTON_B) :: GameControllerButton
pattern GameControllerButtonX = (#const SDL_CONTROLLER_BUTTON_X) :: GameControllerButton
pattern GameControllerButtonY = (#const SDL_CONTROLLER_BUTTON_Y) :: GameControllerButton
pattern GameControllerButtonBack = (#const SDL_CONTROLLER_BUTTON_BACK) :: GameControllerButton
pattern GameControllerButtonGuide = (#const SDL_CONTROLLER_BUTTON_GUIDE) :: GameControllerButton
pattern GameControllerButtonStart = (#const SDL_CONTROLLER_BUTTON_START) :: GameControllerButton
pattern GameControllerButtonLeftStick = (#const SDL_CONTROLLER_BUTTON_LEFTSTICK) :: GameControllerButton
pattern GameControllerButtonRightStick = (#const SDL_CONTROLLER_BUTTON_RIGHTSTICK) :: GameControllerButton
pattern GameControllerButtonLeftShoulder = (#const SDL_CONTROLLER_BUTTON_LEFTSHOULDER) :: GameControllerButton
pattern GameControllerButtonRightShoulder = (#const SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) :: GameControllerButton
pattern GameControllerButtonDPadUp = (#const SDL_CONTROLLER_BUTTON_DPAD_UP) :: GameControllerButton
pattern GameControllerButtonDPadDown = (#const SDL_CONTROLLER_BUTTON_DPAD_DOWN) :: GameControllerButton
pattern GameControllerButtonDPadLeft = (#const SDL_CONTROLLER_BUTTON_DPAD_LEFT) :: GameControllerButton
pattern GameControllerButtonDPadRight = (#const SDL_CONTROLLER_BUTTON_DPAD_RIGHT) :: GameControllerButton
pattern GameControllerButtonMax = (#const SDL_CONTROLLER_BUTTON_MAX) :: GameControllerButton

type GLattr = (#type SDL_GLattr)

pattern GLAttrRedSize = (#const SDL_GL_RED_SIZE) :: GLattr
pattern GLAttrGreenSize = (#const SDL_GL_GREEN_SIZE) :: GLattr
pattern GLAttrBlueSize = (#const SDL_GL_BLUE_SIZE) :: GLattr
pattern GLAttrAlphaSize = (#const SDL_GL_ALPHA_SIZE) :: GLattr
pattern GLAttrBufferSize = (#const SDL_GL_BUFFER_SIZE) :: GLattr
pattern GLAttrDoubleBuffer = (#const SDL_GL_DOUBLEBUFFER) :: GLattr
pattern GLAttrDepthSize = (#const SDL_GL_DEPTH_SIZE) :: GLattr
pattern GLAttrStencilSize = (#const SDL_GL_STENCIL_SIZE) :: GLattr
pattern GLAttrAccumRedSize = (#const SDL_GL_ACCUM_RED_SIZE) :: GLattr
pattern GLAttrAccumGreenSize = (#const SDL_GL_ACCUM_GREEN_SIZE) :: GLattr
pattern GLAttrAccumBlueSize = (#const SDL_GL_ACCUM_BLUE_SIZE) :: GLattr
pattern GLAttrAccumAlphaSize = (#const SDL_GL_ACCUM_ALPHA_SIZE) :: GLattr
pattern GLAttrStereo = (#const SDL_GL_STEREO) :: GLattr
pattern GLAttrMultiSampleBuffers = (#const SDL_GL_MULTISAMPLEBUFFERS) :: GLattr
pattern GLAttrMultiSampleSamples = (#const SDL_GL_MULTISAMPLESAMPLES) :: GLattr
pattern GLAttrAcceleratedVisual = (#const SDL_GL_ACCELERATED_VISUAL) :: GLattr
pattern GLAttrRetainedBacking = (#const SDL_GL_RETAINED_BACKING) :: GLattr
pattern GLAttrContextMajorVersion = (#const SDL_GL_CONTEXT_MAJOR_VERSION) :: GLattr
pattern GLAttrContextMinorVersion = (#const SDL_GL_CONTEXT_MINOR_VERSION) :: GLattr
pattern GLAttrContextEGL = (#const SDL_GL_CONTEXT_EGL) :: GLattr
pattern GLAttrContextFlags = (#const SDL_GL_CONTEXT_FLAGS) :: GLattr
pattern GLAttrContextProfileMask = (#const SDL_GL_CONTEXT_PROFILE_MASK) :: GLattr
pattern GLAttrShareWithCurrentContext = (#const SDL_GL_SHARE_WITH_CURRENT_CONTEXT) :: GLattr
pattern GLAttrFramebufferSRGBCapable = (#const SDL_GL_FRAMEBUFFER_SRGB_CAPABLE) :: GLattr

type HintPriority = (#type SDL_HintPriority)

pattern HintPriorityDefault = (#const SDL_HINT_DEFAULT) :: HintPriority
pattern HintPriorityNormal = (#const SDL_HINT_NORMAL) :: HintPriority
pattern HintPriorityOverride = (#const SDL_HINT_OVERRIDE) :: HintPriority

type Keycode = (#type SDL_Keycode)

pattern KeycodeUnknown = (#const SDLK_UNKNOWN) :: Keycode
pattern KeycodeReturn = (#const SDLK_RETURN) :: Keycode
pattern KeycodeEscape = (#const SDLK_ESCAPE) :: Keycode
pattern KeycodeBackspace = (#const SDLK_BACKSPACE) :: Keycode
pattern KeycodeTab = (#const SDLK_TAB) :: Keycode
pattern KeycodeSpace = (#const SDLK_SPACE) :: Keycode
pattern KeycodeExclaim = (#const SDLK_EXCLAIM) :: Keycode
pattern KeycodeQuoteDbl = (#const SDLK_QUOTEDBL) :: Keycode
pattern KeycodeHash = (#const SDLK_HASH) :: Keycode
pattern KeycodePercent = (#const SDLK_PERCENT) :: Keycode
pattern KeycodeDollar = (#const SDLK_DOLLAR) :: Keycode
pattern KeycodeAmpersand = (#const SDLK_AMPERSAND) :: Keycode
pattern KeycodeQuote = (#const SDLK_QUOTE) :: Keycode
pattern KeycodeLeftParen = (#const SDLK_LEFTPAREN) :: Keycode
pattern KeycodeRightParen = (#const SDLK_RIGHTPAREN) :: Keycode
pattern KeycodeAsterisk = (#const SDLK_ASTERISK) :: Keycode
pattern KeycodePlus = (#const SDLK_PLUS) :: Keycode
pattern KeycodeComma = (#const SDLK_COMMA) :: Keycode
pattern KeycodeMinus = (#const SDLK_MINUS) :: Keycode
pattern KeycodePeriod = (#const SDLK_PERIOD) :: Keycode
pattern KeycodeSlash = (#const SDLK_SLASH) :: Keycode
pattern Keycode0 = (#const SDLK_0) :: Keycode
pattern Keycode1 = (#const SDLK_1) :: Keycode
pattern Keycode2 = (#const SDLK_2) :: Keycode
pattern Keycode3 = (#const SDLK_3) :: Keycode
pattern Keycode4 = (#const SDLK_4) :: Keycode
pattern Keycode5 = (#const SDLK_5) :: Keycode
pattern Keycode6 = (#const SDLK_6) :: Keycode
pattern Keycode7 = (#const SDLK_7) :: Keycode
pattern Keycode8 = (#const SDLK_8) :: Keycode
pattern Keycode9 = (#const SDLK_9) :: Keycode
pattern KeycodeColon = (#const SDLK_COLON) :: Keycode
pattern KeycodeSemicolon = (#const SDLK_SEMICOLON) :: Keycode
pattern KeycodeLess = (#const SDLK_LESS) :: Keycode
pattern KeycodeEquals = (#const SDLK_EQUALS) :: Keycode
pattern KeycodeGreater = (#const SDLK_GREATER) :: Keycode
pattern KeycodeQuestion = (#const SDLK_QUESTION) :: Keycode
pattern KeycodeAt = (#const SDLK_AT) :: Keycode
pattern KeycodeLeftBracket = (#const SDLK_LEFTBRACKET) :: Keycode
pattern KeycodeBackslash = (#const SDLK_BACKSLASH) :: Keycode
pattern KeycodeRightBracket = (#const SDLK_RIGHTBRACKET) :: Keycode
pattern KeycodeCaret = (#const SDLK_CARET) :: Keycode
pattern KeycodeUnderscore = (#const SDLK_UNDERSCORE) :: Keycode
pattern KeycodeBackquote = (#const SDLK_BACKQUOTE) :: Keycode
pattern KeycodeA = (#const SDLK_a) :: Keycode
pattern KeycodeB = (#const SDLK_b) :: Keycode
pattern KeycodeC = (#const SDLK_c) :: Keycode
pattern KeycodeD = (#const SDLK_d) :: Keycode
pattern KeycodeE = (#const SDLK_e) :: Keycode
pattern KeycodeF = (#const SDLK_f) :: Keycode
pattern KeycodeG = (#const SDLK_g) :: Keycode
pattern KeycodeH = (#const SDLK_h) :: Keycode
pattern KeycodeI = (#const SDLK_i) :: Keycode
pattern KeycodeJ = (#const SDLK_j) :: Keycode
pattern KeycodeK = (#const SDLK_k) :: Keycode
pattern KeycodeL = (#const SDLK_l) :: Keycode
pattern KeycodeM = (#const SDLK_m) :: Keycode
pattern KeycodeN = (#const SDLK_n) :: Keycode
pattern KeycodeO = (#const SDLK_o) :: Keycode
pattern KeycodeP = (#const SDLK_p) :: Keycode
pattern KeycodeQ = (#const SDLK_q) :: Keycode
pattern KeycodeR = (#const SDLK_r) :: Keycode
pattern KeycodeS = (#const SDLK_s) :: Keycode
pattern KeycodeT = (#const SDLK_t) :: Keycode
pattern KeycodeU = (#const SDLK_u) :: Keycode
pattern KeycodeV = (#const SDLK_v) :: Keycode
pattern KeycodeW = (#const SDLK_w) :: Keycode
pattern KeycodeX = (#const SDLK_x) :: Keycode
pattern KeycodeY = (#const SDLK_y) :: Keycode
pattern KeycodeZ = (#const SDLK_z) :: Keycode
pattern KeycodeCapsLock = (#const SDLK_CAPSLOCK) :: Keycode
pattern KeycodeF1 = (#const SDLK_F1) :: Keycode
pattern KeycodeF2 = (#const SDLK_F2) :: Keycode
pattern KeycodeF3 = (#const SDLK_F3) :: Keycode
pattern KeycodeF4 = (#const SDLK_F4) :: Keycode
pattern KeycodeF5 = (#const SDLK_F5) :: Keycode
pattern KeycodeF6 = (#const SDLK_F6) :: Keycode
pattern KeycodeF7 = (#const SDLK_F7) :: Keycode
pattern KeycodeF8 = (#const SDLK_F8) :: Keycode
pattern KeycodeF9 = (#const SDLK_F9) :: Keycode
pattern KeycodeF10 = (#const SDLK_F10) :: Keycode
pattern KeycodeF11 = (#const SDLK_F11) :: Keycode
pattern KeycodeF12 = (#const SDLK_F12) :: Keycode
pattern KeycodePrintScreen = (#const SDLK_PRINTSCREEN) :: Keycode
pattern KeycodeScrollLock = (#const SDLK_SCROLLLOCK) :: Keycode
pattern KeycodePause = (#const SDLK_PAUSE) :: Keycode
pattern KeycodeInsert = (#const SDLK_INSERT) :: Keycode
pattern KeycodeHome = (#const SDLK_HOME) :: Keycode
pattern KeycodePageUp = (#const SDLK_PAGEUP) :: Keycode
pattern KeycodeDelete = (#const SDLK_DELETE) :: Keycode
pattern KeycodeEnd = (#const SDLK_END) :: Keycode
pattern KeycodePageDown = (#const SDLK_PAGEDOWN) :: Keycode
pattern KeycodeRight = (#const SDLK_RIGHT) :: Keycode
pattern KeycodeLeft = (#const SDLK_LEFT) :: Keycode
pattern KeycodeDown = (#const SDLK_DOWN) :: Keycode
pattern KeycodeUp = (#const SDLK_UP) :: Keycode
pattern KeycodeNumLockClear = (#const SDLK_NUMLOCKCLEAR) :: Keycode
pattern KeycodeKPDivide = (#const SDLK_KP_DIVIDE) :: Keycode
pattern KeycodeKPMultiply = (#const SDLK_KP_MULTIPLY) :: Keycode
pattern KeycodeKPMinus = (#const SDLK_KP_MINUS) :: Keycode
pattern KeycodeKPPlus = (#const SDLK_KP_PLUS) :: Keycode
pattern KeycodeKPEnter = (#const SDLK_KP_ENTER) :: Keycode
pattern KeycodeKP1 = (#const SDLK_KP_1) :: Keycode
pattern KeycodeKP2 = (#const SDLK_KP_2) :: Keycode
pattern KeycodeKP3 = (#const SDLK_KP_3) :: Keycode
pattern KeycodeKP4 = (#const SDLK_KP_4) :: Keycode
pattern KeycodeKP5 = (#const SDLK_KP_5) :: Keycode
pattern KeycodeKP6 = (#const SDLK_KP_6) :: Keycode
pattern KeycodeKP7 = (#const SDLK_KP_7) :: Keycode
pattern KeycodeKP8 = (#const SDLK_KP_8) :: Keycode
pattern KeycodeKP9 = (#const SDLK_KP_9) :: Keycode
pattern KeycodeKP0 = (#const SDLK_KP_0) :: Keycode
pattern KeycodeKPPeriod = (#const SDLK_KP_PERIOD) :: Keycode
pattern KeycodeApplication = (#const SDLK_APPLICATION) :: Keycode
pattern KeycodePower = (#const SDLK_POWER) :: Keycode
pattern KeycodeKPEquals = (#const SDLK_KP_EQUALS) :: Keycode
pattern KeycodeF13 = (#const SDLK_F13) :: Keycode
pattern KeycodeF14 = (#const SDLK_F14) :: Keycode
pattern KeycodeF15 = (#const SDLK_F15) :: Keycode
pattern KeycodeF16 = (#const SDLK_F16) :: Keycode
pattern KeycodeF17 = (#const SDLK_F17) :: Keycode
pattern KeycodeF18 = (#const SDLK_F18) :: Keycode
pattern KeycodeF19 = (#const SDLK_F19) :: Keycode
pattern KeycodeF20 = (#const SDLK_F20) :: Keycode
pattern KeycodeF21 = (#const SDLK_F21) :: Keycode
pattern KeycodeF22 = (#const SDLK_F22) :: Keycode
pattern KeycodeF23 = (#const SDLK_F23) :: Keycode
pattern KeycodeF24 = (#const SDLK_F24) :: Keycode
pattern KeycodeExecute = (#const SDLK_EXECUTE) :: Keycode
pattern KeycodeHelp = (#const SDLK_HELP) :: Keycode
pattern KeycodeMenu = (#const SDLK_MENU) :: Keycode
pattern KeycodeSelect = (#const SDLK_SELECT) :: Keycode
pattern KeycodeStop = (#const SDLK_STOP) :: Keycode
pattern KeycodeAgain = (#const SDLK_AGAIN) :: Keycode
pattern KeycodeUndo = (#const SDLK_UNDO) :: Keycode
pattern KeycodeCut = (#const SDLK_CUT) :: Keycode
pattern KeycodeCopy = (#const SDLK_COPY) :: Keycode
pattern KeycodePaste = (#const SDLK_PASTE) :: Keycode
pattern KeycodeFind = (#const SDLK_FIND) :: Keycode
pattern KeycodeMute = (#const SDLK_MUTE) :: Keycode
pattern KeycodeVolumeUp = (#const SDLK_VOLUMEUP) :: Keycode
pattern KeycodeVolumeDown = (#const SDLK_VOLUMEDOWN) :: Keycode
pattern KeycodeKPComma = (#const SDLK_KP_COMMA) :: Keycode
pattern KeycodeKPEqualsAS400 = (#const SDLK_KP_EQUALSAS400) :: Keycode
pattern KeycodeAltErase = (#const SDLK_ALTERASE) :: Keycode
pattern KeycodeSysReq = (#const SDLK_SYSREQ) :: Keycode
pattern KeycodeCancel = (#const SDLK_CANCEL) :: Keycode
pattern KeycodeClear = (#const SDLK_CLEAR) :: Keycode
pattern KeycodePrior = (#const SDLK_PRIOR) :: Keycode
pattern KeycodeReturn2 = (#const SDLK_RETURN2) :: Keycode
pattern KeycodeSeparator = (#const SDLK_SEPARATOR) :: Keycode
pattern KeycodeOut = (#const SDLK_OUT) :: Keycode
pattern KeycodeOper = (#const SDLK_OPER) :: Keycode
pattern KeycodeClearAgain = (#const SDLK_CLEARAGAIN) :: Keycode
pattern KeycodeCrSel = (#const SDLK_CRSEL) :: Keycode
pattern KeycodeExSel = (#const SDLK_EXSEL) :: Keycode
pattern KeycodeKP00 = (#const SDLK_KP_00) :: Keycode
pattern KeycodeKP000 = (#const SDLK_KP_000) :: Keycode
pattern KeycodeThousandsSeparator = (#const SDLK_THOUSANDSSEPARATOR) :: Keycode
pattern KeycodeDecimalSeparator = (#const SDLK_DECIMALSEPARATOR) :: Keycode
pattern KeycodeCurrencyUnit = (#const SDLK_CURRENCYUNIT) :: Keycode
pattern KeycodeCurrencySubunit = (#const SDLK_CURRENCYSUBUNIT) :: Keycode
pattern KeycodeKPLeftParen = (#const SDLK_KP_LEFTPAREN) :: Keycode
pattern KeycodeKPRightParen = (#const SDLK_KP_RIGHTPAREN) :: Keycode
pattern KeycodeKPLeftBrace = (#const SDLK_KP_LEFTBRACE) :: Keycode
pattern KeycodeKPRightBrace = (#const SDLK_KP_RIGHTBRACE) :: Keycode
pattern KeycodeKPTab = (#const SDLK_KP_TAB) :: Keycode
pattern KeycodeKPBackspace = (#const SDLK_KP_BACKSPACE) :: Keycode
pattern KeycodeKPA = (#const SDLK_KP_A) :: Keycode
pattern KeycodeKPB = (#const SDLK_KP_B) :: Keycode
pattern KeycodeKPC = (#const SDLK_KP_C) :: Keycode
pattern KeycodeKPD = (#const SDLK_KP_D) :: Keycode
pattern KeycodeKPE = (#const SDLK_KP_E) :: Keycode
pattern KeycodeKPF = (#const SDLK_KP_F) :: Keycode
pattern KeycodeKPXor = (#const SDLK_KP_XOR) :: Keycode
pattern KeycodeKPPower = (#const SDLK_KP_POWER) :: Keycode
pattern KeycodeKPPercent = (#const SDLK_KP_PERCENT) :: Keycode
pattern KeycodeKPLess = (#const SDLK_KP_LESS) :: Keycode
pattern KeycodeKPGreater = (#const SDLK_KP_GREATER) :: Keycode
pattern KeycodeKPAmpersand = (#const SDLK_KP_AMPERSAND) :: Keycode
pattern KeycodeKPDblAmpersand = (#const SDLK_KP_DBLAMPERSAND) :: Keycode
pattern KeycodeKPVecticalBar = (#const SDLK_KP_VERTICALBAR) :: Keycode
pattern KeycodeKPDblVerticalBar = (#const SDLK_KP_DBLVERTICALBAR) :: Keycode
pattern KeycodeKPColon = (#const SDLK_KP_COLON) :: Keycode
pattern KeycodeKPHash = (#const SDLK_KP_HASH) :: Keycode
pattern KeycodeKPSpace = (#const SDLK_KP_SPACE) :: Keycode
pattern KeycodeKPAt = (#const SDLK_KP_AT) :: Keycode
pattern KeycodeKPExclam = (#const SDLK_KP_EXCLAM) :: Keycode
pattern KeycodeKPMemStore = (#const SDLK_KP_MEMSTORE) :: Keycode
pattern KeycodeKPMemRecall = (#const SDLK_KP_MEMRECALL) :: Keycode
pattern KeycodeKPMemClear = (#const SDLK_KP_MEMCLEAR) :: Keycode
pattern KeycodeKPMemAdd = (#const SDLK_KP_MEMADD) :: Keycode
pattern KeycodeKPMemSubtract = (#const SDLK_KP_MEMSUBTRACT) :: Keycode
pattern KeycodeKPMemMultiply = (#const SDLK_KP_MEMMULTIPLY) :: Keycode
pattern KeycodeKPMemDivide = (#const SDLK_KP_MEMDIVIDE) :: Keycode
pattern KeycodeKPPlusMinus = (#const SDLK_KP_PLUSMINUS) :: Keycode
pattern KeycodeKPClear = (#const SDLK_KP_CLEAR) :: Keycode
pattern KeycodeKPClearEntry = (#const SDLK_KP_CLEARENTRY) :: Keycode
pattern KeycodeKPBinary = (#const SDLK_KP_BINARY) :: Keycode
pattern KeycodeKPOctal = (#const SDLK_KP_OCTAL) :: Keycode
pattern KeycodeKPDecimal = (#const SDLK_KP_DECIMAL) :: Keycode
pattern KeycodeKPHexadecimal = (#const SDLK_KP_HEXADECIMAL) :: Keycode
pattern KeycodeLCtrl = (#const SDLK_LCTRL) :: Keycode
pattern KeycodeLShift = (#const SDLK_LSHIFT) :: Keycode
pattern KeycodeLAlt = (#const SDLK_LALT) :: Keycode
pattern KeycodeLGUI = (#const SDLK_LGUI) :: Keycode
pattern KeycodeRCtrl = (#const SDLK_RCTRL) :: Keycode
pattern KeycodeRShift = (#const SDLK_RSHIFT) :: Keycode
pattern KeycodeRAlt = (#const SDLK_RALT) :: Keycode
pattern KeycodeRGUI = (#const SDLK_RGUI) :: Keycode
pattern KeycodeMode = (#const SDLK_MODE) :: Keycode
pattern KeycodeAudioNext = (#const SDLK_AUDIONEXT) :: Keycode
pattern KeycodeAudioPrev = (#const SDLK_AUDIONEXT) :: Keycode
pattern KeycodeAudioStop = (#const SDLK_AUDIOSTOP) :: Keycode
pattern KeycodeAudioPlay = (#const SDLK_AUDIOPLAY) :: Keycode
pattern KeycodeAudioMute = (#const SDLK_AUDIOMUTE) :: Keycode
pattern KeycodeMediaSelect = (#const SDLK_MEDIASELECT) :: Keycode
pattern KeycodeWWW = (#const SDLK_WWW) :: Keycode
pattern KeycodeMail = (#const SDLK_MAIL) :: Keycode
pattern KeycodeCalculator = (#const SDLK_CALCULATOR) :: Keycode
pattern KeycodeComputer = (#const SDLK_COMPUTER) :: Keycode
pattern KeycodeACSearch = (#const SDLK_AC_SEARCH) :: Keycode
pattern KeycodeACHome = (#const SDLK_AC_HOME) :: Keycode
pattern KeycodeACBack = (#const SDLK_AC_BACK) :: Keycode
pattern KeycodeACForward = (#const SDLK_AC_FORWARD) :: Keycode
pattern KeycodeACStop = (#const SDLK_AC_STOP) :: Keycode
pattern KeycodeACRefresh = (#const SDLK_AC_REFRESH) :: Keycode
pattern KeycodeACBookmarks = (#const SDLK_AC_BOOKMARKS) :: Keycode
pattern KeycodeBrightnessDown = (#const SDLK_BRIGHTNESSDOWN) :: Keycode
pattern KeycodeBrightnessUp = (#const SDLK_BRIGHTNESSUP) :: Keycode
pattern KeycodeDisplaySwitch = (#const SDLK_DISPLAYSWITCH) :: Keycode
pattern KeycodeKbdIllumToggle = (#const SDLK_KBDILLUMTOGGLE) :: Keycode
pattern KeycodeKbdIllumDown = (#const SDLK_KBDILLUMDOWN) :: Keycode
pattern KeycodeKbdIllumUp = (#const SDLK_KBDILLUMUP) :: Keycode
pattern KeycodeEject = (#const SDLK_EJECT) :: Keycode
pattern KeycodeSleep = (#const SDLK_SLEEP) :: Keycode

type Keymod = (#type SDL_Keymod)

pattern KeymodNone = (#const KMOD_NONE) :: Keymod
pattern KeymodLShift = (#const KMOD_LSHIFT) :: Keymod
pattern KeymodRShift = (#const KMOD_RSHIFT) :: Keymod
pattern KeymodLCtrl = (#const KMOD_LCTRL) :: Keymod
pattern KeymodRCtrl = (#const KMOD_RCTRL) :: Keymod
pattern KeymodLAlt = (#const KMOD_LALT) :: Keymod
pattern KeymodRAlt = (#const KMOD_RALT) :: Keymod
pattern KeymodLGUI = (#const KMOD_LGUI) :: Keymod
pattern KeymodRGUI = (#const KMOD_RGUI) :: Keymod
pattern KeymodNum = (#const KMOD_NUM) :: Keymod
pattern KeymodCaps = (#const KMOD_CAPS) :: Keymod
pattern KeymodMode = (#const KMOD_MODE) :: Keymod
pattern KeymodReserved = (#const KMOD_RESERVED) :: Keymod

type LogPriority = (#type SDL_LogPriority)

pattern LogPriorityVerbose = (#const SDL_LOG_PRIORITY_VERBOSE) :: LogPriority
pattern LogPriorityDebug = (#const SDL_LOG_PRIORITY_DEBUG) :: LogPriority
pattern LogPriorityInfo = (#const SDL_LOG_PRIORITY_INFO) :: LogPriority
pattern LogPriorityWarn = (#const SDL_LOG_PRIORITY_WARN) :: LogPriority
pattern LogPriorityError = (#const SDL_LOG_PRIORITY_ERROR) :: LogPriority
pattern LogPriorityCritical = (#const SDL_LOG_PRIORITY_CRITICAL) :: LogPriority
pattern LogPriorityPriorities = (#const SDL_NUM_LOG_PRIORITIES) :: LogPriority

type PowerState = (#type SDL_PowerState)

pattern PowerStateUnknown = (#const SDL_POWERSTATE_UNKNOWN) :: PowerState
pattern PowerStateOnBattery = (#const SDL_POWERSTATE_ON_BATTERY) :: PowerState
pattern PowerStateNoBattery = (#const SDL_POWERSTATE_NO_BATTERY) :: PowerState
pattern PowerStateCharging = (#const SDL_POWERSTATE_CHARGING) :: PowerState
pattern PowerStateCharged = (#const SDL_POWERSTATE_CHARGED) :: PowerState

type RendererFlip = (#type SDL_RendererFlip)

pattern RendererFlipNone = (#const SDL_FLIP_NONE) :: RendererFlip
pattern RendererFlipHorizontal = (#const SDL_FLIP_HORIZONTAL) :: RendererFlip
pattern RendererFlipVertical = (#const SDL_FLIP_VERTICAL) :: RendererFlip

type Scancode = (#type SDL_Scancode)

pattern ScancodeUnknown = (#const SDL_SCANCODE_UNKNOWN) :: Scancode
pattern ScancodeA = (#const SDL_SCANCODE_A) :: Scancode
pattern ScancodeB = (#const SDL_SCANCODE_B) :: Scancode
pattern ScancodeC = (#const SDL_SCANCODE_C) :: Scancode
pattern ScancodeD = (#const SDL_SCANCODE_D) :: Scancode
pattern ScancodeE = (#const SDL_SCANCODE_E) :: Scancode
pattern ScancodeF = (#const SDL_SCANCODE_F) :: Scancode
pattern ScancodeG = (#const SDL_SCANCODE_G) :: Scancode
pattern ScancodeH = (#const SDL_SCANCODE_H) :: Scancode
pattern ScancodeI = (#const SDL_SCANCODE_I) :: Scancode
pattern ScancodeJ = (#const SDL_SCANCODE_J) :: Scancode
pattern ScancodeK = (#const SDL_SCANCODE_K) :: Scancode
pattern ScancodeL = (#const SDL_SCANCODE_L) :: Scancode
pattern ScancodeM = (#const SDL_SCANCODE_M) :: Scancode
pattern ScancodeN = (#const SDL_SCANCODE_N) :: Scancode
pattern ScancodeO = (#const SDL_SCANCODE_O) :: Scancode
pattern ScancodeP = (#const SDL_SCANCODE_P) :: Scancode
pattern ScancodeQ = (#const SDL_SCANCODE_Q) :: Scancode
pattern ScancodeR = (#const SDL_SCANCODE_R) :: Scancode
pattern ScancodeS = (#const SDL_SCANCODE_S) :: Scancode
pattern ScancodeT = (#const SDL_SCANCODE_T) :: Scancode
pattern ScancodeU = (#const SDL_SCANCODE_U) :: Scancode
pattern ScancodeV = (#const SDL_SCANCODE_V) :: Scancode
pattern ScancodeW = (#const SDL_SCANCODE_W) :: Scancode
pattern ScancodeX = (#const SDL_SCANCODE_X) :: Scancode
pattern ScancodeY = (#const SDL_SCANCODE_Y) :: Scancode
pattern ScancodeZ = (#const SDL_SCANCODE_Z) :: Scancode
pattern Scancode1 = (#const SDL_SCANCODE_1) :: Scancode
pattern Scancode2 = (#const SDL_SCANCODE_2) :: Scancode
pattern Scancode3 = (#const SDL_SCANCODE_3) :: Scancode
pattern Scancode4 = (#const SDL_SCANCODE_4) :: Scancode
pattern Scancode5 = (#const SDL_SCANCODE_5) :: Scancode
pattern Scancode6 = (#const SDL_SCANCODE_6) :: Scancode
pattern Scancode7 = (#const SDL_SCANCODE_7) :: Scancode
pattern Scancode8 = (#const SDL_SCANCODE_8) :: Scancode
pattern Scancode9 = (#const SDL_SCANCODE_9) :: Scancode
pattern Scancode0 = (#const SDL_SCANCODE_0) :: Scancode
pattern ScancodeReturn = (#const SDL_SCANCODE_RETURN) :: Scancode
pattern ScancodeEscape = (#const SDL_SCANCODE_ESCAPE) :: Scancode
pattern ScancodeBackspace = (#const SDL_SCANCODE_BACKSPACE) :: Scancode
pattern ScancodeTab = (#const SDL_SCANCODE_TAB) :: Scancode
pattern ScancodeSpace = (#const SDL_SCANCODE_SPACE) :: Scancode
pattern ScancodeMinus = (#const SDL_SCANCODE_MINUS) :: Scancode
pattern ScancodeEquals = (#const SDL_SCANCODE_EQUALS) :: Scancode
pattern ScancodeLeftBracket = (#const SDL_SCANCODE_LEFTBRACKET) :: Scancode
pattern ScancodeRightBracket = (#const SDL_SCANCODE_RIGHTBRACKET) :: Scancode
pattern ScancodeBackslash = (#const SDL_SCANCODE_BACKSLASH) :: Scancode
pattern ScancodeNonUSHash = (#const SDL_SCANCODE_NONUSHASH) :: Scancode
pattern ScancodeSemicolon = (#const SDL_SCANCODE_SEMICOLON) :: Scancode
pattern ScancodeApostrophe = (#const SDL_SCANCODE_APOSTROPHE) :: Scancode
pattern ScancodeGrave = (#const SDL_SCANCODE_GRAVE) :: Scancode
pattern ScancodeComma = (#const SDL_SCANCODE_COMMA) :: Scancode
pattern ScancodePeriod = (#const SDL_SCANCODE_PERIOD) :: Scancode
pattern ScancodeSlash = (#const SDL_SCANCODE_SLASH) :: Scancode
pattern ScancodeCapsLock = (#const SDL_SCANCODE_CAPSLOCK) :: Scancode
pattern ScancodeF1 = (#const SDL_SCANCODE_F1) :: Scancode
pattern ScancodeF2 = (#const SDL_SCANCODE_F2) :: Scancode
pattern ScancodeF3 = (#const SDL_SCANCODE_F3) :: Scancode
pattern ScancodeF4 = (#const SDL_SCANCODE_F4) :: Scancode
pattern ScancodeF5 = (#const SDL_SCANCODE_F5) :: Scancode
pattern ScancodeF6 = (#const SDL_SCANCODE_F6) :: Scancode
pattern ScancodeF7 = (#const SDL_SCANCODE_F7) :: Scancode
pattern ScancodeF8 = (#const SDL_SCANCODE_F8) :: Scancode
pattern ScancodeF9 = (#const SDL_SCANCODE_F9) :: Scancode
pattern ScancodeF10 = (#const SDL_SCANCODE_F10) :: Scancode
pattern ScancodeF11 = (#const SDL_SCANCODE_F11) :: Scancode
pattern ScancodeF12 = (#const SDL_SCANCODE_F12) :: Scancode
pattern ScancodePrintScreen = (#const SDL_SCANCODE_PRINTSCREEN) :: Scancode
pattern ScancodeScrollLock = (#const SDL_SCANCODE_SCROLLLOCK) :: Scancode
pattern ScancodePause = (#const SDL_SCANCODE_PAUSE) :: Scancode
pattern ScancodeInsert = (#const SDL_SCANCODE_INSERT) :: Scancode
pattern ScancodeHome = (#const SDL_SCANCODE_HOME) :: Scancode
pattern ScancodePageUp = (#const SDL_SCANCODE_PAGEUP) :: Scancode
pattern ScancodeDelete = (#const SDL_SCANCODE_DELETE) :: Scancode
pattern ScancodeEnd = (#const SDL_SCANCODE_END) :: Scancode
pattern ScancodePageDown = (#const SDL_SCANCODE_PAGEDOWN) :: Scancode
pattern ScancodeRight = (#const SDL_SCANCODE_RIGHT) :: Scancode
pattern ScancodeLeft = (#const SDL_SCANCODE_LEFT) :: Scancode
pattern ScancodeDown = (#const SDL_SCANCODE_DOWN) :: Scancode
pattern ScancodeUp = (#const SDL_SCANCODE_UP) :: Scancode
pattern ScancodeNumLockClear = (#const SDL_SCANCODE_NUMLOCKCLEAR) :: Scancode
pattern ScancodeKPDivide = (#const SDL_SCANCODE_KP_DIVIDE) :: Scancode
pattern ScancodeKPMultiply = (#const SDL_SCANCODE_KP_MULTIPLY) :: Scancode
pattern ScancodeKPMinus = (#const SDL_SCANCODE_KP_MINUS) :: Scancode
pattern ScancodeKPPlus = (#const SDL_SCANCODE_KP_PLUS) :: Scancode
pattern ScancodeKPEnter = (#const SDL_SCANCODE_KP_ENTER) :: Scancode
pattern ScancodeKP1 = (#const SDL_SCANCODE_KP_1) :: Scancode
pattern ScancodeKP2 = (#const SDL_SCANCODE_KP_2) :: Scancode
pattern ScancodeKP3 = (#const SDL_SCANCODE_KP_3) :: Scancode
pattern ScancodeKP4 = (#const SDL_SCANCODE_KP_4) :: Scancode
pattern ScancodeKP5 = (#const SDL_SCANCODE_KP_5) :: Scancode
pattern ScancodeKP6 = (#const SDL_SCANCODE_KP_6) :: Scancode
pattern ScancodeKP7 = (#const SDL_SCANCODE_KP_7) :: Scancode
pattern ScancodeKP8 = (#const SDL_SCANCODE_KP_8) :: Scancode
pattern ScancodeKP9 = (#const SDL_SCANCODE_KP_9) :: Scancode
pattern ScancodeKP0 = (#const SDL_SCANCODE_KP_0) :: Scancode
pattern ScancodeKPPeriod = (#const SDL_SCANCODE_KP_PERIOD) :: Scancode
pattern ScancodeNonUSBackslash = (#const SDL_SCANCODE_NONUSBACKSLASH) :: Scancode
pattern ScancodeApplication = (#const SDL_SCANCODE_APPLICATION) :: Scancode
pattern ScancodePower = (#const SDL_SCANCODE_POWER) :: Scancode
pattern ScancodeKPEquals = (#const SDL_SCANCODE_KP_EQUALS) :: Scancode
pattern ScancodeF13 = (#const SDL_SCANCODE_F13) :: Scancode
pattern ScancodeF14 = (#const SDL_SCANCODE_F14) :: Scancode
pattern ScancodeF15 = (#const SDL_SCANCODE_F15) :: Scancode
pattern ScancodeF16 = (#const SDL_SCANCODE_F16) :: Scancode
pattern ScancodeF17 = (#const SDL_SCANCODE_F17) :: Scancode
pattern ScancodeF18 = (#const SDL_SCANCODE_F18) :: Scancode
pattern ScancodeF19 = (#const SDL_SCANCODE_F19) :: Scancode
pattern ScancodeF20 = (#const SDL_SCANCODE_F20) :: Scancode
pattern ScancodeF21 = (#const SDL_SCANCODE_F21) :: Scancode
pattern ScancodeF22 = (#const SDL_SCANCODE_F22) :: Scancode
pattern ScancodeF23 = (#const SDL_SCANCODE_F23) :: Scancode
pattern ScancodeF24 = (#const SDL_SCANCODE_F24) :: Scancode
pattern ScancodeExecute = (#const SDL_SCANCODE_EXECUTE) :: Scancode
pattern ScancodeHelp = (#const SDL_SCANCODE_HELP) :: Scancode
pattern ScancodeMenu = (#const SDL_SCANCODE_MENU) :: Scancode
pattern ScancodeSelect = (#const SDL_SCANCODE_SELECT) :: Scancode
pattern ScancodeStop = (#const SDL_SCANCODE_STOP) :: Scancode
pattern ScancodeAgain = (#const SDL_SCANCODE_AGAIN) :: Scancode
pattern ScancodeUndo = (#const SDL_SCANCODE_UNDO) :: Scancode
pattern ScancodeCut = (#const SDL_SCANCODE_CUT) :: Scancode
pattern ScancodeCopy = (#const SDL_SCANCODE_COPY) :: Scancode
pattern ScancodePaste = (#const SDL_SCANCODE_PASTE) :: Scancode
pattern ScancodeFind = (#const SDL_SCANCODE_FIND) :: Scancode
pattern ScancodeMute = (#const SDL_SCANCODE_MUTE) :: Scancode
pattern ScancodeVolumeUp = (#const SDL_SCANCODE_VOLUMEUP) :: Scancode
pattern ScancodeVolumeDown = (#const SDL_SCANCODE_VOLUMEDOWN) :: Scancode
pattern ScancodeKPComma = (#const SDL_SCANCODE_KP_COMMA) :: Scancode
pattern ScancodeEqualsAs400 = (#const SDL_SCANCODE_KP_EQUALSAS400) :: Scancode
pattern ScancodeInternational1 = (#const SDL_SCANCODE_INTERNATIONAL1) :: Scancode
pattern ScancodeInternational2 = (#const SDL_SCANCODE_INTERNATIONAL2) :: Scancode
pattern ScancodeInternational3 = (#const SDL_SCANCODE_INTERNATIONAL3) :: Scancode
pattern ScancodeInternational4 = (#const SDL_SCANCODE_INTERNATIONAL4) :: Scancode
pattern ScancodeInternational5 = (#const SDL_SCANCODE_INTERNATIONAL5) :: Scancode
pattern ScancodeInternational6 = (#const SDL_SCANCODE_INTERNATIONAL6) :: Scancode
pattern ScancodeInternational7 = (#const SDL_SCANCODE_INTERNATIONAL7) :: Scancode
pattern ScancodeInternational8 = (#const SDL_SCANCODE_INTERNATIONAL8) :: Scancode
pattern ScancodeInternational9 = (#const SDL_SCANCODE_INTERNATIONAL9) :: Scancode
pattern ScancodeLang1 = (#const SDL_SCANCODE_LANG1) :: Scancode
pattern ScancodeLang2 = (#const SDL_SCANCODE_LANG2) :: Scancode
pattern ScancodeLang3 = (#const SDL_SCANCODE_LANG3) :: Scancode
pattern ScancodeLang4 = (#const SDL_SCANCODE_LANG4) :: Scancode
pattern ScancodeLang5 = (#const SDL_SCANCODE_LANG5) :: Scancode
pattern ScancodeLang6 = (#const SDL_SCANCODE_LANG6) :: Scancode
pattern ScancodeLang7 = (#const SDL_SCANCODE_LANG7) :: Scancode
pattern ScancodeLang8 = (#const SDL_SCANCODE_LANG8) :: Scancode
pattern ScancodeLang9 = (#const SDL_SCANCODE_LANG9) :: Scancode
pattern ScancodeAltErase = (#const SDL_SCANCODE_ALTERASE) :: Scancode
pattern ScancodeSysReq = (#const SDL_SCANCODE_SYSREQ) :: Scancode
pattern ScancodeCancel = (#const SDL_SCANCODE_CANCEL) :: Scancode
pattern ScancodeClear = (#const SDL_SCANCODE_CLEAR) :: Scancode
pattern ScancodePrior = (#const SDL_SCANCODE_PRIOR) :: Scancode
pattern ScancodeReturn2 = (#const SDL_SCANCODE_RETURN2) :: Scancode
pattern ScancodeSeparator = (#const SDL_SCANCODE_SEPARATOR) :: Scancode
pattern ScancodeOut = (#const SDL_SCANCODE_OUT) :: Scancode
pattern ScancodeOper = (#const SDL_SCANCODE_OPER) :: Scancode
pattern ScancodeClearAgain = (#const SDL_SCANCODE_CLEARAGAIN) :: Scancode
pattern ScancodeCrSel = (#const SDL_SCANCODE_CRSEL) :: Scancode
pattern ScancodeExSel = (#const SDL_SCANCODE_EXSEL) :: Scancode
pattern ScancodeKP00 = (#const SDL_SCANCODE_KP_00) :: Scancode
pattern ScancodeKP000 = (#const SDL_SCANCODE_KP_000) :: Scancode
pattern ScancodeThousandsSeparator = (#const SDL_SCANCODE_THOUSANDSSEPARATOR) :: Scancode
pattern ScancodeDecimalSeparator = (#const SDL_SCANCODE_DECIMALSEPARATOR) :: Scancode
pattern ScancodeCurrencyUnit = (#const SDL_SCANCODE_CURRENCYUNIT) :: Scancode
pattern ScancodeCurrencySubunit = (#const SDL_SCANCODE_CURRENCYSUBUNIT) :: Scancode
pattern ScancodeLeftParen = (#const SDL_SCANCODE_KP_LEFTPAREN) :: Scancode
pattern ScancodeRightParen = (#const SDL_SCANCODE_KP_RIGHTPAREN) :: Scancode
pattern ScancodeLeftBrace = (#const SDL_SCANCODE_KP_LEFTBRACE) :: Scancode
pattern ScancodeRightBrace = (#const SDL_SCANCODE_KP_RIGHTBRACE) :: Scancode
pattern ScancodeKPTab = (#const SDL_SCANCODE_KP_TAB) :: Scancode
pattern ScancodeKPBackspace = (#const SDL_SCANCODE_KP_BACKSPACE) :: Scancode
pattern ScancodeKPA = (#const SDL_SCANCODE_KP_A) :: Scancode
pattern ScancodeKPB = (#const SDL_SCANCODE_KP_B) :: Scancode
pattern ScancodeKPC = (#const SDL_SCANCODE_KP_C) :: Scancode
pattern ScancodeKPD = (#const SDL_SCANCODE_KP_D) :: Scancode
pattern ScancodeKPE = (#const SDL_SCANCODE_KP_E) :: Scancode
pattern ScancodeKPF = (#const SDL_SCANCODE_KP_F) :: Scancode
pattern ScancodeKPXOR = (#const SDL_SCANCODE_KP_XOR) :: Scancode
pattern ScancodeKPPower = (#const SDL_SCANCODE_KP_POWER) :: Scancode
pattern ScancodeKPPercent = (#const SDL_SCANCODE_KP_PERCENT) :: Scancode
pattern ScancodeKPLess = (#const SDL_SCANCODE_KP_LESS) :: Scancode
pattern ScancodeKPGreater = (#const SDL_SCANCODE_KP_GREATER) :: Scancode
pattern ScancodeKPAmpersand = (#const SDL_SCANCODE_KP_AMPERSAND) :: Scancode
pattern ScancodeKPDBLAmpersand = (#const SDL_SCANCODE_KP_DBLAMPERSAND) :: Scancode
pattern ScancodeKPVerticalBar = (#const SDL_SCANCODE_KP_VERTICALBAR) :: Scancode
pattern ScancodeKPDBLVerticalBar = (#const SDL_SCANCODE_KP_DBLVERTICALBAR) :: Scancode
pattern ScancodeKPColon = (#const SDL_SCANCODE_KP_COLON) :: Scancode
pattern ScancodeKPHash = (#const SDL_SCANCODE_KP_HASH) :: Scancode
pattern ScancodeKPSpace = (#const SDL_SCANCODE_KP_SPACE) :: Scancode
pattern ScancodeKPAt = (#const SDL_SCANCODE_KP_AT) :: Scancode
pattern ScancodeKPExclam = (#const SDL_SCANCODE_KP_EXCLAM) :: Scancode
pattern ScancodeKPMemStore = (#const SDL_SCANCODE_KP_MEMSTORE) :: Scancode
pattern ScancodeKPMemRecall = (#const SDL_SCANCODE_KP_MEMRECALL) :: Scancode
pattern ScancodeKPMemClear = (#const SDL_SCANCODE_KP_MEMCLEAR) :: Scancode
pattern ScancodeKPMemAdd = (#const SDL_SCANCODE_KP_MEMADD) :: Scancode
pattern ScancodeKPMemSubtract = (#const SDL_SCANCODE_KP_MEMSUBTRACT) :: Scancode
pattern ScancodeKPMemMultiply = (#const SDL_SCANCODE_KP_MEMMULTIPLY) :: Scancode
pattern ScancodeKPMemDivide = (#const SDL_SCANCODE_KP_MEMDIVIDE) :: Scancode
pattern ScancodeKPPlusMinus = (#const SDL_SCANCODE_KP_PLUSMINUS) :: Scancode
pattern ScancodeKPClear = (#const SDL_SCANCODE_KP_CLEAR) :: Scancode
pattern ScancodeKPClearEntry = (#const SDL_SCANCODE_KP_CLEARENTRY) :: Scancode
pattern ScancodeKPBinary = (#const SDL_SCANCODE_KP_BINARY) :: Scancode
pattern ScancodeKPOctal = (#const SDL_SCANCODE_KP_OCTAL) :: Scancode
pattern ScancodeKPDecimal = (#const SDL_SCANCODE_KP_DECIMAL) :: Scancode
pattern ScancodeKPHexadecimal = (#const SDL_SCANCODE_KP_HEXADECIMAL) :: Scancode
pattern ScancodeLCtrl = (#const SDL_SCANCODE_LCTRL) :: Scancode
pattern ScancodeLShift = (#const SDL_SCANCODE_LSHIFT) :: Scancode
pattern ScancodeLAlt = (#const SDL_SCANCODE_LALT) :: Scancode
pattern ScancodeLGUI = (#const SDL_SCANCODE_LGUI) :: Scancode
pattern ScancodeRCtrl = (#const SDL_SCANCODE_RCTRL) :: Scancode
pattern ScancodeRShift = (#const SDL_SCANCODE_RSHIFT) :: Scancode
pattern ScancodeRAlt = (#const SDL_SCANCODE_RALT) :: Scancode
pattern ScancodeRGUI = (#const SDL_SCANCODE_RGUI) :: Scancode
pattern ScancodeMode = (#const SDL_SCANCODE_MODE) :: Scancode
pattern ScancodeAudioNext = (#const SDL_SCANCODE_AUDIONEXT) :: Scancode
pattern ScancodeAudioPrev = (#const SDL_SCANCODE_AUDIOPREV) :: Scancode
pattern ScancodeAudioStop = (#const SDL_SCANCODE_AUDIOSTOP) :: Scancode
pattern ScancodeAudioPlay = (#const SDL_SCANCODE_AUDIOPLAY) :: Scancode
pattern ScancodeAudioMute = (#const SDL_SCANCODE_AUDIOMUTE) :: Scancode
pattern ScancodeMediaSelect = (#const SDL_SCANCODE_MEDIASELECT) :: Scancode
pattern ScancodeWWW = (#const SDL_SCANCODE_WWW) :: Scancode
pattern ScancodeMail = (#const SDL_SCANCODE_MAIL) :: Scancode
pattern ScancodeCalculator = (#const SDL_SCANCODE_CALCULATOR) :: Scancode
pattern ScancodeComputer = (#const SDL_SCANCODE_COMPUTER) :: Scancode
pattern ScancodeACSearch = (#const SDL_SCANCODE_AC_SEARCH) :: Scancode
pattern ScancodeACHome = (#const SDL_SCANCODE_AC_HOME) :: Scancode
pattern ScancodeACBack = (#const SDL_SCANCODE_AC_BACK) :: Scancode
pattern ScancodeACForward = (#const SDL_SCANCODE_AC_FORWARD) :: Scancode
pattern ScancodeACStop = (#const SDL_SCANCODE_AC_STOP) :: Scancode
pattern ScancodeACRefresh = (#const SDL_SCANCODE_AC_REFRESH) :: Scancode
pattern ScancodeACBookmarks = (#const SDL_SCANCODE_AC_BOOKMARKS) :: Scancode
pattern ScancodeBrightnessDown = (#const SDL_SCANCODE_BRIGHTNESSDOWN) :: Scancode
pattern ScancodeBrightnessUp = (#const SDL_SCANCODE_BRIGHTNESSUP) :: Scancode
pattern ScancodeDisplaySwitch = (#const SDL_SCANCODE_DISPLAYSWITCH) :: Scancode
pattern ScancodeKBDIllumToggle = (#const SDL_SCANCODE_KBDILLUMTOGGLE) :: Scancode
pattern ScancodeKBDIllumDown = (#const SDL_SCANCODE_KBDILLUMDOWN) :: Scancode
pattern ScancodeKBDIllumUp = (#const SDL_SCANCODE_KBDILLUMUP) :: Scancode
pattern ScancodeEject = (#const SDL_SCANCODE_EJECT) :: Scancode
pattern ScancodeSleep = (#const SDL_SCANCODE_SLEEP) :: Scancode
pattern ScancodeApp1 = (#const SDL_SCANCODE_APP1) :: Scancode
pattern ScancodeApp2 = (#const SDL_SCANCODE_APP2) :: Scancode
pattern ScancodeNum = (#const SDL_NUM_SCANCODES) :: Scancode

type SystemCursor = (#type SDL_SystemCursor)

pattern SystemCursorArrow = (#const SDL_SYSTEM_CURSOR_ARROW) :: SystemCursor
pattern SystemCursorIBeam = (#const SDL_SYSTEM_CURSOR_IBEAM) :: SystemCursor
pattern SystemCursorWait = (#const SDL_SYSTEM_CURSOR_WAIT) :: SystemCursor
pattern SystemCursorCrosshair = (#const SDL_SYSTEM_CURSOR_CROSSHAIR) :: SystemCursor
pattern SystemCursorWaitArrow = (#const SDL_SYSTEM_CURSOR_WAITARROW) :: SystemCursor
pattern SystemCursorSizeNWSE = (#const SDL_SYSTEM_CURSOR_SIZENWSE) :: SystemCursor
pattern SystemCursorSizeNESW = (#const SDL_SYSTEM_CURSOR_SIZENESW) :: SystemCursor
pattern SystemCursorSizeWE = (#const SDL_SYSTEM_CURSOR_SIZEWE) :: SystemCursor
pattern SystemCursorSizeNS = (#const SDL_SYSTEM_CURSOR_SIZENS) :: SystemCursor
pattern SystemCursorSizeAll = (#const SDL_SYSTEM_CURSOR_SIZEALL) :: SystemCursor
pattern SystemCursorNo = (#const SDL_SYSTEM_CURSOR_NO) :: SystemCursor
pattern SystemCursorHand = (#const SDL_SYSTEM_CURSOR_HAND) :: SystemCursor
pattern SystemCursorNum = (#const SDL_NUM_SYSTEM_CURSORS) :: SystemCursor

type ThreadPriority = (#type SDL_ThreadPriority)

pattern ThreadPriorityLow = (#const SDL_THREAD_PRIORITY_LOW) :: ThreadPriority
pattern ThreadPriorityNormal = (#const SDL_THREAD_PRIORITY_NORMAL) :: ThreadPriority
pattern ThreadPriorityHigh = (#const SDL_THREAD_PRIORITY_HIGH) :: ThreadPriority

pattern AudioAllowFrequencyChange = (#const SDL_AUDIO_ALLOW_FREQUENCY_CHANGE)
pattern AudioAllowFormatChange = (#const SDL_AUDIO_ALLOW_FORMAT_CHANGE)
pattern AudioAllowChannelsChange = (#const SDL_AUDIO_ALLOW_CHANNELS_CHANGE)
pattern AudioAllowAnyChange = (#const SDL_AUDIO_ALLOW_ANY_CHANGE)

pattern ButtonLeft = (#const SDL_BUTTON_LEFT)
pattern ButtonMiddle = (#const SDL_BUTTON_MIDDLE)
pattern ButtonRight = (#const SDL_BUTTON_RIGHT)
pattern ButtonX1 = (#const SDL_BUTTON_X1)
pattern ButtonX2 = (#const SDL_BUTTON_X2)
pattern ButtonLMask = (#const SDL_BUTTON_LMASK)
pattern ButtonMMask = (#const SDL_BUTTON_MMASK)
pattern ButtonRMask = (#const SDL_BUTTON_RMASK)
pattern ButtonX1Mask = (#const SDL_BUTTON_X1MASK)
pattern ButtonX2Mask = (#const SDL_BUTTON_X2MASK)

pattern EventTypeFirstEvent = (#const SDL_FIRSTEVENT)
pattern EventTypeQuit = (#const SDL_QUIT)
pattern EventTypeAppTerminating = (#const SDL_APP_TERMINATING)
pattern EventTypeAppLowMemory = (#const SDL_APP_LOWMEMORY)
pattern EventTypeAppWillEnterBackground = (#const SDL_APP_WILLENTERBACKGROUND)
pattern EventTypeAppDidEnterBackground = (#const SDL_APP_DIDENTERBACKGROUND)
pattern EventTypeAppWillEnterForeground = (#const SDL_APP_WILLENTERFOREGROUND)
pattern EventTypeAppDidEnterForeground = (#const SDL_APP_DIDENTERFOREGROUND)
pattern EventTypeWindowEvent = (#const SDL_WINDOWEVENT)
pattern EventTypeSysWMEvent = (#const SDL_SYSWMEVENT)
pattern EventTypeKeyDown = (#const SDL_KEYDOWN)
pattern EventTypeKeyUp = (#const SDL_KEYUP)
pattern EventTypeTextEditing = (#const SDL_TEXTEDITING)
pattern EventTypeTextInput = (#const SDL_TEXTINPUT)
pattern EventTypeMouseMotion = (#const SDL_MOUSEMOTION)
pattern EventTypeMouseButtonDown = (#const SDL_MOUSEBUTTONDOWN)
pattern EventTypeMouseButtonUp = (#const SDL_MOUSEBUTTONUP)
pattern EventTypeMouseWheel = (#const SDL_MOUSEWHEEL)
pattern EventTypeJoyAxisMotion = (#const SDL_JOYAXISMOTION)
pattern EventTypeJoyBallMotion = (#const SDL_JOYBALLMOTION)
pattern EventTypeJoyHatMotion = (#const SDL_JOYHATMOTION)
pattern EventTypeJoyButtonDown = (#const SDL_JOYBUTTONDOWN)
pattern EventTypeJoyButtonUp = (#const SDL_JOYBUTTONUP)
pattern EventTypeJoyDeviceAdded = (#const SDL_JOYDEVICEADDED)
pattern EventTypeJoyDeviceRemoved = (#const SDL_JOYDEVICEREMOVED)
pattern EventTypeControllerAxisMotion = (#const SDL_CONTROLLERAXISMOTION)
pattern EventTypeControllerButtonDown = (#const SDL_CONTROLLERBUTTONDOWN)
pattern EventTypeControllerButtonUp = (#const SDL_CONTROLLERBUTTONUP)
pattern EventTypeControllerDeviceAdded = (#const SDL_CONTROLLERDEVICEADDED)
pattern EventTypeControllerDeviceRemoved = (#const SDL_CONTROLLERDEVICEREMOVED)
pattern EventTypeControllerDeviceRemapped = (#const SDL_CONTROLLERDEVICEREMAPPED)
pattern EventTypeFingerDown = (#const SDL_FINGERDOWN)
pattern EventTypeFingerUp = (#const SDL_FINGERUP)
pattern EventTypeFingerMotion = (#const SDL_FINGERMOTION)
pattern EventTypeDollarGesture = (#const SDL_DOLLARGESTURE)
pattern EventTypeDollarRecord = (#const SDL_DOLLARRECORD)
pattern EventTypeMultiGesture = (#const SDL_MULTIGESTURE)
pattern EventTypeClipboardUpdate = (#const SDL_CLIPBOARDUPDATE)
pattern EventTypeDropFile = (#const SDL_DROPFILE)
pattern EventTypeUserEvent = (#const SDL_USEREVENT)
pattern EventTypeLastEvent = (#const SDL_LASTEVENT)

pattern InitFlagTimer = (#const SDL_INIT_TIMER)
pattern InitFlagAudio = (#const SDL_INIT_AUDIO)
pattern InitFlagVideo = (#const SDL_INIT_VIDEO)
pattern InitFlagJoystick = (#const SDL_INIT_JOYSTICK)
pattern InitFlagHaptic = (#const SDL_INIT_HAPTIC)
pattern InitFlagGameController = (#const SDL_INIT_GAMECONTROLLER)
pattern InitFlagEvents = (#const SDL_INIT_EVENTS)
pattern InitFlagNoParachute = (#const SDL_INIT_NOPARACHUTE)
pattern InitFlagEverything = (#const SDL_INIT_EVERYTHING)

pattern JoystickHatCentered = (#const SDL_HAT_CENTERED)
pattern JoystickHatUp = (#const SDL_HAT_UP)
pattern JoystickHatRight = (#const SDL_HAT_RIGHT)
pattern JoystickHatDown = (#const SDL_HAT_DOWN)
pattern JoystickHatLeft = (#const SDL_HAT_LEFT)
pattern JoystickHatRightUp = (#const SDL_HAT_RIGHTUP)
pattern JoystickHatRightDown = (#const SDL_HAT_RIGHTDOWN)
pattern JoystickHatLeftUp = (#const SDL_HAT_LEFTUP)
pattern JoystickHatLeftDown = (#const SDL_HAT_LEFTDOWN)

pattern LogCategoryApplication = (#const SDL_LOG_CATEGORY_APPLICATION)
pattern LogCategoryError = (#const SDL_LOG_CATEGORY_ERROR)
pattern LogCategoryAssert = (#const SDL_LOG_CATEGORY_ASSERT)
pattern LogCategorySystem = (#const SDL_LOG_CATEGORY_SYSTEM)
pattern LogCategoryAudio = (#const SDL_LOG_CATEGORY_AUDIO)
pattern LogCategoryVideo = (#const SDL_LOG_CATEGORY_VIDEO)
pattern LogCategoryRender = (#const SDL_LOG_CATEGORY_RENDER)
pattern LogCategoryInput = (#const SDL_LOG_CATEGORY_INPUT)
pattern LogCategoryTest = (#const SDL_LOG_CATEGORY_TEST)
pattern LogCategoryCustom = (#const SDL_LOG_CATEGORY_CUSTOM)

pattern MessageBoxFlagError = (#const SDL_MESSAGEBOX_ERROR)
pattern MessageBoxFlagWarning = (#const SDL_MESSAGEBOX_WARNING)
pattern MessageBoxFlagInformation = (#const SDL_MESSAGEBOX_INFORMATION)

pattern MessageBoxButtonFlagReturnKeyDefault = (#const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT)
pattern MessageBoxButtonFlagEscapeKeyDefault = (#const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT)

pattern GLProfileCore = (#const SDL_GL_CONTEXT_PROFILE_CORE)
pattern GLProfileCompatibility = (#const SDL_GL_CONTEXT_PROFILE_COMPATIBILITY)
pattern GLProfileES = (#const SDL_GL_CONTEXT_PROFILE_ES)

pattern GLContextFlagDebug = (#const SDL_GL_CONTEXT_DEBUG_FLAG)
pattern GLContextFlagForwardCompatible = (#const SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG)
pattern GLContextFlagRobustAccess = (#const SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG)
pattern GLContextFlagResetIsolation = (#const SDL_GL_CONTEXT_RESET_ISOLATION_FLAG)

pattern PixelFormatUnknown = (#const SDL_PIXELFORMAT_UNKNOWN)
pattern PixelFormatIndex1LSB = (#const SDL_PIXELFORMAT_INDEX1LSB)
pattern PixelFormatIndex1MSB = (#const SDL_PIXELFORMAT_INDEX1MSB)
pattern PixelFormatIndex4LSB = (#const SDL_PIXELFORMAT_INDEX4LSB)
pattern PixelFormatIndex4MSB = (#const SDL_PIXELFORMAT_INDEX4MSB)
pattern PixelFormatIndex8 = (#const SDL_PIXELFORMAT_INDEX8)
pattern PixelFormatRGB332 = (#const SDL_PIXELFORMAT_RGB332)
pattern PixelFormatRGB444 = (#const SDL_PIXELFORMAT_RGB444)
pattern PixelFormatRGB555 = (#const SDL_PIXELFORMAT_RGB555)
pattern PixelFormatBGR555 = (#const SDL_PIXELFORMAT_BGR555)
pattern PixelFormatARGB4444 = (#const SDL_PIXELFORMAT_ARGB4444)
pattern PixelFormatRGBA4444 = (#const SDL_PIXELFORMAT_RGBA4444)
pattern PixelFormatABGR4444 = (#const SDL_PIXELFORMAT_ABGR4444)
pattern PixelFormatBGRA4444 = (#const SDL_PIXELFORMAT_BGRA4444)
pattern PixelFormatARGB1555 = (#const SDL_PIXELFORMAT_ARGB1555)
pattern PixelFormatRGBA5551 = (#const SDL_PIXELFORMAT_RGBA5551)
pattern PixelFormatABGR1555 = (#const SDL_PIXELFORMAT_ABGR1555)
pattern PixelFormatBGRA5551 = (#const SDL_PIXELFORMAT_BGRA5551)
pattern PixelFormatRGB565 = (#const SDL_PIXELFORMAT_RGB565)
pattern PixelFormatBGR565 = (#const SDL_PIXELFORMAT_BGR565)
pattern PixelFormatRGB24 = (#const SDL_PIXELFORMAT_RGB24)
pattern PixelFormatBGR24 = (#const SDL_PIXELFORMAT_BGR24)
pattern PixelFormatRGB888 = (#const SDL_PIXELFORMAT_RGB888)
pattern PixelFormatRGBX8888 = (#const SDL_PIXELFORMAT_RGBX8888)
pattern PixelFormatBGR888 = (#const SDL_PIXELFORMAT_BGR888)
pattern PixelFormatBGRX8888 = (#const SDL_PIXELFORMAT_BGRX8888)
pattern PixelFormatARGB8888 = (#const SDL_PIXELFORMAT_ARGB8888)
pattern PixelFormatRGBA8888 = (#const SDL_PIXELFORMAT_RGBA8888)
pattern PixelFormatABGR8888 = (#const SDL_PIXELFORMAT_ABGR8888)
pattern PixelFormatBGRA8888 = (#const SDL_PIXELFORMAT_BGRA8888)
pattern PixelFormatARGB2101010 = (#const SDL_PIXELFORMAT_ARGB2101010)
pattern PixelFormatYV12 = (#const SDL_PIXELFORMAT_YV12)
pattern PixelFormatIYUV = (#const SDL_PIXELFORMAT_IYUV)
pattern PixelFormatYUY2 = (#const SDL_PIXELFORMAT_YUY2)
pattern PixelFormatUYVY = (#const SDL_PIXELFORMAT_UYVY)
pattern PixelFormatYVYU = (#const SDL_PIXELFORMAT_YVYU)

pattern RendererFlagSoftware = (#const SDL_RENDERER_SOFTWARE)
pattern RendererFlagAccelerated = (#const SDL_RENDERER_ACCELERATED)
pattern RendererFlagPresentVSync = (#const SDL_RENDERER_PRESENTVSYNC)
pattern RendererFlagTargetTexture = (#const SDL_RENDERER_TARGETTEXTURE)

pattern TextureAccessStatic = (#const SDL_TEXTUREACCESS_STATIC)
pattern TextureAccessStreaming = (#const SDL_TEXTUREACCESS_STREAMING)
pattern TextureAccessTarget = (#const SDL_TEXTUREACCESS_TARGET)

pattern TextureModulateNone = (#const SDL_TEXTUREMODULATE_NONE)
pattern TextureModulateColor = (#const SDL_TEXTUREMODULATE_COLOR)
pattern TextureModulateAlpha = (#const SDL_TEXTUREMODULATE_ALPHA)

pattern WindowEventNone = (#const SDL_WINDOWEVENT_NONE)
pattern WindowEventShown = (#const SDL_WINDOWEVENT_SHOWN)
pattern WindowEventHidden = (#const SDL_WINDOWEVENT_HIDDEN)
pattern WindowEventExposed = (#const SDL_WINDOWEVENT_EXPOSED)
pattern WindowEventMoved = (#const SDL_WINDOWEVENT_MOVED)
pattern WindowEventResized = (#const SDL_WINDOWEVENT_RESIZED)
pattern WindowEventSizeChanged = (#const SDL_WINDOWEVENT_SIZE_CHANGED)
pattern WindowEventMinimized = (#const SDL_WINDOWEVENT_MINIMIZED)
pattern WindowEventMaximized = (#const SDL_WINDOWEVENT_MAXIMIZED)
pattern WindowEventRestored = (#const SDL_WINDOWEVENT_RESTORED)
pattern WindowEventEnter = (#const SDL_WINDOWEVENT_ENTER)
pattern WindowEventLeave = (#const SDL_WINDOWEVENT_LEAVE)
pattern WindowEventFocusGained = (#const SDL_WINDOWEVENT_FOCUS_GAINED)
pattern WindowEventFocusLost = (#const SDL_WINDOWEVENT_FOCUS_LOST)
pattern WindowEventClose = (#const SDL_WINDOWEVENT_CLOSE)

pattern WindowFlagFullscreen = (#const SDL_WINDOW_FULLSCREEN)
pattern WindowFlagOpenGL = (#const SDL_WINDOW_OPENGL)
pattern WindowFlagShown = (#const SDL_WINDOW_SHOWN)
pattern WindowFlagHidden = (#const SDL_WINDOW_HIDDEN)
pattern WindowFlagBorderless = (#const SDL_WINDOW_BORDERLESS)
pattern WindowFlagResizable = (#const SDL_WINDOW_RESIZABLE)
pattern WindowFlagMinimized = (#const SDL_WINDOW_MINIMIZED)
pattern WindowFlagMaximized = (#const SDL_WINDOW_MAXIMIZED)
pattern WindowFlagInputGrabbed = (#const SDL_WINDOW_INPUT_GRABBED)
pattern WindowFlagInputFocus = (#const SDL_WINDOW_INPUT_FOCUS)
pattern WindowFlagMouseFocus = (#const SDL_WINDOW_MOUSE_FOCUS)
pattern WindowFlagFullscreenDesktop = (#const SDL_WINDOW_FULLSCREEN_DESKTOP)
pattern WindowFlagForeign = (#const SDL_WINDOW_FOREIGN)
pattern WindowFlagAllowHighDPI = (#const SDL_WINDOW_ALLOW_HIGHDPI)

pattern WindowPosUndefined = (#const SDL_WINDOWPOS_UNDEFINED)
pattern WindowPosCentered = (#const SDL_WINDOWPOS_CENTERED)
