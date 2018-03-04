{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Raw.Enum (
  -- * Enumerations

  -- ** Audio Format
  AudioFormat,
  pattern SDL_AUDIO_S8,
  pattern SDL_AUDIO_U8,
  pattern SDL_AUDIO_S16LSB,
  pattern SDL_AUDIO_S16MSB,
  pattern SDL_AUDIO_S16SYS,
  pattern SDL_AUDIO_U16LSB,
  pattern SDL_AUDIO_U16MSB,
  pattern SDL_AUDIO_U16SYS,
  pattern SDL_AUDIO_S32LSB,
  pattern SDL_AUDIO_S32MSB,
  pattern SDL_AUDIO_S32SYS,
  pattern SDL_AUDIO_F32LSB,
  pattern SDL_AUDIO_F32MSB,
  pattern SDL_AUDIO_F32SYS,

  -- ** Audio Status
  AudioStatus,
  pattern SDL_AUDIO_STOPPED,
  pattern SDL_AUDIO_PLAYING,
  pattern SDL_AUDIO_PAUSED,

  -- ** Blend Mode
  BlendMode,
  pattern SDL_BLENDMODE_NONE,
  pattern SDL_BLENDMODE_BLEND,
  pattern SDL_BLENDMODE_ADD,
  pattern SDL_BLENDMODE_MOD,

  -- ** Endian Detetection
  pattern SDL_BYTEORDER,
  pattern SDL_LIL_ENDIAN,
  pattern SDL_BIG_ENDIAN,

  -- ** Event Action
  EventAction,
  pattern SDL_ADDEVENT,
  pattern SDL_PEEKEVENT,
  pattern SDL_GETEVENT,

  -- ** Game Controller Axis
  GameControllerAxis,
  pattern SDL_CONTROLLER_AXIS_INVALID,
  pattern SDL_CONTROLLER_AXIS_LEFTX,
  pattern SDL_CONTROLLER_AXIS_LEFTY,
  pattern SDL_CONTROLLER_AXIS_RIGHTX,
  pattern SDL_CONTROLLER_AXIS_RIGHTY,
  pattern SDL_CONTROLLER_AXIS_TRIGGERLEFT,
  pattern SDL_CONTROLLER_AXIS_TRIGGERRIGHT,
  pattern SDL_CONTROLLER_AXIS_MAX,

  -- ** Game Controller Button
  GameControllerButton,
  pattern SDL_CONTROLLER_BUTTON_INVALID,
  pattern SDL_CONTROLLER_BUTTON_A,
  pattern SDL_CONTROLLER_BUTTON_B,
  pattern SDL_CONTROLLER_BUTTON_X,
  pattern SDL_CONTROLLER_BUTTON_Y,
  pattern SDL_CONTROLLER_BUTTON_BACK,
  pattern SDL_CONTROLLER_BUTTON_GUIDE,
  pattern SDL_CONTROLLER_BUTTON_START,
  pattern SDL_CONTROLLER_BUTTON_LEFTSTICK,
  pattern SDL_CONTROLLER_BUTTON_RIGHTSTICK,
  pattern SDL_CONTROLLER_BUTTON_LEFTSHOULDER,
  pattern SDL_CONTROLLER_BUTTON_RIGHTSHOULDER,
  pattern SDL_CONTROLLER_BUTTON_DPAD_UP,
  pattern SDL_CONTROLLER_BUTTON_DPAD_DOWN,
  pattern SDL_CONTROLLER_BUTTON_DPAD_LEFT,
  pattern SDL_CONTROLLER_BUTTON_DPAD_RIGHT,
  pattern SDL_CONTROLLER_BUTTON_MAX,

  -- ** OpenGL Attribute
  GLattr,
  pattern SDL_GL_RED_SIZE,
  pattern SDL_GL_GREEN_SIZE,
  pattern SDL_GL_BLUE_SIZE,
  pattern SDL_GL_ALPHA_SIZE,
  pattern SDL_GL_BUFFER_SIZE,
  pattern SDL_GL_DOUBLEBUFFER,
  pattern SDL_GL_DEPTH_SIZE,
  pattern SDL_GL_STENCIL_SIZE,
  pattern SDL_GL_ACCUM_RED_SIZE,
  pattern SDL_GL_ACCUM_GREEN_SIZE,
  pattern SDL_GL_ACCUM_BLUE_SIZE,
  pattern SDL_GL_ACCUM_ALPHA_SIZE,
  pattern SDL_GL_STEREO,
  pattern SDL_GL_MULTISAMPLEBUFFERS,
  pattern SDL_GL_MULTISAMPLESAMPLES,
  pattern SDL_GL_ACCELERATED_VISUAL,
  pattern SDL_GL_RETAINED_BACKING,
  pattern SDL_GL_CONTEXT_MAJOR_VERSION,
  pattern SDL_GL_CONTEXT_MINOR_VERSION,
  pattern SDL_GL_CONTEXT_EGL,
  pattern SDL_GL_CONTEXT_FLAGS,
  pattern SDL_GL_CONTEXT_PROFILE_MASK,
  pattern SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
  pattern SDL_GL_FRAMEBUFFER_SRGB_CAPABLE,
  pattern SDL_GL_CONTEXT_RELEASE_BEHAVIOR,

  -- ** Hint Priority
  HintPriority,
  pattern SDL_HINT_DEFAULT,
  pattern SDL_HINT_NORMAL,
  pattern SDL_HINT_OVERRIDE,

  -- ** Initialization Flag
  InitFlag,
  pattern SDL_INIT_TIMER,
  pattern SDL_INIT_AUDIO,
  pattern SDL_INIT_VIDEO,
  pattern SDL_INIT_JOYSTICK,
  pattern SDL_INIT_HAPTIC,
  pattern SDL_INIT_GAMECONTROLLER,
  pattern SDL_INIT_EVENTS,
  pattern SDL_INIT_NOPARACHUTE,
  pattern SDL_INIT_EVERYTHING,

  -- ** Joystick Power Level
  JoystickPowerLevel,
  pattern SDL_JOYSTICK_POWER_UNKNOWN,
  pattern SDL_JOYSTICK_POWER_EMPTY,
  pattern SDL_JOYSTICK_POWER_LOW,
  pattern SDL_JOYSTICK_POWER_MEDIUM,
  pattern SDL_JOYSTICK_POWER_FULL,
  pattern SDL_JOYSTICK_POWER_WIRED,
  pattern SDL_JOYSTICK_POWER_MAX,

  -- ** Keycode
  Keycode,
  pattern SDLK_UNKNOWN,
  pattern SDLK_RETURN,
  pattern SDLK_ESCAPE,
  pattern SDLK_BACKSPACE,
  pattern SDLK_TAB,
  pattern SDLK_SPACE,
  pattern SDLK_EXCLAIM,
  pattern SDLK_QUOTEDBL,
  pattern SDLK_HASH,
  pattern SDLK_PERCENT,
  pattern SDLK_DOLLAR,
  pattern SDLK_AMPERSAND,
  pattern SDLK_QUOTE,
  pattern SDLK_LEFTPAREN,
  pattern SDLK_RIGHTPAREN,
  pattern SDLK_ASTERISK,
  pattern SDLK_PLUS,
  pattern SDLK_COMMA,
  pattern SDLK_MINUS,
  pattern SDLK_PERIOD,
  pattern SDLK_SLASH,
  pattern SDLK_0,
  pattern SDLK_1,
  pattern SDLK_2,
  pattern SDLK_3,
  pattern SDLK_4,
  pattern SDLK_5,
  pattern SDLK_6,
  pattern SDLK_7,
  pattern SDLK_8,
  pattern SDLK_9,
  pattern SDLK_COLON,
  pattern SDLK_SEMICOLON,
  pattern SDLK_LESS,
  pattern SDLK_EQUALS,
  pattern SDLK_GREATER,
  pattern SDLK_QUESTION,
  pattern SDLK_AT,
  pattern SDLK_LEFTBRACKET,
  pattern SDLK_BACKSLASH,
  pattern SDLK_RIGHTBRACKET,
  pattern SDLK_CARET,
  pattern SDLK_UNDERSCORE,
  pattern SDLK_BACKQUOTE,
  pattern SDLK_a,
  pattern SDLK_b,
  pattern SDLK_c,
  pattern SDLK_d,
  pattern SDLK_e,
  pattern SDLK_f,
  pattern SDLK_g,
  pattern SDLK_h,
  pattern SDLK_i,
  pattern SDLK_j,
  pattern SDLK_k,
  pattern SDLK_l,
  pattern SDLK_m,
  pattern SDLK_n,
  pattern SDLK_o,
  pattern SDLK_p,
  pattern SDLK_q,
  pattern SDLK_r,
  pattern SDLK_s,
  pattern SDLK_t,
  pattern SDLK_u,
  pattern SDLK_v,
  pattern SDLK_w,
  pattern SDLK_x,
  pattern SDLK_y,
  pattern SDLK_z,
  pattern SDLK_CAPSLOCK,
  pattern SDLK_F1,
  pattern SDLK_F2,
  pattern SDLK_F3,
  pattern SDLK_F4,
  pattern SDLK_F5,
  pattern SDLK_F6,
  pattern SDLK_F7,
  pattern SDLK_F8,
  pattern SDLK_F9,
  pattern SDLK_F10,
  pattern SDLK_F11,
  pattern SDLK_F12,
  pattern SDLK_PRINTSCREEN,
  pattern SDLK_SCROLLLOCK,
  pattern SDLK_PAUSE,
  pattern SDLK_INSERT,
  pattern SDLK_HOME,
  pattern SDLK_PAGEUP,
  pattern SDLK_DELETE,
  pattern SDLK_END,
  pattern SDLK_PAGEDOWN,
  pattern SDLK_RIGHT,
  pattern SDLK_LEFT,
  pattern SDLK_DOWN,
  pattern SDLK_UP,
  pattern SDLK_NUMLOCKCLEAR,
  pattern SDLK_KP_DIVIDE,
  pattern SDLK_KP_MULTIPLY,
  pattern SDLK_KP_MINUS,
  pattern SDLK_KP_PLUS,
  pattern SDLK_KP_ENTER,
  pattern SDLK_KP_1,
  pattern SDLK_KP_2,
  pattern SDLK_KP_3,
  pattern SDLK_KP_4,
  pattern SDLK_KP_5,
  pattern SDLK_KP_6,
  pattern SDLK_KP_7,
  pattern SDLK_KP_8,
  pattern SDLK_KP_9,
  pattern SDLK_KP_0,
  pattern SDLK_KP_PERIOD,
  pattern SDLK_APPLICATION,
  pattern SDLK_POWER,
  pattern SDLK_KP_EQUALS,
  pattern SDLK_F13,
  pattern SDLK_F14,
  pattern SDLK_F15,
  pattern SDLK_F16,
  pattern SDLK_F17,
  pattern SDLK_F18,
  pattern SDLK_F19,
  pattern SDLK_F20,
  pattern SDLK_F21,
  pattern SDLK_F22,
  pattern SDLK_F23,
  pattern SDLK_F24,
  pattern SDLK_EXECUTE,
  pattern SDLK_HELP,
  pattern SDLK_MENU,
  pattern SDLK_SELECT,
  pattern SDLK_STOP,
  pattern SDLK_AGAIN,
  pattern SDLK_UNDO,
  pattern SDLK_CUT,
  pattern SDLK_COPY,
  pattern SDLK_PASTE,
  pattern SDLK_FIND,
  pattern SDLK_MUTE,
  pattern SDLK_VOLUMEUP,
  pattern SDLK_VOLUMEDOWN,
  pattern SDLK_KP_COMMA,
  pattern SDLK_KP_EQUALSAS400,
  pattern SDLK_ALTERASE,
  pattern SDLK_SYSREQ,
  pattern SDLK_CANCEL,
  pattern SDLK_CLEAR,
  pattern SDLK_PRIOR,
  pattern SDLK_RETURN2,
  pattern SDLK_SEPARATOR,
  pattern SDLK_OUT,
  pattern SDLK_OPER,
  pattern SDLK_CLEARAGAIN,
  pattern SDLK_CRSEL,
  pattern SDLK_EXSEL,
  pattern SDLK_KP_00,
  pattern SDLK_KP_000,
  pattern SDLK_THOUSANDSSEPARATOR,
  pattern SDLK_DECIMALSEPARATOR,
  pattern SDLK_CURRENCYUNIT,
  pattern SDLK_CURRENCYSUBUNIT,
  pattern SDLK_KP_LEFTPAREN,
  pattern SDLK_KP_RIGHTPAREN,
  pattern SDLK_KP_LEFTBRACE,
  pattern SDLK_KP_RIGHTBRACE,
  pattern SDLK_KP_TAB,
  pattern SDLK_KP_BACKSPACE,
  pattern SDLK_KP_A,
  pattern SDLK_KP_B,
  pattern SDLK_KP_C,
  pattern SDLK_KP_D,
  pattern SDLK_KP_E,
  pattern SDLK_KP_F,
  pattern SDLK_KP_XOR,
  pattern SDLK_KP_POWER,
  pattern SDLK_KP_PERCENT,
  pattern SDLK_KP_LESS,
  pattern SDLK_KP_GREATER,
  pattern SDLK_KP_AMPERSAND,
  pattern SDLK_KP_DBLAMPERSAND,
  pattern SDLK_KP_VERTICALBAR,
  pattern SDLK_KP_DBLVERTICALBAR,
  pattern SDLK_KP_COLON,
  pattern SDLK_KP_HASH,
  pattern SDLK_KP_SPACE,
  pattern SDLK_KP_AT,
  pattern SDLK_KP_EXCLAM,
  pattern SDLK_KP_MEMSTORE,
  pattern SDLK_KP_MEMRECALL,
  pattern SDLK_KP_MEMCLEAR,
  pattern SDLK_KP_MEMADD,
  pattern SDLK_KP_MEMSUBTRACT,
  pattern SDLK_KP_MEMMULTIPLY,
  pattern SDLK_KP_MEMDIVIDE,
  pattern SDLK_KP_PLUSMINUS,
  pattern SDLK_KP_CLEAR,
  pattern SDLK_KP_CLEARENTRY,
  pattern SDLK_KP_BINARY,
  pattern SDLK_KP_OCTAL,
  pattern SDLK_KP_DECIMAL,
  pattern SDLK_KP_HEXADECIMAL,
  pattern SDLK_LCTRL,
  pattern SDLK_LSHIFT,
  pattern SDLK_LALT,
  pattern SDLK_LGUI,
  pattern SDLK_RCTRL,
  pattern SDLK_RSHIFT,
  pattern SDLK_RALT,
  pattern SDLK_RGUI,
  pattern SDLK_MODE,
  pattern SDLK_AUDIONEXT,
  pattern SDLK_AUDIOPREV,
  pattern SDLK_AUDIOSTOP,
  pattern SDLK_AUDIOPLAY,
  pattern SDLK_AUDIOMUTE,
  pattern SDLK_MEDIASELECT,
  pattern SDLK_WWW,
  pattern SDLK_MAIL,
  pattern SDLK_CALCULATOR,
  pattern SDLK_COMPUTER,
  pattern SDLK_AC_SEARCH,
  pattern SDLK_AC_HOME,
  pattern SDLK_AC_BACK,
  pattern SDLK_AC_FORWARD,
  pattern SDLK_AC_STOP,
  pattern SDLK_AC_REFRESH,
  pattern SDLK_AC_BOOKMARKS,
  pattern SDLK_BRIGHTNESSDOWN,
  pattern SDLK_BRIGHTNESSUP,
  pattern SDLK_DISPLAYSWITCH,
  pattern SDLK_KBDILLUMTOGGLE,
  pattern SDLK_KBDILLUMDOWN,
  pattern SDLK_KBDILLUMUP,
  pattern SDLK_EJECT,
  pattern SDLK_SLEEP,

  -- ** Key Modifier
  Keymod,
  pattern KMOD_NONE,
  pattern KMOD_LSHIFT,
  pattern KMOD_RSHIFT,
  pattern KMOD_SHIFT,
  pattern KMOD_LCTRL,
  pattern KMOD_RCTRL,
  pattern KMOD_CTRL,
  pattern KMOD_LALT,
  pattern KMOD_RALT,
  pattern KMOD_ALT,
  pattern KMOD_LGUI,
  pattern KMOD_RGUI,
  pattern KMOD_GUI,
  pattern KMOD_NUM,
  pattern KMOD_CAPS,
  pattern KMOD_MODE,
  pattern KMOD_RESERVED,

  -- ** Log Priority
  LogPriority,
  pattern SDL_LOG_PRIORITY_VERBOSE,
  pattern SDL_LOG_PRIORITY_DEBUG,
  pattern SDL_LOG_PRIORITY_INFO,
  pattern SDL_LOG_PRIORITY_WARN,
  pattern SDL_LOG_PRIORITY_ERROR,
  pattern SDL_LOG_PRIORITY_CRITICAL,
  pattern SDL_NUM_LOG_PRIORITIES,

  -- ** Power State
  PowerState,
  pattern SDL_POWERSTATE_UNKNOWN,
  pattern SDL_POWERSTATE_ON_BATTERY,
  pattern SDL_POWERSTATE_NO_BATTERY,
  pattern SDL_POWERSTATE_CHARGING,
  pattern SDL_POWERSTATE_CHARGED,

  -- ** Renderer Flip
  RendererFlip,
  pattern SDL_FLIP_NONE,
  pattern SDL_FLIP_HORIZONTAL,
  pattern SDL_FLIP_VERTICAL,

  -- ** Scancode
  Scancode,
  pattern SDL_SCANCODE_UNKNOWN,
  pattern SDL_SCANCODE_A,
  pattern SDL_SCANCODE_B,
  pattern SDL_SCANCODE_C,
  pattern SDL_SCANCODE_D,
  pattern SDL_SCANCODE_E,
  pattern SDL_SCANCODE_F,
  pattern SDL_SCANCODE_G,
  pattern SDL_SCANCODE_H,
  pattern SDL_SCANCODE_I,
  pattern SDL_SCANCODE_J,
  pattern SDL_SCANCODE_K,
  pattern SDL_SCANCODE_L,
  pattern SDL_SCANCODE_M,
  pattern SDL_SCANCODE_N,
  pattern SDL_SCANCODE_O,
  pattern SDL_SCANCODE_P,
  pattern SDL_SCANCODE_Q,
  pattern SDL_SCANCODE_R,
  pattern SDL_SCANCODE_S,
  pattern SDL_SCANCODE_T,
  pattern SDL_SCANCODE_U,
  pattern SDL_SCANCODE_V,
  pattern SDL_SCANCODE_W,
  pattern SDL_SCANCODE_X,
  pattern SDL_SCANCODE_Y,
  pattern SDL_SCANCODE_Z,
  pattern SDL_SCANCODE_1,
  pattern SDL_SCANCODE_2,
  pattern SDL_SCANCODE_3,
  pattern SDL_SCANCODE_4,
  pattern SDL_SCANCODE_5,
  pattern SDL_SCANCODE_6,
  pattern SDL_SCANCODE_7,
  pattern SDL_SCANCODE_8,
  pattern SDL_SCANCODE_9,
  pattern SDL_SCANCODE_0,
  pattern SDL_SCANCODE_RETURN,
  pattern SDL_SCANCODE_ESCAPE,
  pattern SDL_SCANCODE_BACKSPACE,
  pattern SDL_SCANCODE_TAB,
  pattern SDL_SCANCODE_SPACE,
  pattern SDL_SCANCODE_MINUS,
  pattern SDL_SCANCODE_EQUALS,
  pattern SDL_SCANCODE_LEFTBRACKET,
  pattern SDL_SCANCODE_RIGHTBRACKET,
  pattern SDL_SCANCODE_BACKSLASH,
  pattern SDL_SCANCODE_NONUSHASH,
  pattern SDL_SCANCODE_SEMICOLON,
  pattern SDL_SCANCODE_APOSTROPHE,
  pattern SDL_SCANCODE_GRAVE,
  pattern SDL_SCANCODE_COMMA,
  pattern SDL_SCANCODE_PERIOD,
  pattern SDL_SCANCODE_SLASH,
  pattern SDL_SCANCODE_CAPSLOCK,
  pattern SDL_SCANCODE_F1,
  pattern SDL_SCANCODE_F2,
  pattern SDL_SCANCODE_F3,
  pattern SDL_SCANCODE_F4,
  pattern SDL_SCANCODE_F5,
  pattern SDL_SCANCODE_F6,
  pattern SDL_SCANCODE_F7,
  pattern SDL_SCANCODE_F8,
  pattern SDL_SCANCODE_F9,
  pattern SDL_SCANCODE_F10,
  pattern SDL_SCANCODE_F11,
  pattern SDL_SCANCODE_F12,
  pattern SDL_SCANCODE_PRINTSCREEN,
  pattern SDL_SCANCODE_SCROLLLOCK,
  pattern SDL_SCANCODE_PAUSE,
  pattern SDL_SCANCODE_INSERT,
  pattern SDL_SCANCODE_HOME,
  pattern SDL_SCANCODE_PAGEUP,
  pattern SDL_SCANCODE_DELETE,
  pattern SDL_SCANCODE_END,
  pattern SDL_SCANCODE_PAGEDOWN,
  pattern SDL_SCANCODE_RIGHT,
  pattern SDL_SCANCODE_LEFT,
  pattern SDL_SCANCODE_DOWN,
  pattern SDL_SCANCODE_UP,
  pattern SDL_SCANCODE_NUMLOCKCLEAR,
  pattern SDL_SCANCODE_KP_DIVIDE,
  pattern SDL_SCANCODE_KP_MULTIPLY,
  pattern SDL_SCANCODE_KP_MINUS,
  pattern SDL_SCANCODE_KP_PLUS,
  pattern SDL_SCANCODE_KP_ENTER,
  pattern SDL_SCANCODE_KP_1,
  pattern SDL_SCANCODE_KP_2,
  pattern SDL_SCANCODE_KP_3,
  pattern SDL_SCANCODE_KP_4,
  pattern SDL_SCANCODE_KP_5,
  pattern SDL_SCANCODE_KP_6,
  pattern SDL_SCANCODE_KP_7,
  pattern SDL_SCANCODE_KP_8,
  pattern SDL_SCANCODE_KP_9,
  pattern SDL_SCANCODE_KP_0,
  pattern SDL_SCANCODE_KP_PERIOD,
  pattern SDL_SCANCODE_NONUSBACKSLASH,
  pattern SDL_SCANCODE_APPLICATION,
  pattern SDL_SCANCODE_POWER,
  pattern SDL_SCANCODE_KP_EQUALS,
  pattern SDL_SCANCODE_F13,
  pattern SDL_SCANCODE_F14,
  pattern SDL_SCANCODE_F15,
  pattern SDL_SCANCODE_F16,
  pattern SDL_SCANCODE_F17,
  pattern SDL_SCANCODE_F18,
  pattern SDL_SCANCODE_F19,
  pattern SDL_SCANCODE_F20,
  pattern SDL_SCANCODE_F21,
  pattern SDL_SCANCODE_F22,
  pattern SDL_SCANCODE_F23,
  pattern SDL_SCANCODE_F24,
  pattern SDL_SCANCODE_EXECUTE,
  pattern SDL_SCANCODE_HELP,
  pattern SDL_SCANCODE_MENU,
  pattern SDL_SCANCODE_SELECT,
  pattern SDL_SCANCODE_STOP,
  pattern SDL_SCANCODE_AGAIN,
  pattern SDL_SCANCODE_UNDO,
  pattern SDL_SCANCODE_CUT,
  pattern SDL_SCANCODE_COPY,
  pattern SDL_SCANCODE_PASTE,
  pattern SDL_SCANCODE_FIND,
  pattern SDL_SCANCODE_MUTE,
  pattern SDL_SCANCODE_VOLUMEUP,
  pattern SDL_SCANCODE_VOLUMEDOWN,
  pattern SDL_SCANCODE_KP_COMMA,
  pattern SDL_SCANCODE_KP_EQUALSAS400,
  pattern SDL_SCANCODE_INTERNATIONAL1,
  pattern SDL_SCANCODE_INTERNATIONAL2,
  pattern SDL_SCANCODE_INTERNATIONAL3,
  pattern SDL_SCANCODE_INTERNATIONAL4,
  pattern SDL_SCANCODE_INTERNATIONAL5,
  pattern SDL_SCANCODE_INTERNATIONAL6,
  pattern SDL_SCANCODE_INTERNATIONAL7,
  pattern SDL_SCANCODE_INTERNATIONAL8,
  pattern SDL_SCANCODE_INTERNATIONAL9,
  pattern SDL_SCANCODE_LANG1,
  pattern SDL_SCANCODE_LANG2,
  pattern SDL_SCANCODE_LANG3,
  pattern SDL_SCANCODE_LANG4,
  pattern SDL_SCANCODE_LANG5,
  pattern SDL_SCANCODE_LANG6,
  pattern SDL_SCANCODE_LANG7,
  pattern SDL_SCANCODE_LANG8,
  pattern SDL_SCANCODE_LANG9,
  pattern SDL_SCANCODE_ALTERASE,
  pattern SDL_SCANCODE_SYSREQ,
  pattern SDL_SCANCODE_CANCEL,
  pattern SDL_SCANCODE_CLEAR,
  pattern SDL_SCANCODE_PRIOR,
  pattern SDL_SCANCODE_RETURN2,
  pattern SDL_SCANCODE_SEPARATOR,
  pattern SDL_SCANCODE_OUT,
  pattern SDL_SCANCODE_OPER,
  pattern SDL_SCANCODE_CLEARAGAIN,
  pattern SDL_SCANCODE_CRSEL,
  pattern SDL_SCANCODE_EXSEL,
  pattern SDL_SCANCODE_KP_00,
  pattern SDL_SCANCODE_KP_000,
  pattern SDL_SCANCODE_THOUSANDSSEPARATOR,
  pattern SDL_SCANCODE_DECIMALSEPARATOR,
  pattern SDL_SCANCODE_CURRENCYUNIT,
  pattern SDL_SCANCODE_CURRENCYSUBUNIT,
  pattern SDL_SCANCODE_KP_LEFTPAREN,
  pattern SDL_SCANCODE_KP_RIGHTPAREN,
  pattern SDL_SCANCODE_KP_LEFTBRACE,
  pattern SDL_SCANCODE_KP_RIGHTBRACE,
  pattern SDL_SCANCODE_KP_TAB,
  pattern SDL_SCANCODE_KP_BACKSPACE,
  pattern SDL_SCANCODE_KP_A,
  pattern SDL_SCANCODE_KP_B,
  pattern SDL_SCANCODE_KP_C,
  pattern SDL_SCANCODE_KP_D,
  pattern SDL_SCANCODE_KP_E,
  pattern SDL_SCANCODE_KP_F,
  pattern SDL_SCANCODE_KP_XOR,
  pattern SDL_SCANCODE_KP_POWER,
  pattern SDL_SCANCODE_KP_PERCENT,
  pattern SDL_SCANCODE_KP_LESS,
  pattern SDL_SCANCODE_KP_GREATER,
  pattern SDL_SCANCODE_KP_AMPERSAND,
  pattern SDL_SCANCODE_KP_DBLAMPERSAND,
  pattern SDL_SCANCODE_KP_VERTICALBAR,
  pattern SDL_SCANCODE_KP_DBLVERTICALBAR,
  pattern SDL_SCANCODE_KP_COLON,
  pattern SDL_SCANCODE_KP_HASH,
  pattern SDL_SCANCODE_KP_SPACE,
  pattern SDL_SCANCODE_KP_AT,
  pattern SDL_SCANCODE_KP_EXCLAM,
  pattern SDL_SCANCODE_KP_MEMSTORE,
  pattern SDL_SCANCODE_KP_MEMRECALL,
  pattern SDL_SCANCODE_KP_MEMCLEAR,
  pattern SDL_SCANCODE_KP_MEMADD,
  pattern SDL_SCANCODE_KP_MEMSUBTRACT,
  pattern SDL_SCANCODE_KP_MEMMULTIPLY,
  pattern SDL_SCANCODE_KP_MEMDIVIDE,
  pattern SDL_SCANCODE_KP_PLUSMINUS,
  pattern SDL_SCANCODE_KP_CLEAR,
  pattern SDL_SCANCODE_KP_CLEARENTRY,
  pattern SDL_SCANCODE_KP_BINARY,
  pattern SDL_SCANCODE_KP_OCTAL,
  pattern SDL_SCANCODE_KP_DECIMAL,
  pattern SDL_SCANCODE_KP_HEXADECIMAL,
  pattern SDL_SCANCODE_LCTRL,
  pattern SDL_SCANCODE_LSHIFT,
  pattern SDL_SCANCODE_LALT,
  pattern SDL_SCANCODE_LGUI,
  pattern SDL_SCANCODE_RCTRL,
  pattern SDL_SCANCODE_RSHIFT,
  pattern SDL_SCANCODE_RALT,
  pattern SDL_SCANCODE_RGUI,
  pattern SDL_SCANCODE_MODE,
  pattern SDL_SCANCODE_AUDIONEXT,
  pattern SDL_SCANCODE_AUDIOPREV,
  pattern SDL_SCANCODE_AUDIOSTOP,
  pattern SDL_SCANCODE_AUDIOPLAY,
  pattern SDL_SCANCODE_AUDIOMUTE,
  pattern SDL_SCANCODE_MEDIASELECT,
  pattern SDL_SCANCODE_WWW,
  pattern SDL_SCANCODE_MAIL,
  pattern SDL_SCANCODE_CALCULATOR,
  pattern SDL_SCANCODE_COMPUTER,
  pattern SDL_SCANCODE_AC_SEARCH,
  pattern SDL_SCANCODE_AC_HOME,
  pattern SDL_SCANCODE_AC_BACK,
  pattern SDL_SCANCODE_AC_FORWARD,
  pattern SDL_SCANCODE_AC_STOP,
  pattern SDL_SCANCODE_AC_REFRESH,
  pattern SDL_SCANCODE_AC_BOOKMARKS,
  pattern SDL_SCANCODE_BRIGHTNESSDOWN,
  pattern SDL_SCANCODE_BRIGHTNESSUP,
  pattern SDL_SCANCODE_DISPLAYSWITCH,
  pattern SDL_SCANCODE_KBDILLUMTOGGLE,
  pattern SDL_SCANCODE_KBDILLUMDOWN,
  pattern SDL_SCANCODE_KBDILLUMUP,
  pattern SDL_SCANCODE_EJECT,
  pattern SDL_SCANCODE_SLEEP,
  pattern SDL_SCANCODE_APP1,
  pattern SDL_SCANCODE_APP2,
  pattern SDL_NUM_SCANCODES,

  pattern SDL_SYSTEM_CURSOR_ARROW,
  pattern SDL_SYSTEM_CURSOR_IBEAM,
  pattern SDL_SYSTEM_CURSOR_WAIT,

  -- ** System Cursor
  SystemCursor,
  pattern SDL_SYSTEM_CURSOR_CROSSHAIR,
  pattern SDL_SYSTEM_CURSOR_WAITARROW,
  pattern SDL_SYSTEM_CURSOR_SIZENWSE,
  pattern SDL_SYSTEM_CURSOR_SIZENESW,
  pattern SDL_SYSTEM_CURSOR_SIZEWE,
  pattern SDL_SYSTEM_CURSOR_SIZENS,
  pattern SDL_SYSTEM_CURSOR_SIZEALL,
  pattern SDL_SYSTEM_CURSOR_NO,
  pattern SDL_SYSTEM_CURSOR_HAND,
  pattern SDL_NUM_SYSTEM_CURSORS,

  -- ** Thread Priority
  ThreadPriority,
  pattern SDL_THREAD_PRIORITY_LOW,
  pattern SDL_THREAD_PRIORITY_NORMAL,
  pattern SDL_THREAD_PRIORITY_HIGH,

  -- * Miscellaneous Enumerations
  -- | These enumerations are not used directly by any SDL function, thus they have a polymorphic type.

  -- ** Audio Allowed Changes
  pattern SDL_AUDIO_ALLOW_FREQUENCY_CHANGE,
  pattern SDL_AUDIO_ALLOW_FORMAT_CHANGE,
  pattern SDL_AUDIO_ALLOW_CHANNELS_CHANGE,
  pattern SDL_AUDIO_ALLOW_ANY_CHANGE,

  -- ** Mouse Buttons
  pattern SDL_BUTTON_LEFT,
  pattern SDL_BUTTON_MIDDLE,
  pattern SDL_BUTTON_RIGHT,
  pattern SDL_BUTTON_X1,
  pattern SDL_BUTTON_X2,

  -- ** Mouse Button Masks
  pattern SDL_BUTTON_LMASK,
  pattern SDL_BUTTON_MMASK,
  pattern SDL_BUTTON_RMASK,
  pattern SDL_BUTTON_X1MASK,
  pattern SDL_BUTTON_X2MASK,

  -- ** Mouse Wheel Direction
  pattern SDL_MOUSEWHEEL_NORMAL,
  pattern SDL_MOUSEWHEEL_FLIPPED,

  -- ** Event Type
  pattern SDL_FIRSTEVENT,
  pattern SDL_QUIT,
  pattern SDL_APP_TERMINATING,
  pattern SDL_APP_LOWMEMORY,
  pattern SDL_APP_WILLENTERBACKGROUND,
  pattern SDL_APP_DIDENTERBACKGROUND,
  pattern SDL_APP_WILLENTERFOREGROUND,
  pattern SDL_APP_DIDENTERFOREGROUND,
  pattern SDL_WINDOWEVENT,
  pattern SDL_SYSWMEVENT,
  pattern SDL_KEYDOWN,
  pattern SDL_KEYUP,
  pattern SDL_TEXTEDITING,
  pattern SDL_TEXTINPUT,
  pattern SDL_KEYMAPCHANGED,
  pattern SDL_MOUSEMOTION,
  pattern SDL_MOUSEBUTTONDOWN,
  pattern SDL_MOUSEBUTTONUP,
  pattern SDL_MOUSEWHEEL,
  pattern SDL_JOYAXISMOTION,
  pattern SDL_JOYBALLMOTION,
  pattern SDL_JOYHATMOTION,
  pattern SDL_JOYBUTTONDOWN,
  pattern SDL_JOYBUTTONUP,
  pattern SDL_JOYDEVICEADDED,
  pattern SDL_JOYDEVICEREMOVED,
  pattern SDL_CONTROLLERAXISMOTION,
  pattern SDL_CONTROLLERBUTTONDOWN,
  pattern SDL_CONTROLLERBUTTONUP,
  pattern SDL_CONTROLLERDEVICEADDED,
  pattern SDL_CONTROLLERDEVICEREMOVED,
  pattern SDL_CONTROLLERDEVICEREMAPPED,
  pattern SDL_FINGERDOWN,
  pattern SDL_FINGERUP,
  pattern SDL_FINGERMOTION,
  pattern SDL_DOLLARGESTURE,
  pattern SDL_DOLLARRECORD,
  pattern SDL_MULTIGESTURE,
  pattern SDL_CLIPBOARDUPDATE,
  pattern SDL_DROPFILE,
  pattern SDL_AUDIODEVICEADDED,
  pattern SDL_AUDIODEVICEREMOVED,
  pattern SDL_RENDER_TARGETS_RESET,
  pattern SDL_RENDER_DEVICE_RESET,
  pattern SDL_USEREVENT,
  pattern SDL_LASTEVENT,

  -- ** Joystick Hat Position
  pattern SDL_HAT_CENTERED,
  pattern SDL_HAT_UP,
  pattern SDL_HAT_RIGHT,
  pattern SDL_HAT_DOWN,
  pattern SDL_HAT_LEFT,
  pattern SDL_HAT_RIGHTUP,
  pattern SDL_HAT_RIGHTDOWN,
  pattern SDL_HAT_LEFTUP,
  pattern SDL_HAT_LEFTDOWN,

  -- ** Key States
  pattern SDL_PRESSED,
  pattern SDL_RELEASED,

  -- ** Log Category
  pattern SDL_LOG_CATEGORY_APPLICATION,
  pattern SDL_LOG_CATEGORY_ERROR,
  pattern SDL_LOG_CATEGORY_ASSERT,
  pattern SDL_LOG_CATEGORY_SYSTEM,
  pattern SDL_LOG_CATEGORY_AUDIO,
  pattern SDL_LOG_CATEGORY_VIDEO,
  pattern SDL_LOG_CATEGORY_RENDER,
  pattern SDL_LOG_CATEGORY_INPUT,
  pattern SDL_LOG_CATEGORY_TEST,
  pattern SDL_LOG_CATEGORY_CUSTOM,

  -- ** Message Box Flags
  pattern SDL_MESSAGEBOX_ERROR,
  pattern SDL_MESSAGEBOX_WARNING,
  pattern SDL_MESSAGEBOX_INFORMATION,

  -- ** Message Box Button Flags
  pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT,
  pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT,

  -- ** OpenGL Profile
  pattern SDL_GL_CONTEXT_PROFILE_CORE,
  pattern SDL_GL_CONTEXT_PROFILE_COMPATIBILITY,
  pattern SDL_GL_CONTEXT_PROFILE_ES,

  -- ** OpenGL Context Flag
  pattern SDL_GL_CONTEXT_DEBUG_FLAG,
  pattern SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG,
  pattern SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG,
  pattern SDL_GL_CONTEXT_RESET_ISOLATION_FLAG,

  -- ** OpenGL Context Release Behavior Flag
  pattern SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE,
  pattern SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH,

  -- ** Pixel Formats
  pattern SDL_PIXELFORMAT_UNKNOWN,
  pattern SDL_PIXELFORMAT_INDEX1LSB,
  pattern SDL_PIXELFORMAT_INDEX1MSB,
  pattern SDL_PIXELFORMAT_INDEX4LSB,
  pattern SDL_PIXELFORMAT_INDEX4MSB,
  pattern SDL_PIXELFORMAT_INDEX8,
  pattern SDL_PIXELFORMAT_RGB332,
  pattern SDL_PIXELFORMAT_RGB444,
  pattern SDL_PIXELFORMAT_RGB555,
  pattern SDL_PIXELFORMAT_BGR555,
  pattern SDL_PIXELFORMAT_ARGB4444,
  pattern SDL_PIXELFORMAT_RGBA4444,
  pattern SDL_PIXELFORMAT_ABGR4444,
  pattern SDL_PIXELFORMAT_BGRA4444,
  pattern SDL_PIXELFORMAT_ARGB1555,
  pattern SDL_PIXELFORMAT_RGBA5551,
  pattern SDL_PIXELFORMAT_ABGR1555,
  pattern SDL_PIXELFORMAT_BGRA5551,
  pattern SDL_PIXELFORMAT_RGB565,
  pattern SDL_PIXELFORMAT_BGR565,
  pattern SDL_PIXELFORMAT_RGB24,
  pattern SDL_PIXELFORMAT_BGR24,
  pattern SDL_PIXELFORMAT_RGB888,
  pattern SDL_PIXELFORMAT_RGBX8888,
  pattern SDL_PIXELFORMAT_BGR888,
  pattern SDL_PIXELFORMAT_BGRX8888,
  pattern SDL_PIXELFORMAT_ARGB8888,
  pattern SDL_PIXELFORMAT_RGBA8888,
  pattern SDL_PIXELFORMAT_ABGR8888,
  pattern SDL_PIXELFORMAT_BGRA8888,
  pattern SDL_PIXELFORMAT_ARGB2101010,
  pattern SDL_PIXELFORMAT_YV12,
  pattern SDL_PIXELFORMAT_IYUV,
  pattern SDL_PIXELFORMAT_YUY2,
  pattern SDL_PIXELFORMAT_UYVY,
  pattern SDL_PIXELFORMAT_YVYU,

  -- ** Renderer Flags
  pattern SDL_RENDERER_SOFTWARE,
  pattern SDL_RENDERER_ACCELERATED,
  pattern SDL_RENDERER_PRESENTVSYNC,
  pattern SDL_RENDERER_TARGETTEXTURE,

  -- ** Texture Access
  pattern SDL_TEXTUREACCESS_STATIC,
  pattern SDL_TEXTUREACCESS_STREAMING,
  pattern SDL_TEXTUREACCESS_TARGET,

  -- ** Texture Modulate
  pattern SDL_TEXTUREMODULATE_NONE,
  pattern SDL_TEXTUREMODULATE_COLOR,
  pattern SDL_TEXTUREMODULATE_ALPHA,

  -- ** Touch
  pattern SDL_TOUCH_MOUSEID,

  -- ** Window Event
  pattern SDL_WINDOWEVENT_NONE,
  pattern SDL_WINDOWEVENT_SHOWN,
  pattern SDL_WINDOWEVENT_HIDDEN,
  pattern SDL_WINDOWEVENT_EXPOSED,
  pattern SDL_WINDOWEVENT_MOVED,
  pattern SDL_WINDOWEVENT_RESIZED,
  pattern SDL_WINDOWEVENT_SIZE_CHANGED,
  pattern SDL_WINDOWEVENT_MINIMIZED,
  pattern SDL_WINDOWEVENT_MAXIMIZED,
  pattern SDL_WINDOWEVENT_RESTORED,
  pattern SDL_WINDOWEVENT_ENTER,
  pattern SDL_WINDOWEVENT_LEAVE,
  pattern SDL_WINDOWEVENT_FOCUS_GAINED,
  pattern SDL_WINDOWEVENT_FOCUS_LOST,
  pattern SDL_WINDOWEVENT_CLOSE,

  -- ** Window Flags
  pattern SDL_WINDOW_FULLSCREEN,
  pattern SDL_WINDOW_OPENGL,
  pattern SDL_WINDOW_SHOWN,
  pattern SDL_WINDOW_HIDDEN,
  pattern SDL_WINDOW_BORDERLESS,
  pattern SDL_WINDOW_RESIZABLE,
  pattern SDL_WINDOW_MINIMIZED,
  pattern SDL_WINDOW_MAXIMIZED,
  pattern SDL_WINDOW_INPUT_GRABBED,
  pattern SDL_WINDOW_INPUT_FOCUS,
  pattern SDL_WINDOW_MOUSE_FOCUS,
  pattern SDL_WINDOW_FULLSCREEN_DESKTOP,
  pattern SDL_WINDOW_FOREIGN,
  pattern SDL_WINDOW_ALLOW_HIGHDPI,
  pattern SDL_WINDOW_MOUSE_CAPTURE,
  pattern SDL_WINDOW_VULKAN,

  -- ** Window Positioning
  pattern SDL_WINDOWPOS_UNDEFINED,
  pattern SDL_WINDOWPOS_CENTERED,

  -- ** Haptic Event Types
  pattern SDL_HAPTIC_CONSTANT
) where

#include "SDL.h"

import Data.Int
import Data.Word

import Foreign.C.Types

type AudioFormat = (#type SDL_AudioFormat)
type AudioStatus = (#type SDL_AudioStatus)
type BlendMode = (#type SDL_BlendMode)
type Endian = CInt
type EventAction = (#type SDL_eventaction)
type GameControllerAxis = (#type SDL_GameControllerAxis)
type GameControllerButton = (#type SDL_GameControllerButton)
type GLattr = (#type SDL_GLattr)
type HintPriority = (#type SDL_HintPriority)
type InitFlag = Word32
type JoystickPowerLevel = (#type SDL_JoystickPowerLevel)
type Keycode = (#type SDL_Keycode)
type Keymod = (#type SDL_Keymod)
type LogPriority = (#type SDL_LogPriority)
type PowerState = (#type SDL_PowerState)
type RendererFlip = (#type SDL_RendererFlip)
type Scancode = (#type SDL_Scancode)
type SystemCursor = (#type SDL_SystemCursor)
type ThreadPriority = (#type SDL_ThreadPriority)

pattern SDL_AUDIO_S8 = (#const AUDIO_S8) :: AudioFormat
pattern SDL_AUDIO_U8 = (#const AUDIO_U8) :: AudioFormat
pattern SDL_AUDIO_S16LSB = (#const AUDIO_S16LSB) :: AudioFormat
pattern SDL_AUDIO_S16MSB = (#const AUDIO_S16MSB) :: AudioFormat
pattern SDL_AUDIO_S16SYS = (#const AUDIO_S16SYS) :: AudioFormat
pattern SDL_AUDIO_U16LSB = (#const AUDIO_U16LSB) :: AudioFormat
pattern SDL_AUDIO_U16MSB = (#const AUDIO_U16MSB) :: AudioFormat
pattern SDL_AUDIO_U16SYS = (#const AUDIO_U16SYS) :: AudioFormat
pattern SDL_AUDIO_S32LSB = (#const AUDIO_S32LSB) :: AudioFormat
pattern SDL_AUDIO_S32MSB = (#const AUDIO_S32MSB) :: AudioFormat
pattern SDL_AUDIO_S32SYS = (#const AUDIO_S32SYS) :: AudioFormat
pattern SDL_AUDIO_F32LSB = (#const AUDIO_F32LSB) :: AudioFormat
pattern SDL_AUDIO_F32MSB = (#const AUDIO_F32MSB) :: AudioFormat
pattern SDL_AUDIO_F32SYS = (#const AUDIO_F32SYS) :: AudioFormat

pattern SDL_AUDIO_STOPPED = (#const SDL_AUDIO_STOPPED) :: AudioStatus
pattern SDL_AUDIO_PLAYING = (#const SDL_AUDIO_PLAYING) :: AudioStatus
pattern SDL_AUDIO_PAUSED = (#const SDL_AUDIO_PAUSED) :: AudioStatus

pattern SDL_BLENDMODE_NONE = (#const SDL_BLENDMODE_NONE) :: BlendMode
pattern SDL_BLENDMODE_BLEND = (#const SDL_BLENDMODE_BLEND) :: BlendMode
pattern SDL_BLENDMODE_ADD = (#const SDL_BLENDMODE_ADD) :: BlendMode
pattern SDL_BLENDMODE_MOD = (#const SDL_BLENDMODE_MOD) :: BlendMode

pattern SDL_BYTEORDER = (#const SDL_BYTEORDER) :: Endian
pattern SDL_LIL_ENDIAN = (#const SDL_LIL_ENDIAN) :: Endian
pattern SDL_BIG_ENDIAN = (#const SDL_BIG_ENDIAN) :: Endian

pattern SDL_ADDEVENT = (#const SDL_ADDEVENT) :: EventAction
pattern SDL_PEEKEVENT = (#const SDL_PEEKEVENT) :: EventAction
pattern SDL_GETEVENT = (#const SDL_GETEVENT) :: EventAction

pattern SDL_CONTROLLER_AXIS_INVALID = (#const SDL_CONTROLLER_AXIS_INVALID) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_LEFTX = (#const SDL_CONTROLLER_AXIS_LEFTX) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_LEFTY = (#const SDL_CONTROLLER_AXIS_LEFTY) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_RIGHTX = (#const SDL_CONTROLLER_AXIS_RIGHTX) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_RIGHTY = (#const SDL_CONTROLLER_AXIS_RIGHTY) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_TRIGGERLEFT = (#const SDL_CONTROLLER_AXIS_TRIGGERLEFT) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_TRIGGERRIGHT = (#const SDL_CONTROLLER_AXIS_TRIGGERRIGHT) :: GameControllerAxis
pattern SDL_CONTROLLER_AXIS_MAX = (#const SDL_CONTROLLER_AXIS_MAX) :: GameControllerAxis

pattern SDL_CONTROLLER_BUTTON_INVALID = (#const SDL_CONTROLLER_BUTTON_INVALID) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_A = (#const SDL_CONTROLLER_BUTTON_A) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_B = (#const SDL_CONTROLLER_BUTTON_B) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_X = (#const SDL_CONTROLLER_BUTTON_X) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_Y = (#const SDL_CONTROLLER_BUTTON_Y) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_BACK = (#const SDL_CONTROLLER_BUTTON_BACK) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_GUIDE = (#const SDL_CONTROLLER_BUTTON_GUIDE) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_START = (#const SDL_CONTROLLER_BUTTON_START) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_LEFTSTICK = (#const SDL_CONTROLLER_BUTTON_LEFTSTICK) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_RIGHTSTICK = (#const SDL_CONTROLLER_BUTTON_RIGHTSTICK) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_LEFTSHOULDER = (#const SDL_CONTROLLER_BUTTON_LEFTSHOULDER) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_RIGHTSHOULDER = (#const SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_DPAD_UP = (#const SDL_CONTROLLER_BUTTON_DPAD_UP) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_DPAD_DOWN = (#const SDL_CONTROLLER_BUTTON_DPAD_DOWN) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_DPAD_LEFT = (#const SDL_CONTROLLER_BUTTON_DPAD_LEFT) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_DPAD_RIGHT = (#const SDL_CONTROLLER_BUTTON_DPAD_RIGHT) :: GameControllerButton
pattern SDL_CONTROLLER_BUTTON_MAX = (#const SDL_CONTROLLER_BUTTON_MAX) :: GameControllerButton

pattern SDL_GL_RED_SIZE = (#const SDL_GL_RED_SIZE) :: GLattr
pattern SDL_GL_GREEN_SIZE = (#const SDL_GL_GREEN_SIZE) :: GLattr
pattern SDL_GL_BLUE_SIZE = (#const SDL_GL_BLUE_SIZE) :: GLattr
pattern SDL_GL_ALPHA_SIZE = (#const SDL_GL_ALPHA_SIZE) :: GLattr
pattern SDL_GL_BUFFER_SIZE = (#const SDL_GL_BUFFER_SIZE) :: GLattr
pattern SDL_GL_DOUBLEBUFFER = (#const SDL_GL_DOUBLEBUFFER) :: GLattr
pattern SDL_GL_DEPTH_SIZE = (#const SDL_GL_DEPTH_SIZE) :: GLattr
pattern SDL_GL_STENCIL_SIZE = (#const SDL_GL_STENCIL_SIZE) :: GLattr
pattern SDL_GL_ACCUM_RED_SIZE = (#const SDL_GL_ACCUM_RED_SIZE) :: GLattr
pattern SDL_GL_ACCUM_GREEN_SIZE = (#const SDL_GL_ACCUM_GREEN_SIZE) :: GLattr
pattern SDL_GL_ACCUM_BLUE_SIZE = (#const SDL_GL_ACCUM_BLUE_SIZE) :: GLattr
pattern SDL_GL_ACCUM_ALPHA_SIZE = (#const SDL_GL_ACCUM_ALPHA_SIZE) :: GLattr
pattern SDL_GL_STEREO = (#const SDL_GL_STEREO) :: GLattr
pattern SDL_GL_MULTISAMPLEBUFFERS = (#const SDL_GL_MULTISAMPLEBUFFERS) :: GLattr
pattern SDL_GL_MULTISAMPLESAMPLES = (#const SDL_GL_MULTISAMPLESAMPLES) :: GLattr
pattern SDL_GL_ACCELERATED_VISUAL = (#const SDL_GL_ACCELERATED_VISUAL) :: GLattr
pattern SDL_GL_RETAINED_BACKING = (#const SDL_GL_RETAINED_BACKING) :: GLattr
pattern SDL_GL_CONTEXT_MAJOR_VERSION = (#const SDL_GL_CONTEXT_MAJOR_VERSION) :: GLattr
pattern SDL_GL_CONTEXT_MINOR_VERSION = (#const SDL_GL_CONTEXT_MINOR_VERSION) :: GLattr
pattern SDL_GL_CONTEXT_EGL = (#const SDL_GL_CONTEXT_EGL) :: GLattr
pattern SDL_GL_CONTEXT_FLAGS = (#const SDL_GL_CONTEXT_FLAGS) :: GLattr
pattern SDL_GL_CONTEXT_PROFILE_MASK = (#const SDL_GL_CONTEXT_PROFILE_MASK) :: GLattr
pattern SDL_GL_SHARE_WITH_CURRENT_CONTEXT = (#const SDL_GL_SHARE_WITH_CURRENT_CONTEXT) :: GLattr
pattern SDL_GL_FRAMEBUFFER_SRGB_CAPABLE = (#const SDL_GL_FRAMEBUFFER_SRGB_CAPABLE) :: GLattr
pattern SDL_GL_CONTEXT_RELEASE_BEHAVIOR = (#const SDL_GL_CONTEXT_RELEASE_BEHAVIOR) :: GLattr

pattern SDL_HINT_DEFAULT = (#const SDL_HINT_DEFAULT) :: HintPriority
pattern SDL_HINT_NORMAL = (#const SDL_HINT_NORMAL) :: HintPriority
pattern SDL_HINT_OVERRIDE = (#const SDL_HINT_OVERRIDE) :: HintPriority

pattern SDL_INIT_TIMER = (#const SDL_INIT_TIMER) :: InitFlag
pattern SDL_INIT_AUDIO = (#const SDL_INIT_AUDIO) :: InitFlag
pattern SDL_INIT_VIDEO = (#const SDL_INIT_VIDEO) :: InitFlag
pattern SDL_INIT_JOYSTICK = (#const SDL_INIT_JOYSTICK) :: InitFlag
pattern SDL_INIT_HAPTIC = (#const SDL_INIT_HAPTIC) :: InitFlag
pattern SDL_INIT_GAMECONTROLLER = (#const SDL_INIT_GAMECONTROLLER) :: InitFlag
pattern SDL_INIT_EVENTS = (#const SDL_INIT_EVENTS) :: InitFlag
pattern SDL_INIT_NOPARACHUTE = (#const SDL_INIT_NOPARACHUTE) :: InitFlag
pattern SDL_INIT_EVERYTHING = (#const SDL_INIT_EVERYTHING) :: InitFlag

pattern SDL_JOYSTICK_POWER_UNKNOWN = (#const SDL_JOYSTICK_POWER_UNKNOWN) :: JoystickPowerLevel
pattern SDL_JOYSTICK_POWER_EMPTY = (#const SDL_JOYSTICK_POWER_EMPTY) :: JoystickPowerLevel
pattern SDL_JOYSTICK_POWER_LOW = (#const SDL_JOYSTICK_POWER_LOW) :: JoystickPowerLevel
pattern SDL_JOYSTICK_POWER_MEDIUM = (#const SDL_JOYSTICK_POWER_MEDIUM) :: JoystickPowerLevel
pattern SDL_JOYSTICK_POWER_FULL = (#const SDL_JOYSTICK_POWER_FULL) :: JoystickPowerLevel
pattern SDL_JOYSTICK_POWER_WIRED = (#const SDL_JOYSTICK_POWER_WIRED) :: JoystickPowerLevel
pattern SDL_JOYSTICK_POWER_MAX = (#const SDL_JOYSTICK_POWER_MAX) :: JoystickPowerLevel

pattern SDLK_UNKNOWN = (#const SDLK_UNKNOWN) :: Keycode
pattern SDLK_RETURN = (#const SDLK_RETURN) :: Keycode
pattern SDLK_ESCAPE = (#const SDLK_ESCAPE) :: Keycode
pattern SDLK_BACKSPACE = (#const SDLK_BACKSPACE) :: Keycode
pattern SDLK_TAB = (#const SDLK_TAB) :: Keycode
pattern SDLK_SPACE = (#const SDLK_SPACE) :: Keycode
pattern SDLK_EXCLAIM = (#const SDLK_EXCLAIM) :: Keycode
pattern SDLK_QUOTEDBL = (#const SDLK_QUOTEDBL) :: Keycode
pattern SDLK_HASH = (#const SDLK_HASH) :: Keycode
pattern SDLK_PERCENT = (#const SDLK_PERCENT) :: Keycode
pattern SDLK_DOLLAR = (#const SDLK_DOLLAR) :: Keycode
pattern SDLK_AMPERSAND = (#const SDLK_AMPERSAND) :: Keycode
pattern SDLK_QUOTE = (#const SDLK_QUOTE) :: Keycode
pattern SDLK_LEFTPAREN = (#const SDLK_LEFTPAREN) :: Keycode
pattern SDLK_RIGHTPAREN = (#const SDLK_RIGHTPAREN) :: Keycode
pattern SDLK_ASTERISK = (#const SDLK_ASTERISK) :: Keycode
pattern SDLK_PLUS = (#const SDLK_PLUS) :: Keycode
pattern SDLK_COMMA = (#const SDLK_COMMA) :: Keycode
pattern SDLK_MINUS = (#const SDLK_MINUS) :: Keycode
pattern SDLK_PERIOD = (#const SDLK_PERIOD) :: Keycode
pattern SDLK_SLASH = (#const SDLK_SLASH) :: Keycode
pattern SDLK_0 = (#const SDLK_0) :: Keycode
pattern SDLK_1 = (#const SDLK_1) :: Keycode
pattern SDLK_2 = (#const SDLK_2) :: Keycode
pattern SDLK_3 = (#const SDLK_3) :: Keycode
pattern SDLK_4 = (#const SDLK_4) :: Keycode
pattern SDLK_5 = (#const SDLK_5) :: Keycode
pattern SDLK_6 = (#const SDLK_6) :: Keycode
pattern SDLK_7 = (#const SDLK_7) :: Keycode
pattern SDLK_8 = (#const SDLK_8) :: Keycode
pattern SDLK_9 = (#const SDLK_9) :: Keycode
pattern SDLK_COLON = (#const SDLK_COLON) :: Keycode
pattern SDLK_SEMICOLON = (#const SDLK_SEMICOLON) :: Keycode
pattern SDLK_LESS = (#const SDLK_LESS) :: Keycode
pattern SDLK_EQUALS = (#const SDLK_EQUALS) :: Keycode
pattern SDLK_GREATER = (#const SDLK_GREATER) :: Keycode
pattern SDLK_QUESTION = (#const SDLK_QUESTION) :: Keycode
pattern SDLK_AT = (#const SDLK_AT) :: Keycode
pattern SDLK_LEFTBRACKET = (#const SDLK_LEFTBRACKET) :: Keycode
pattern SDLK_BACKSLASH = (#const SDLK_BACKSLASH) :: Keycode
pattern SDLK_RIGHTBRACKET = (#const SDLK_RIGHTBRACKET) :: Keycode
pattern SDLK_CARET = (#const SDLK_CARET) :: Keycode
pattern SDLK_UNDERSCORE = (#const SDLK_UNDERSCORE) :: Keycode
pattern SDLK_BACKQUOTE = (#const SDLK_BACKQUOTE) :: Keycode
pattern SDLK_a = (#const SDLK_a) :: Keycode
pattern SDLK_b = (#const SDLK_b) :: Keycode
pattern SDLK_c = (#const SDLK_c) :: Keycode
pattern SDLK_d = (#const SDLK_d) :: Keycode
pattern SDLK_e = (#const SDLK_e) :: Keycode
pattern SDLK_f = (#const SDLK_f) :: Keycode
pattern SDLK_g = (#const SDLK_g) :: Keycode
pattern SDLK_h = (#const SDLK_h) :: Keycode
pattern SDLK_i = (#const SDLK_i) :: Keycode
pattern SDLK_j = (#const SDLK_j) :: Keycode
pattern SDLK_k = (#const SDLK_k) :: Keycode
pattern SDLK_l = (#const SDLK_l) :: Keycode
pattern SDLK_m = (#const SDLK_m) :: Keycode
pattern SDLK_n = (#const SDLK_n) :: Keycode
pattern SDLK_o = (#const SDLK_o) :: Keycode
pattern SDLK_p = (#const SDLK_p) :: Keycode
pattern SDLK_q = (#const SDLK_q) :: Keycode
pattern SDLK_r = (#const SDLK_r) :: Keycode
pattern SDLK_s = (#const SDLK_s) :: Keycode
pattern SDLK_t = (#const SDLK_t) :: Keycode
pattern SDLK_u = (#const SDLK_u) :: Keycode
pattern SDLK_v = (#const SDLK_v) :: Keycode
pattern SDLK_w = (#const SDLK_w) :: Keycode
pattern SDLK_x = (#const SDLK_x) :: Keycode
pattern SDLK_y = (#const SDLK_y) :: Keycode
pattern SDLK_z = (#const SDLK_z) :: Keycode
pattern SDLK_CAPSLOCK = (#const SDLK_CAPSLOCK) :: Keycode
pattern SDLK_F1 = (#const SDLK_F1) :: Keycode
pattern SDLK_F2 = (#const SDLK_F2) :: Keycode
pattern SDLK_F3 = (#const SDLK_F3) :: Keycode
pattern SDLK_F4 = (#const SDLK_F4) :: Keycode
pattern SDLK_F5 = (#const SDLK_F5) :: Keycode
pattern SDLK_F6 = (#const SDLK_F6) :: Keycode
pattern SDLK_F7 = (#const SDLK_F7) :: Keycode
pattern SDLK_F8 = (#const SDLK_F8) :: Keycode
pattern SDLK_F9 = (#const SDLK_F9) :: Keycode
pattern SDLK_F10 = (#const SDLK_F10) :: Keycode
pattern SDLK_F11 = (#const SDLK_F11) :: Keycode
pattern SDLK_F12 = (#const SDLK_F12) :: Keycode
pattern SDLK_PRINTSCREEN = (#const SDLK_PRINTSCREEN) :: Keycode
pattern SDLK_SCROLLLOCK = (#const SDLK_SCROLLLOCK) :: Keycode
pattern SDLK_PAUSE = (#const SDLK_PAUSE) :: Keycode
pattern SDLK_INSERT = (#const SDLK_INSERT) :: Keycode
pattern SDLK_HOME = (#const SDLK_HOME) :: Keycode
pattern SDLK_PAGEUP = (#const SDLK_PAGEUP) :: Keycode
pattern SDLK_DELETE = (#const SDLK_DELETE) :: Keycode
pattern SDLK_END = (#const SDLK_END) :: Keycode
pattern SDLK_PAGEDOWN = (#const SDLK_PAGEDOWN) :: Keycode
pattern SDLK_RIGHT = (#const SDLK_RIGHT) :: Keycode
pattern SDLK_LEFT = (#const SDLK_LEFT) :: Keycode
pattern SDLK_DOWN = (#const SDLK_DOWN) :: Keycode
pattern SDLK_UP = (#const SDLK_UP) :: Keycode
pattern SDLK_NUMLOCKCLEAR = (#const SDLK_NUMLOCKCLEAR) :: Keycode
pattern SDLK_KP_DIVIDE = (#const SDLK_KP_DIVIDE) :: Keycode
pattern SDLK_KP_MULTIPLY = (#const SDLK_KP_MULTIPLY) :: Keycode
pattern SDLK_KP_MINUS = (#const SDLK_KP_MINUS) :: Keycode
pattern SDLK_KP_PLUS = (#const SDLK_KP_PLUS) :: Keycode
pattern SDLK_KP_ENTER = (#const SDLK_KP_ENTER) :: Keycode
pattern SDLK_KP_1 = (#const SDLK_KP_1) :: Keycode
pattern SDLK_KP_2 = (#const SDLK_KP_2) :: Keycode
pattern SDLK_KP_3 = (#const SDLK_KP_3) :: Keycode
pattern SDLK_KP_4 = (#const SDLK_KP_4) :: Keycode
pattern SDLK_KP_5 = (#const SDLK_KP_5) :: Keycode
pattern SDLK_KP_6 = (#const SDLK_KP_6) :: Keycode
pattern SDLK_KP_7 = (#const SDLK_KP_7) :: Keycode
pattern SDLK_KP_8 = (#const SDLK_KP_8) :: Keycode
pattern SDLK_KP_9 = (#const SDLK_KP_9) :: Keycode
pattern SDLK_KP_0 = (#const SDLK_KP_0) :: Keycode
pattern SDLK_KP_PERIOD = (#const SDLK_KP_PERIOD) :: Keycode
pattern SDLK_APPLICATION = (#const SDLK_APPLICATION) :: Keycode
pattern SDLK_POWER = (#const SDLK_POWER) :: Keycode
pattern SDLK_KP_EQUALS = (#const SDLK_KP_EQUALS) :: Keycode
pattern SDLK_F13 = (#const SDLK_F13) :: Keycode
pattern SDLK_F14 = (#const SDLK_F14) :: Keycode
pattern SDLK_F15 = (#const SDLK_F15) :: Keycode
pattern SDLK_F16 = (#const SDLK_F16) :: Keycode
pattern SDLK_F17 = (#const SDLK_F17) :: Keycode
pattern SDLK_F18 = (#const SDLK_F18) :: Keycode
pattern SDLK_F19 = (#const SDLK_F19) :: Keycode
pattern SDLK_F20 = (#const SDLK_F20) :: Keycode
pattern SDLK_F21 = (#const SDLK_F21) :: Keycode
pattern SDLK_F22 = (#const SDLK_F22) :: Keycode
pattern SDLK_F23 = (#const SDLK_F23) :: Keycode
pattern SDLK_F24 = (#const SDLK_F24) :: Keycode
pattern SDLK_EXECUTE = (#const SDLK_EXECUTE) :: Keycode
pattern SDLK_HELP = (#const SDLK_HELP) :: Keycode
pattern SDLK_MENU = (#const SDLK_MENU) :: Keycode
pattern SDLK_SELECT = (#const SDLK_SELECT) :: Keycode
pattern SDLK_STOP = (#const SDLK_STOP) :: Keycode
pattern SDLK_AGAIN = (#const SDLK_AGAIN) :: Keycode
pattern SDLK_UNDO = (#const SDLK_UNDO) :: Keycode
pattern SDLK_CUT = (#const SDLK_CUT) :: Keycode
pattern SDLK_COPY = (#const SDLK_COPY) :: Keycode
pattern SDLK_PASTE = (#const SDLK_PASTE) :: Keycode
pattern SDLK_FIND = (#const SDLK_FIND) :: Keycode
pattern SDLK_MUTE = (#const SDLK_MUTE) :: Keycode
pattern SDLK_VOLUMEUP = (#const SDLK_VOLUMEUP) :: Keycode
pattern SDLK_VOLUMEDOWN = (#const SDLK_VOLUMEDOWN) :: Keycode
pattern SDLK_KP_COMMA = (#const SDLK_KP_COMMA) :: Keycode
pattern SDLK_KP_EQUALSAS400 = (#const SDLK_KP_EQUALSAS400) :: Keycode
pattern SDLK_ALTERASE = (#const SDLK_ALTERASE) :: Keycode
pattern SDLK_SYSREQ = (#const SDLK_SYSREQ) :: Keycode
pattern SDLK_CANCEL = (#const SDLK_CANCEL) :: Keycode
pattern SDLK_CLEAR = (#const SDLK_CLEAR) :: Keycode
pattern SDLK_PRIOR = (#const SDLK_PRIOR) :: Keycode
pattern SDLK_RETURN2 = (#const SDLK_RETURN2) :: Keycode
pattern SDLK_SEPARATOR = (#const SDLK_SEPARATOR) :: Keycode
pattern SDLK_OUT = (#const SDLK_OUT) :: Keycode
pattern SDLK_OPER = (#const SDLK_OPER) :: Keycode
pattern SDLK_CLEARAGAIN = (#const SDLK_CLEARAGAIN) :: Keycode
pattern SDLK_CRSEL = (#const SDLK_CRSEL) :: Keycode
pattern SDLK_EXSEL = (#const SDLK_EXSEL) :: Keycode
pattern SDLK_KP_00 = (#const SDLK_KP_00) :: Keycode
pattern SDLK_KP_000 = (#const SDLK_KP_000) :: Keycode
pattern SDLK_THOUSANDSSEPARATOR = (#const SDLK_THOUSANDSSEPARATOR) :: Keycode
pattern SDLK_DECIMALSEPARATOR = (#const SDLK_DECIMALSEPARATOR) :: Keycode
pattern SDLK_CURRENCYUNIT = (#const SDLK_CURRENCYUNIT) :: Keycode
pattern SDLK_CURRENCYSUBUNIT = (#const SDLK_CURRENCYSUBUNIT) :: Keycode
pattern SDLK_KP_LEFTPAREN = (#const SDLK_KP_LEFTPAREN) :: Keycode
pattern SDLK_KP_RIGHTPAREN = (#const SDLK_KP_RIGHTPAREN) :: Keycode
pattern SDLK_KP_LEFTBRACE = (#const SDLK_KP_LEFTBRACE) :: Keycode
pattern SDLK_KP_RIGHTBRACE = (#const SDLK_KP_RIGHTBRACE) :: Keycode
pattern SDLK_KP_TAB = (#const SDLK_KP_TAB) :: Keycode
pattern SDLK_KP_BACKSPACE = (#const SDLK_KP_BACKSPACE) :: Keycode
pattern SDLK_KP_A = (#const SDLK_KP_A) :: Keycode
pattern SDLK_KP_B = (#const SDLK_KP_B) :: Keycode
pattern SDLK_KP_C = (#const SDLK_KP_C) :: Keycode
pattern SDLK_KP_D = (#const SDLK_KP_D) :: Keycode
pattern SDLK_KP_E = (#const SDLK_KP_E) :: Keycode
pattern SDLK_KP_F = (#const SDLK_KP_F) :: Keycode
pattern SDLK_KP_XOR = (#const SDLK_KP_XOR) :: Keycode
pattern SDLK_KP_POWER = (#const SDLK_KP_POWER) :: Keycode
pattern SDLK_KP_PERCENT = (#const SDLK_KP_PERCENT) :: Keycode
pattern SDLK_KP_LESS = (#const SDLK_KP_LESS) :: Keycode
pattern SDLK_KP_GREATER = (#const SDLK_KP_GREATER) :: Keycode
pattern SDLK_KP_AMPERSAND = (#const SDLK_KP_AMPERSAND) :: Keycode
pattern SDLK_KP_DBLAMPERSAND = (#const SDLK_KP_DBLAMPERSAND) :: Keycode
pattern SDLK_KP_VERTICALBAR = (#const SDLK_KP_VERTICALBAR) :: Keycode
pattern SDLK_KP_DBLVERTICALBAR = (#const SDLK_KP_DBLVERTICALBAR) :: Keycode
pattern SDLK_KP_COLON = (#const SDLK_KP_COLON) :: Keycode
pattern SDLK_KP_HASH = (#const SDLK_KP_HASH) :: Keycode
pattern SDLK_KP_SPACE = (#const SDLK_KP_SPACE) :: Keycode
pattern SDLK_KP_AT = (#const SDLK_KP_AT) :: Keycode
pattern SDLK_KP_EXCLAM = (#const SDLK_KP_EXCLAM) :: Keycode
pattern SDLK_KP_MEMSTORE = (#const SDLK_KP_MEMSTORE) :: Keycode
pattern SDLK_KP_MEMRECALL = (#const SDLK_KP_MEMRECALL) :: Keycode
pattern SDLK_KP_MEMCLEAR = (#const SDLK_KP_MEMCLEAR) :: Keycode
pattern SDLK_KP_MEMADD = (#const SDLK_KP_MEMADD) :: Keycode
pattern SDLK_KP_MEMSUBTRACT = (#const SDLK_KP_MEMSUBTRACT) :: Keycode
pattern SDLK_KP_MEMMULTIPLY = (#const SDLK_KP_MEMMULTIPLY) :: Keycode
pattern SDLK_KP_MEMDIVIDE = (#const SDLK_KP_MEMDIVIDE) :: Keycode
pattern SDLK_KP_PLUSMINUS = (#const SDLK_KP_PLUSMINUS) :: Keycode
pattern SDLK_KP_CLEAR = (#const SDLK_KP_CLEAR) :: Keycode
pattern SDLK_KP_CLEARENTRY = (#const SDLK_KP_CLEARENTRY) :: Keycode
pattern SDLK_KP_BINARY = (#const SDLK_KP_BINARY) :: Keycode
pattern SDLK_KP_OCTAL = (#const SDLK_KP_OCTAL) :: Keycode
pattern SDLK_KP_DECIMAL = (#const SDLK_KP_DECIMAL) :: Keycode
pattern SDLK_KP_HEXADECIMAL = (#const SDLK_KP_HEXADECIMAL) :: Keycode
pattern SDLK_LCTRL = (#const SDLK_LCTRL) :: Keycode
pattern SDLK_LSHIFT = (#const SDLK_LSHIFT) :: Keycode
pattern SDLK_LALT = (#const SDLK_LALT) :: Keycode
pattern SDLK_LGUI = (#const SDLK_LGUI) :: Keycode
pattern SDLK_RCTRL = (#const SDLK_RCTRL) :: Keycode
pattern SDLK_RSHIFT = (#const SDLK_RSHIFT) :: Keycode
pattern SDLK_RALT = (#const SDLK_RALT) :: Keycode
pattern SDLK_RGUI = (#const SDLK_RGUI) :: Keycode
pattern SDLK_MODE = (#const SDLK_MODE) :: Keycode
pattern SDLK_AUDIONEXT = (#const SDLK_AUDIONEXT) :: Keycode
pattern SDLK_AUDIOPREV = (#const SDLK_AUDIOPREV) :: Keycode
pattern SDLK_AUDIOSTOP = (#const SDLK_AUDIOSTOP) :: Keycode
pattern SDLK_AUDIOPLAY = (#const SDLK_AUDIOPLAY) :: Keycode
pattern SDLK_AUDIOMUTE = (#const SDLK_AUDIOMUTE) :: Keycode
pattern SDLK_MEDIASELECT = (#const SDLK_MEDIASELECT) :: Keycode
pattern SDLK_WWW = (#const SDLK_WWW) :: Keycode
pattern SDLK_MAIL = (#const SDLK_MAIL) :: Keycode
pattern SDLK_CALCULATOR = (#const SDLK_CALCULATOR) :: Keycode
pattern SDLK_COMPUTER = (#const SDLK_COMPUTER) :: Keycode
pattern SDLK_AC_SEARCH = (#const SDLK_AC_SEARCH) :: Keycode
pattern SDLK_AC_HOME = (#const SDLK_AC_HOME) :: Keycode
pattern SDLK_AC_BACK = (#const SDLK_AC_BACK) :: Keycode
pattern SDLK_AC_FORWARD = (#const SDLK_AC_FORWARD) :: Keycode
pattern SDLK_AC_STOP = (#const SDLK_AC_STOP) :: Keycode
pattern SDLK_AC_REFRESH = (#const SDLK_AC_REFRESH) :: Keycode
pattern SDLK_AC_BOOKMARKS = (#const SDLK_AC_BOOKMARKS) :: Keycode
pattern SDLK_BRIGHTNESSDOWN = (#const SDLK_BRIGHTNESSDOWN) :: Keycode
pattern SDLK_BRIGHTNESSUP = (#const SDLK_BRIGHTNESSUP) :: Keycode
pattern SDLK_DISPLAYSWITCH = (#const SDLK_DISPLAYSWITCH) :: Keycode
pattern SDLK_KBDILLUMTOGGLE = (#const SDLK_KBDILLUMTOGGLE) :: Keycode
pattern SDLK_KBDILLUMDOWN = (#const SDLK_KBDILLUMDOWN) :: Keycode
pattern SDLK_KBDILLUMUP = (#const SDLK_KBDILLUMUP) :: Keycode
pattern SDLK_EJECT = (#const SDLK_EJECT) :: Keycode
pattern SDLK_SLEEP = (#const SDLK_SLEEP) :: Keycode

pattern KMOD_NONE = (#const KMOD_NONE)
pattern KMOD_LSHIFT = (#const KMOD_LSHIFT)
pattern KMOD_RSHIFT = (#const KMOD_RSHIFT)
pattern KMOD_SHIFT = (#const KMOD_SHIFT)
pattern KMOD_LCTRL = (#const KMOD_LCTRL)
pattern KMOD_RCTRL = (#const KMOD_RCTRL)
pattern KMOD_CTRL = (#const KMOD_CTRL)
pattern KMOD_LALT = (#const KMOD_LALT)
pattern KMOD_RALT = (#const KMOD_RALT)
pattern KMOD_ALT = (#const KMOD_ALT)
pattern KMOD_LGUI = (#const KMOD_LGUI)
pattern KMOD_RGUI = (#const KMOD_RGUI)
pattern KMOD_GUI = (#const KMOD_GUI)
pattern KMOD_NUM = (#const KMOD_NUM)
pattern KMOD_CAPS = (#const KMOD_CAPS)
pattern KMOD_MODE = (#const KMOD_MODE)
pattern KMOD_RESERVED = (#const KMOD_RESERVED)

pattern SDL_LOG_PRIORITY_VERBOSE = (#const SDL_LOG_PRIORITY_VERBOSE) :: LogPriority
pattern SDL_LOG_PRIORITY_DEBUG = (#const SDL_LOG_PRIORITY_DEBUG) :: LogPriority
pattern SDL_LOG_PRIORITY_INFO = (#const SDL_LOG_PRIORITY_INFO) :: LogPriority
pattern SDL_LOG_PRIORITY_WARN = (#const SDL_LOG_PRIORITY_WARN) :: LogPriority
pattern SDL_LOG_PRIORITY_ERROR = (#const SDL_LOG_PRIORITY_ERROR) :: LogPriority
pattern SDL_LOG_PRIORITY_CRITICAL = (#const SDL_LOG_PRIORITY_CRITICAL) :: LogPriority
pattern SDL_NUM_LOG_PRIORITIES = (#const SDL_NUM_LOG_PRIORITIES) :: LogPriority

pattern SDL_POWERSTATE_UNKNOWN = (#const SDL_POWERSTATE_UNKNOWN) :: PowerState
pattern SDL_POWERSTATE_ON_BATTERY = (#const SDL_POWERSTATE_ON_BATTERY) :: PowerState
pattern SDL_POWERSTATE_NO_BATTERY = (#const SDL_POWERSTATE_NO_BATTERY) :: PowerState
pattern SDL_POWERSTATE_CHARGING = (#const SDL_POWERSTATE_CHARGING) :: PowerState
pattern SDL_POWERSTATE_CHARGED = (#const SDL_POWERSTATE_CHARGED) :: PowerState

pattern SDL_FLIP_NONE = (#const SDL_FLIP_NONE) :: RendererFlip
pattern SDL_FLIP_HORIZONTAL = (#const SDL_FLIP_HORIZONTAL) :: RendererFlip
pattern SDL_FLIP_VERTICAL = (#const SDL_FLIP_VERTICAL) :: RendererFlip

pattern SDL_SCANCODE_UNKNOWN = (#const SDL_SCANCODE_UNKNOWN) :: Scancode
pattern SDL_SCANCODE_A = (#const SDL_SCANCODE_A) :: Scancode
pattern SDL_SCANCODE_B = (#const SDL_SCANCODE_B) :: Scancode
pattern SDL_SCANCODE_C = (#const SDL_SCANCODE_C) :: Scancode
pattern SDL_SCANCODE_D = (#const SDL_SCANCODE_D) :: Scancode
pattern SDL_SCANCODE_E = (#const SDL_SCANCODE_E) :: Scancode
pattern SDL_SCANCODE_F = (#const SDL_SCANCODE_F) :: Scancode
pattern SDL_SCANCODE_G = (#const SDL_SCANCODE_G) :: Scancode
pattern SDL_SCANCODE_H = (#const SDL_SCANCODE_H) :: Scancode
pattern SDL_SCANCODE_I = (#const SDL_SCANCODE_I) :: Scancode
pattern SDL_SCANCODE_J = (#const SDL_SCANCODE_J) :: Scancode
pattern SDL_SCANCODE_K = (#const SDL_SCANCODE_K) :: Scancode
pattern SDL_SCANCODE_L = (#const SDL_SCANCODE_L) :: Scancode
pattern SDL_SCANCODE_M = (#const SDL_SCANCODE_M) :: Scancode
pattern SDL_SCANCODE_N = (#const SDL_SCANCODE_N) :: Scancode
pattern SDL_SCANCODE_O = (#const SDL_SCANCODE_O) :: Scancode
pattern SDL_SCANCODE_P = (#const SDL_SCANCODE_P) :: Scancode
pattern SDL_SCANCODE_Q = (#const SDL_SCANCODE_Q) :: Scancode
pattern SDL_SCANCODE_R = (#const SDL_SCANCODE_R) :: Scancode
pattern SDL_SCANCODE_S = (#const SDL_SCANCODE_S) :: Scancode
pattern SDL_SCANCODE_T = (#const SDL_SCANCODE_T) :: Scancode
pattern SDL_SCANCODE_U = (#const SDL_SCANCODE_U) :: Scancode
pattern SDL_SCANCODE_V = (#const SDL_SCANCODE_V) :: Scancode
pattern SDL_SCANCODE_W = (#const SDL_SCANCODE_W) :: Scancode
pattern SDL_SCANCODE_X = (#const SDL_SCANCODE_X) :: Scancode
pattern SDL_SCANCODE_Y = (#const SDL_SCANCODE_Y) :: Scancode
pattern SDL_SCANCODE_Z = (#const SDL_SCANCODE_Z) :: Scancode
pattern SDL_SCANCODE_1 = (#const SDL_SCANCODE_1) :: Scancode
pattern SDL_SCANCODE_2 = (#const SDL_SCANCODE_2) :: Scancode
pattern SDL_SCANCODE_3 = (#const SDL_SCANCODE_3) :: Scancode
pattern SDL_SCANCODE_4 = (#const SDL_SCANCODE_4) :: Scancode
pattern SDL_SCANCODE_5 = (#const SDL_SCANCODE_5) :: Scancode
pattern SDL_SCANCODE_6 = (#const SDL_SCANCODE_6) :: Scancode
pattern SDL_SCANCODE_7 = (#const SDL_SCANCODE_7) :: Scancode
pattern SDL_SCANCODE_8 = (#const SDL_SCANCODE_8) :: Scancode
pattern SDL_SCANCODE_9 = (#const SDL_SCANCODE_9) :: Scancode
pattern SDL_SCANCODE_0 = (#const SDL_SCANCODE_0) :: Scancode
pattern SDL_SCANCODE_RETURN = (#const SDL_SCANCODE_RETURN) :: Scancode
pattern SDL_SCANCODE_ESCAPE = (#const SDL_SCANCODE_ESCAPE) :: Scancode
pattern SDL_SCANCODE_BACKSPACE = (#const SDL_SCANCODE_BACKSPACE) :: Scancode
pattern SDL_SCANCODE_TAB = (#const SDL_SCANCODE_TAB) :: Scancode
pattern SDL_SCANCODE_SPACE = (#const SDL_SCANCODE_SPACE) :: Scancode
pattern SDL_SCANCODE_MINUS = (#const SDL_SCANCODE_MINUS) :: Scancode
pattern SDL_SCANCODE_EQUALS = (#const SDL_SCANCODE_EQUALS) :: Scancode
pattern SDL_SCANCODE_LEFTBRACKET = (#const SDL_SCANCODE_LEFTBRACKET) :: Scancode
pattern SDL_SCANCODE_RIGHTBRACKET = (#const SDL_SCANCODE_RIGHTBRACKET) :: Scancode
pattern SDL_SCANCODE_BACKSLASH = (#const SDL_SCANCODE_BACKSLASH) :: Scancode
pattern SDL_SCANCODE_NONUSHASH = (#const SDL_SCANCODE_NONUSHASH) :: Scancode
pattern SDL_SCANCODE_SEMICOLON = (#const SDL_SCANCODE_SEMICOLON) :: Scancode
pattern SDL_SCANCODE_APOSTROPHE = (#const SDL_SCANCODE_APOSTROPHE) :: Scancode
pattern SDL_SCANCODE_GRAVE = (#const SDL_SCANCODE_GRAVE) :: Scancode
pattern SDL_SCANCODE_COMMA = (#const SDL_SCANCODE_COMMA) :: Scancode
pattern SDL_SCANCODE_PERIOD = (#const SDL_SCANCODE_PERIOD) :: Scancode
pattern SDL_SCANCODE_SLASH = (#const SDL_SCANCODE_SLASH) :: Scancode
pattern SDL_SCANCODE_CAPSLOCK = (#const SDL_SCANCODE_CAPSLOCK) :: Scancode
pattern SDL_SCANCODE_F1 = (#const SDL_SCANCODE_F1) :: Scancode
pattern SDL_SCANCODE_F2 = (#const SDL_SCANCODE_F2) :: Scancode
pattern SDL_SCANCODE_F3 = (#const SDL_SCANCODE_F3) :: Scancode
pattern SDL_SCANCODE_F4 = (#const SDL_SCANCODE_F4) :: Scancode
pattern SDL_SCANCODE_F5 = (#const SDL_SCANCODE_F5) :: Scancode
pattern SDL_SCANCODE_F6 = (#const SDL_SCANCODE_F6) :: Scancode
pattern SDL_SCANCODE_F7 = (#const SDL_SCANCODE_F7) :: Scancode
pattern SDL_SCANCODE_F8 = (#const SDL_SCANCODE_F8) :: Scancode
pattern SDL_SCANCODE_F9 = (#const SDL_SCANCODE_F9) :: Scancode
pattern SDL_SCANCODE_F10 = (#const SDL_SCANCODE_F10) :: Scancode
pattern SDL_SCANCODE_F11 = (#const SDL_SCANCODE_F11) :: Scancode
pattern SDL_SCANCODE_F12 = (#const SDL_SCANCODE_F12) :: Scancode
pattern SDL_SCANCODE_PRINTSCREEN = (#const SDL_SCANCODE_PRINTSCREEN) :: Scancode
pattern SDL_SCANCODE_SCROLLLOCK = (#const SDL_SCANCODE_SCROLLLOCK) :: Scancode
pattern SDL_SCANCODE_PAUSE = (#const SDL_SCANCODE_PAUSE) :: Scancode
pattern SDL_SCANCODE_INSERT = (#const SDL_SCANCODE_INSERT) :: Scancode
pattern SDL_SCANCODE_HOME = (#const SDL_SCANCODE_HOME) :: Scancode
pattern SDL_SCANCODE_PAGEUP = (#const SDL_SCANCODE_PAGEUP) :: Scancode
pattern SDL_SCANCODE_DELETE = (#const SDL_SCANCODE_DELETE) :: Scancode
pattern SDL_SCANCODE_END = (#const SDL_SCANCODE_END) :: Scancode
pattern SDL_SCANCODE_PAGEDOWN = (#const SDL_SCANCODE_PAGEDOWN) :: Scancode
pattern SDL_SCANCODE_RIGHT = (#const SDL_SCANCODE_RIGHT) :: Scancode
pattern SDL_SCANCODE_LEFT = (#const SDL_SCANCODE_LEFT) :: Scancode
pattern SDL_SCANCODE_DOWN = (#const SDL_SCANCODE_DOWN) :: Scancode
pattern SDL_SCANCODE_UP = (#const SDL_SCANCODE_UP) :: Scancode
pattern SDL_SCANCODE_NUMLOCKCLEAR = (#const SDL_SCANCODE_NUMLOCKCLEAR) :: Scancode
pattern SDL_SCANCODE_KP_DIVIDE = (#const SDL_SCANCODE_KP_DIVIDE) :: Scancode
pattern SDL_SCANCODE_KP_MULTIPLY = (#const SDL_SCANCODE_KP_MULTIPLY) :: Scancode
pattern SDL_SCANCODE_KP_MINUS = (#const SDL_SCANCODE_KP_MINUS) :: Scancode
pattern SDL_SCANCODE_KP_PLUS = (#const SDL_SCANCODE_KP_PLUS) :: Scancode
pattern SDL_SCANCODE_KP_ENTER = (#const SDL_SCANCODE_KP_ENTER) :: Scancode
pattern SDL_SCANCODE_KP_1 = (#const SDL_SCANCODE_KP_1) :: Scancode
pattern SDL_SCANCODE_KP_2 = (#const SDL_SCANCODE_KP_2) :: Scancode
pattern SDL_SCANCODE_KP_3 = (#const SDL_SCANCODE_KP_3) :: Scancode
pattern SDL_SCANCODE_KP_4 = (#const SDL_SCANCODE_KP_4) :: Scancode
pattern SDL_SCANCODE_KP_5 = (#const SDL_SCANCODE_KP_5) :: Scancode
pattern SDL_SCANCODE_KP_6 = (#const SDL_SCANCODE_KP_6) :: Scancode
pattern SDL_SCANCODE_KP_7 = (#const SDL_SCANCODE_KP_7) :: Scancode
pattern SDL_SCANCODE_KP_8 = (#const SDL_SCANCODE_KP_8) :: Scancode
pattern SDL_SCANCODE_KP_9 = (#const SDL_SCANCODE_KP_9) :: Scancode
pattern SDL_SCANCODE_KP_0 = (#const SDL_SCANCODE_KP_0) :: Scancode
pattern SDL_SCANCODE_KP_PERIOD = (#const SDL_SCANCODE_KP_PERIOD) :: Scancode
pattern SDL_SCANCODE_NONUSBACKSLASH = (#const SDL_SCANCODE_NONUSBACKSLASH) :: Scancode
pattern SDL_SCANCODE_APPLICATION = (#const SDL_SCANCODE_APPLICATION) :: Scancode
pattern SDL_SCANCODE_POWER = (#const SDL_SCANCODE_POWER) :: Scancode
pattern SDL_SCANCODE_KP_EQUALS = (#const SDL_SCANCODE_KP_EQUALS) :: Scancode
pattern SDL_SCANCODE_F13 = (#const SDL_SCANCODE_F13) :: Scancode
pattern SDL_SCANCODE_F14 = (#const SDL_SCANCODE_F14) :: Scancode
pattern SDL_SCANCODE_F15 = (#const SDL_SCANCODE_F15) :: Scancode
pattern SDL_SCANCODE_F16 = (#const SDL_SCANCODE_F16) :: Scancode
pattern SDL_SCANCODE_F17 = (#const SDL_SCANCODE_F17) :: Scancode
pattern SDL_SCANCODE_F18 = (#const SDL_SCANCODE_F18) :: Scancode
pattern SDL_SCANCODE_F19 = (#const SDL_SCANCODE_F19) :: Scancode
pattern SDL_SCANCODE_F20 = (#const SDL_SCANCODE_F20) :: Scancode
pattern SDL_SCANCODE_F21 = (#const SDL_SCANCODE_F21) :: Scancode
pattern SDL_SCANCODE_F22 = (#const SDL_SCANCODE_F22) :: Scancode
pattern SDL_SCANCODE_F23 = (#const SDL_SCANCODE_F23) :: Scancode
pattern SDL_SCANCODE_F24 = (#const SDL_SCANCODE_F24) :: Scancode
pattern SDL_SCANCODE_EXECUTE = (#const SDL_SCANCODE_EXECUTE) :: Scancode
pattern SDL_SCANCODE_HELP = (#const SDL_SCANCODE_HELP) :: Scancode
pattern SDL_SCANCODE_MENU = (#const SDL_SCANCODE_MENU) :: Scancode
pattern SDL_SCANCODE_SELECT = (#const SDL_SCANCODE_SELECT) :: Scancode
pattern SDL_SCANCODE_STOP = (#const SDL_SCANCODE_STOP) :: Scancode
pattern SDL_SCANCODE_AGAIN = (#const SDL_SCANCODE_AGAIN) :: Scancode
pattern SDL_SCANCODE_UNDO = (#const SDL_SCANCODE_UNDO) :: Scancode
pattern SDL_SCANCODE_CUT = (#const SDL_SCANCODE_CUT) :: Scancode
pattern SDL_SCANCODE_COPY = (#const SDL_SCANCODE_COPY) :: Scancode
pattern SDL_SCANCODE_PASTE = (#const SDL_SCANCODE_PASTE) :: Scancode
pattern SDL_SCANCODE_FIND = (#const SDL_SCANCODE_FIND) :: Scancode
pattern SDL_SCANCODE_MUTE = (#const SDL_SCANCODE_MUTE) :: Scancode
pattern SDL_SCANCODE_VOLUMEUP = (#const SDL_SCANCODE_VOLUMEUP) :: Scancode
pattern SDL_SCANCODE_VOLUMEDOWN = (#const SDL_SCANCODE_VOLUMEDOWN) :: Scancode
pattern SDL_SCANCODE_KP_COMMA = (#const SDL_SCANCODE_KP_COMMA) :: Scancode
pattern SDL_SCANCODE_KP_EQUALSAS400 = (#const SDL_SCANCODE_KP_EQUALSAS400) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL1 = (#const SDL_SCANCODE_INTERNATIONAL1) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL2 = (#const SDL_SCANCODE_INTERNATIONAL2) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL3 = (#const SDL_SCANCODE_INTERNATIONAL3) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL4 = (#const SDL_SCANCODE_INTERNATIONAL4) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL5 = (#const SDL_SCANCODE_INTERNATIONAL5) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL6 = (#const SDL_SCANCODE_INTERNATIONAL6) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL7 = (#const SDL_SCANCODE_INTERNATIONAL7) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL8 = (#const SDL_SCANCODE_INTERNATIONAL8) :: Scancode
pattern SDL_SCANCODE_INTERNATIONAL9 = (#const SDL_SCANCODE_INTERNATIONAL9) :: Scancode
pattern SDL_SCANCODE_LANG1 = (#const SDL_SCANCODE_LANG1) :: Scancode
pattern SDL_SCANCODE_LANG2 = (#const SDL_SCANCODE_LANG2) :: Scancode
pattern SDL_SCANCODE_LANG3 = (#const SDL_SCANCODE_LANG3) :: Scancode
pattern SDL_SCANCODE_LANG4 = (#const SDL_SCANCODE_LANG4) :: Scancode
pattern SDL_SCANCODE_LANG5 = (#const SDL_SCANCODE_LANG5) :: Scancode
pattern SDL_SCANCODE_LANG6 = (#const SDL_SCANCODE_LANG6) :: Scancode
pattern SDL_SCANCODE_LANG7 = (#const SDL_SCANCODE_LANG7) :: Scancode
pattern SDL_SCANCODE_LANG8 = (#const SDL_SCANCODE_LANG8) :: Scancode
pattern SDL_SCANCODE_LANG9 = (#const SDL_SCANCODE_LANG9) :: Scancode
pattern SDL_SCANCODE_ALTERASE = (#const SDL_SCANCODE_ALTERASE) :: Scancode
pattern SDL_SCANCODE_SYSREQ = (#const SDL_SCANCODE_SYSREQ) :: Scancode
pattern SDL_SCANCODE_CANCEL = (#const SDL_SCANCODE_CANCEL) :: Scancode
pattern SDL_SCANCODE_CLEAR = (#const SDL_SCANCODE_CLEAR) :: Scancode
pattern SDL_SCANCODE_PRIOR = (#const SDL_SCANCODE_PRIOR) :: Scancode
pattern SDL_SCANCODE_RETURN2 = (#const SDL_SCANCODE_RETURN2) :: Scancode
pattern SDL_SCANCODE_SEPARATOR = (#const SDL_SCANCODE_SEPARATOR) :: Scancode
pattern SDL_SCANCODE_OUT = (#const SDL_SCANCODE_OUT) :: Scancode
pattern SDL_SCANCODE_OPER = (#const SDL_SCANCODE_OPER) :: Scancode
pattern SDL_SCANCODE_CLEARAGAIN = (#const SDL_SCANCODE_CLEARAGAIN) :: Scancode
pattern SDL_SCANCODE_CRSEL = (#const SDL_SCANCODE_CRSEL) :: Scancode
pattern SDL_SCANCODE_EXSEL = (#const SDL_SCANCODE_EXSEL) :: Scancode
pattern SDL_SCANCODE_KP_00 = (#const SDL_SCANCODE_KP_00) :: Scancode
pattern SDL_SCANCODE_KP_000 = (#const SDL_SCANCODE_KP_000) :: Scancode
pattern SDL_SCANCODE_THOUSANDSSEPARATOR = (#const SDL_SCANCODE_THOUSANDSSEPARATOR) :: Scancode
pattern SDL_SCANCODE_DECIMALSEPARATOR = (#const SDL_SCANCODE_DECIMALSEPARATOR) :: Scancode
pattern SDL_SCANCODE_CURRENCYUNIT = (#const SDL_SCANCODE_CURRENCYUNIT) :: Scancode
pattern SDL_SCANCODE_CURRENCYSUBUNIT = (#const SDL_SCANCODE_CURRENCYSUBUNIT) :: Scancode
pattern SDL_SCANCODE_KP_LEFTPAREN = (#const SDL_SCANCODE_KP_LEFTPAREN) :: Scancode
pattern SDL_SCANCODE_KP_RIGHTPAREN = (#const SDL_SCANCODE_KP_RIGHTPAREN) :: Scancode
pattern SDL_SCANCODE_KP_LEFTBRACE = (#const SDL_SCANCODE_KP_LEFTBRACE) :: Scancode
pattern SDL_SCANCODE_KP_RIGHTBRACE = (#const SDL_SCANCODE_KP_RIGHTBRACE) :: Scancode
pattern SDL_SCANCODE_KP_TAB = (#const SDL_SCANCODE_KP_TAB) :: Scancode
pattern SDL_SCANCODE_KP_BACKSPACE = (#const SDL_SCANCODE_KP_BACKSPACE) :: Scancode
pattern SDL_SCANCODE_KP_A = (#const SDL_SCANCODE_KP_A) :: Scancode
pattern SDL_SCANCODE_KP_B = (#const SDL_SCANCODE_KP_B) :: Scancode
pattern SDL_SCANCODE_KP_C = (#const SDL_SCANCODE_KP_C) :: Scancode
pattern SDL_SCANCODE_KP_D = (#const SDL_SCANCODE_KP_D) :: Scancode
pattern SDL_SCANCODE_KP_E = (#const SDL_SCANCODE_KP_E) :: Scancode
pattern SDL_SCANCODE_KP_F = (#const SDL_SCANCODE_KP_F) :: Scancode
pattern SDL_SCANCODE_KP_XOR = (#const SDL_SCANCODE_KP_XOR) :: Scancode
pattern SDL_SCANCODE_KP_POWER = (#const SDL_SCANCODE_KP_POWER) :: Scancode
pattern SDL_SCANCODE_KP_PERCENT = (#const SDL_SCANCODE_KP_PERCENT) :: Scancode
pattern SDL_SCANCODE_KP_LESS = (#const SDL_SCANCODE_KP_LESS) :: Scancode
pattern SDL_SCANCODE_KP_GREATER = (#const SDL_SCANCODE_KP_GREATER) :: Scancode
pattern SDL_SCANCODE_KP_AMPERSAND = (#const SDL_SCANCODE_KP_AMPERSAND) :: Scancode
pattern SDL_SCANCODE_KP_DBLAMPERSAND = (#const SDL_SCANCODE_KP_DBLAMPERSAND) :: Scancode
pattern SDL_SCANCODE_KP_VERTICALBAR = (#const SDL_SCANCODE_KP_VERTICALBAR) :: Scancode
pattern SDL_SCANCODE_KP_DBLVERTICALBAR = (#const SDL_SCANCODE_KP_DBLVERTICALBAR) :: Scancode
pattern SDL_SCANCODE_KP_COLON = (#const SDL_SCANCODE_KP_COLON) :: Scancode
pattern SDL_SCANCODE_KP_HASH = (#const SDL_SCANCODE_KP_HASH) :: Scancode
pattern SDL_SCANCODE_KP_SPACE = (#const SDL_SCANCODE_KP_SPACE) :: Scancode
pattern SDL_SCANCODE_KP_AT = (#const SDL_SCANCODE_KP_AT) :: Scancode
pattern SDL_SCANCODE_KP_EXCLAM = (#const SDL_SCANCODE_KP_EXCLAM) :: Scancode
pattern SDL_SCANCODE_KP_MEMSTORE = (#const SDL_SCANCODE_KP_MEMSTORE) :: Scancode
pattern SDL_SCANCODE_KP_MEMRECALL = (#const SDL_SCANCODE_KP_MEMRECALL) :: Scancode
pattern SDL_SCANCODE_KP_MEMCLEAR = (#const SDL_SCANCODE_KP_MEMCLEAR) :: Scancode
pattern SDL_SCANCODE_KP_MEMADD = (#const SDL_SCANCODE_KP_MEMADD) :: Scancode
pattern SDL_SCANCODE_KP_MEMSUBTRACT = (#const SDL_SCANCODE_KP_MEMSUBTRACT) :: Scancode
pattern SDL_SCANCODE_KP_MEMMULTIPLY = (#const SDL_SCANCODE_KP_MEMMULTIPLY) :: Scancode
pattern SDL_SCANCODE_KP_MEMDIVIDE = (#const SDL_SCANCODE_KP_MEMDIVIDE) :: Scancode
pattern SDL_SCANCODE_KP_PLUSMINUS = (#const SDL_SCANCODE_KP_PLUSMINUS) :: Scancode
pattern SDL_SCANCODE_KP_CLEAR = (#const SDL_SCANCODE_KP_CLEAR) :: Scancode
pattern SDL_SCANCODE_KP_CLEARENTRY = (#const SDL_SCANCODE_KP_CLEARENTRY) :: Scancode
pattern SDL_SCANCODE_KP_BINARY = (#const SDL_SCANCODE_KP_BINARY) :: Scancode
pattern SDL_SCANCODE_KP_OCTAL = (#const SDL_SCANCODE_KP_OCTAL) :: Scancode
pattern SDL_SCANCODE_KP_DECIMAL = (#const SDL_SCANCODE_KP_DECIMAL) :: Scancode
pattern SDL_SCANCODE_KP_HEXADECIMAL = (#const SDL_SCANCODE_KP_HEXADECIMAL) :: Scancode
pattern SDL_SCANCODE_LCTRL = (#const SDL_SCANCODE_LCTRL) :: Scancode
pattern SDL_SCANCODE_LSHIFT = (#const SDL_SCANCODE_LSHIFT) :: Scancode
pattern SDL_SCANCODE_LALT = (#const SDL_SCANCODE_LALT) :: Scancode
pattern SDL_SCANCODE_LGUI = (#const SDL_SCANCODE_LGUI) :: Scancode
pattern SDL_SCANCODE_RCTRL = (#const SDL_SCANCODE_RCTRL) :: Scancode
pattern SDL_SCANCODE_RSHIFT = (#const SDL_SCANCODE_RSHIFT) :: Scancode
pattern SDL_SCANCODE_RALT = (#const SDL_SCANCODE_RALT) :: Scancode
pattern SDL_SCANCODE_RGUI = (#const SDL_SCANCODE_RGUI) :: Scancode
pattern SDL_SCANCODE_MODE = (#const SDL_SCANCODE_MODE) :: Scancode
pattern SDL_SCANCODE_AUDIONEXT = (#const SDL_SCANCODE_AUDIONEXT) :: Scancode
pattern SDL_SCANCODE_AUDIOPREV = (#const SDL_SCANCODE_AUDIOPREV) :: Scancode
pattern SDL_SCANCODE_AUDIOSTOP = (#const SDL_SCANCODE_AUDIOSTOP) :: Scancode
pattern SDL_SCANCODE_AUDIOPLAY = (#const SDL_SCANCODE_AUDIOPLAY) :: Scancode
pattern SDL_SCANCODE_AUDIOMUTE = (#const SDL_SCANCODE_AUDIOMUTE) :: Scancode
pattern SDL_SCANCODE_MEDIASELECT = (#const SDL_SCANCODE_MEDIASELECT) :: Scancode
pattern SDL_SCANCODE_WWW = (#const SDL_SCANCODE_WWW) :: Scancode
pattern SDL_SCANCODE_MAIL = (#const SDL_SCANCODE_MAIL) :: Scancode
pattern SDL_SCANCODE_CALCULATOR = (#const SDL_SCANCODE_CALCULATOR) :: Scancode
pattern SDL_SCANCODE_COMPUTER = (#const SDL_SCANCODE_COMPUTER) :: Scancode
pattern SDL_SCANCODE_AC_SEARCH = (#const SDL_SCANCODE_AC_SEARCH) :: Scancode
pattern SDL_SCANCODE_AC_HOME = (#const SDL_SCANCODE_AC_HOME) :: Scancode
pattern SDL_SCANCODE_AC_BACK = (#const SDL_SCANCODE_AC_BACK) :: Scancode
pattern SDL_SCANCODE_AC_FORWARD = (#const SDL_SCANCODE_AC_FORWARD) :: Scancode
pattern SDL_SCANCODE_AC_STOP = (#const SDL_SCANCODE_AC_STOP) :: Scancode
pattern SDL_SCANCODE_AC_REFRESH = (#const SDL_SCANCODE_AC_REFRESH) :: Scancode
pattern SDL_SCANCODE_AC_BOOKMARKS = (#const SDL_SCANCODE_AC_BOOKMARKS) :: Scancode
pattern SDL_SCANCODE_BRIGHTNESSDOWN = (#const SDL_SCANCODE_BRIGHTNESSDOWN) :: Scancode
pattern SDL_SCANCODE_BRIGHTNESSUP = (#const SDL_SCANCODE_BRIGHTNESSUP) :: Scancode
pattern SDL_SCANCODE_DISPLAYSWITCH = (#const SDL_SCANCODE_DISPLAYSWITCH) :: Scancode
pattern SDL_SCANCODE_KBDILLUMTOGGLE = (#const SDL_SCANCODE_KBDILLUMTOGGLE) :: Scancode
pattern SDL_SCANCODE_KBDILLUMDOWN = (#const SDL_SCANCODE_KBDILLUMDOWN) :: Scancode
pattern SDL_SCANCODE_KBDILLUMUP = (#const SDL_SCANCODE_KBDILLUMUP) :: Scancode
pattern SDL_SCANCODE_EJECT = (#const SDL_SCANCODE_EJECT) :: Scancode
pattern SDL_SCANCODE_SLEEP = (#const SDL_SCANCODE_SLEEP) :: Scancode
pattern SDL_SCANCODE_APP1 = (#const SDL_SCANCODE_APP1) :: Scancode
pattern SDL_SCANCODE_APP2 = (#const SDL_SCANCODE_APP2) :: Scancode
pattern SDL_NUM_SCANCODES = (#const SDL_NUM_SCANCODES) :: Scancode

pattern SDL_SYSTEM_CURSOR_ARROW = (#const SDL_SYSTEM_CURSOR_ARROW) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_IBEAM = (#const SDL_SYSTEM_CURSOR_IBEAM) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_WAIT = (#const SDL_SYSTEM_CURSOR_WAIT) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_CROSSHAIR = (#const SDL_SYSTEM_CURSOR_CROSSHAIR) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_WAITARROW = (#const SDL_SYSTEM_CURSOR_WAITARROW) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_SIZENWSE = (#const SDL_SYSTEM_CURSOR_SIZENWSE) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_SIZENESW = (#const SDL_SYSTEM_CURSOR_SIZENESW) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_SIZEWE = (#const SDL_SYSTEM_CURSOR_SIZEWE) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_SIZENS = (#const SDL_SYSTEM_CURSOR_SIZENS) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_SIZEALL = (#const SDL_SYSTEM_CURSOR_SIZEALL) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_NO = (#const SDL_SYSTEM_CURSOR_NO) :: SystemCursor
pattern SDL_SYSTEM_CURSOR_HAND = (#const SDL_SYSTEM_CURSOR_HAND) :: SystemCursor
pattern SDL_NUM_SYSTEM_CURSORS = (#const SDL_NUM_SYSTEM_CURSORS) :: SystemCursor

pattern SDL_THREAD_PRIORITY_LOW = (#const SDL_THREAD_PRIORITY_LOW) :: ThreadPriority
pattern SDL_THREAD_PRIORITY_NORMAL = (#const SDL_THREAD_PRIORITY_NORMAL) :: ThreadPriority
pattern SDL_THREAD_PRIORITY_HIGH = (#const SDL_THREAD_PRIORITY_HIGH) :: ThreadPriority

pattern SDL_AUDIO_ALLOW_FREQUENCY_CHANGE = (#const SDL_AUDIO_ALLOW_FREQUENCY_CHANGE)
pattern SDL_AUDIO_ALLOW_FORMAT_CHANGE = (#const SDL_AUDIO_ALLOW_FORMAT_CHANGE)
pattern SDL_AUDIO_ALLOW_CHANNELS_CHANGE = (#const SDL_AUDIO_ALLOW_CHANNELS_CHANGE)
pattern SDL_AUDIO_ALLOW_ANY_CHANGE = (#const SDL_AUDIO_ALLOW_ANY_CHANGE)

pattern SDL_BUTTON_LEFT = (#const SDL_BUTTON_LEFT)
pattern SDL_BUTTON_MIDDLE = (#const SDL_BUTTON_MIDDLE)
pattern SDL_BUTTON_RIGHT = (#const SDL_BUTTON_RIGHT)
pattern SDL_BUTTON_X1 = (#const SDL_BUTTON_X1)
pattern SDL_BUTTON_X2 = (#const SDL_BUTTON_X2)
pattern SDL_BUTTON_LMASK = (#const SDL_BUTTON_LMASK)
pattern SDL_BUTTON_MMASK = (#const SDL_BUTTON_MMASK)
pattern SDL_BUTTON_RMASK = (#const SDL_BUTTON_RMASK)
pattern SDL_BUTTON_X1MASK = (#const SDL_BUTTON_X1MASK)
pattern SDL_BUTTON_X2MASK = (#const SDL_BUTTON_X2MASK)

pattern SDL_MOUSEWHEEL_NORMAL = (#const SDL_MOUSEWHEEL_NORMAL)
pattern SDL_MOUSEWHEEL_FLIPPED = (#const SDL_MOUSEWHEEL_FLIPPED)

pattern SDL_FIRSTEVENT = (#const SDL_FIRSTEVENT)
pattern SDL_QUIT = (#const SDL_QUIT)
pattern SDL_APP_TERMINATING = (#const SDL_APP_TERMINATING)
pattern SDL_APP_LOWMEMORY = (#const SDL_APP_LOWMEMORY)
pattern SDL_APP_WILLENTERBACKGROUND = (#const SDL_APP_WILLENTERBACKGROUND)
pattern SDL_APP_DIDENTERBACKGROUND = (#const SDL_APP_DIDENTERBACKGROUND)
pattern SDL_APP_WILLENTERFOREGROUND = (#const SDL_APP_WILLENTERFOREGROUND)
pattern SDL_APP_DIDENTERFOREGROUND = (#const SDL_APP_DIDENTERFOREGROUND)
pattern SDL_WINDOWEVENT = (#const SDL_WINDOWEVENT)
pattern SDL_SYSWMEVENT = (#const SDL_SYSWMEVENT)
pattern SDL_KEYDOWN = (#const SDL_KEYDOWN)
pattern SDL_KEYUP = (#const SDL_KEYUP)
pattern SDL_TEXTEDITING = (#const SDL_TEXTEDITING)
pattern SDL_TEXTINPUT = (#const SDL_TEXTINPUT)
pattern SDL_KEYMAPCHANGED = (#const SDL_KEYMAPCHANGED)
pattern SDL_MOUSEMOTION = (#const SDL_MOUSEMOTION)
pattern SDL_MOUSEBUTTONDOWN = (#const SDL_MOUSEBUTTONDOWN)
pattern SDL_MOUSEBUTTONUP = (#const SDL_MOUSEBUTTONUP)
pattern SDL_MOUSEWHEEL = (#const SDL_MOUSEWHEEL)
pattern SDL_JOYAXISMOTION = (#const SDL_JOYAXISMOTION)
pattern SDL_JOYBALLMOTION = (#const SDL_JOYBALLMOTION)
pattern SDL_JOYHATMOTION = (#const SDL_JOYHATMOTION)
pattern SDL_JOYBUTTONDOWN = (#const SDL_JOYBUTTONDOWN)
pattern SDL_JOYBUTTONUP = (#const SDL_JOYBUTTONUP)
pattern SDL_JOYDEVICEADDED = (#const SDL_JOYDEVICEADDED)
pattern SDL_JOYDEVICEREMOVED = (#const SDL_JOYDEVICEREMOVED)
pattern SDL_CONTROLLERAXISMOTION = (#const SDL_CONTROLLERAXISMOTION)
pattern SDL_CONTROLLERBUTTONDOWN = (#const SDL_CONTROLLERBUTTONDOWN)
pattern SDL_CONTROLLERBUTTONUP = (#const SDL_CONTROLLERBUTTONUP)
pattern SDL_CONTROLLERDEVICEADDED = (#const SDL_CONTROLLERDEVICEADDED)
pattern SDL_CONTROLLERDEVICEREMOVED = (#const SDL_CONTROLLERDEVICEREMOVED)
pattern SDL_CONTROLLERDEVICEREMAPPED = (#const SDL_CONTROLLERDEVICEREMAPPED)
pattern SDL_FINGERDOWN = (#const SDL_FINGERDOWN)
pattern SDL_FINGERUP = (#const SDL_FINGERUP)
pattern SDL_FINGERMOTION = (#const SDL_FINGERMOTION)
pattern SDL_DOLLARGESTURE = (#const SDL_DOLLARGESTURE)
pattern SDL_DOLLARRECORD = (#const SDL_DOLLARRECORD)
pattern SDL_MULTIGESTURE = (#const SDL_MULTIGESTURE)
pattern SDL_CLIPBOARDUPDATE = (#const SDL_CLIPBOARDUPDATE)
pattern SDL_DROPFILE = (#const SDL_DROPFILE)
pattern SDL_AUDIODEVICEADDED = (#const SDL_AUDIODEVICEADDED)
pattern SDL_AUDIODEVICEREMOVED = (#const SDL_AUDIODEVICEREMOVED)
pattern SDL_RENDER_TARGETS_RESET = (#const SDL_RENDER_TARGETS_RESET)
pattern SDL_RENDER_DEVICE_RESET = (#const SDL_RENDER_DEVICE_RESET)
pattern SDL_USEREVENT = (#const SDL_USEREVENT)
pattern SDL_LASTEVENT = (#const SDL_LASTEVENT)

pattern SDL_HAT_CENTERED = (#const SDL_HAT_CENTERED)
pattern SDL_HAT_UP = (#const SDL_HAT_UP)
pattern SDL_HAT_RIGHT = (#const SDL_HAT_RIGHT)
pattern SDL_HAT_DOWN = (#const SDL_HAT_DOWN)
pattern SDL_HAT_LEFT = (#const SDL_HAT_LEFT)
pattern SDL_HAT_RIGHTUP = (#const SDL_HAT_RIGHTUP)
pattern SDL_HAT_RIGHTDOWN = (#const SDL_HAT_RIGHTDOWN)
pattern SDL_HAT_LEFTUP = (#const SDL_HAT_LEFTUP)
pattern SDL_HAT_LEFTDOWN = (#const SDL_HAT_LEFTDOWN)

pattern SDL_PRESSED = (#const SDL_PRESSED)
pattern SDL_RELEASED = (#const SDL_RELEASED)

pattern SDL_LOG_CATEGORY_APPLICATION = (#const SDL_LOG_CATEGORY_APPLICATION)
pattern SDL_LOG_CATEGORY_ERROR = (#const SDL_LOG_CATEGORY_ERROR)
pattern SDL_LOG_CATEGORY_ASSERT = (#const SDL_LOG_CATEGORY_ASSERT)
pattern SDL_LOG_CATEGORY_SYSTEM = (#const SDL_LOG_CATEGORY_SYSTEM)
pattern SDL_LOG_CATEGORY_AUDIO = (#const SDL_LOG_CATEGORY_AUDIO)
pattern SDL_LOG_CATEGORY_VIDEO = (#const SDL_LOG_CATEGORY_VIDEO)
pattern SDL_LOG_CATEGORY_RENDER = (#const SDL_LOG_CATEGORY_RENDER)
pattern SDL_LOG_CATEGORY_INPUT = (#const SDL_LOG_CATEGORY_INPUT)
pattern SDL_LOG_CATEGORY_TEST = (#const SDL_LOG_CATEGORY_TEST)
pattern SDL_LOG_CATEGORY_CUSTOM = (#const SDL_LOG_CATEGORY_CUSTOM)

pattern SDL_MESSAGEBOX_ERROR = (#const SDL_MESSAGEBOX_ERROR)
pattern SDL_MESSAGEBOX_WARNING = (#const SDL_MESSAGEBOX_WARNING)
pattern SDL_MESSAGEBOX_INFORMATION = (#const SDL_MESSAGEBOX_INFORMATION)

pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = (#const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT)
pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = (#const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT)

pattern SDL_GL_CONTEXT_PROFILE_CORE = (#const SDL_GL_CONTEXT_PROFILE_CORE)
pattern SDL_GL_CONTEXT_PROFILE_COMPATIBILITY = (#const SDL_GL_CONTEXT_PROFILE_COMPATIBILITY)
pattern SDL_GL_CONTEXT_PROFILE_ES = (#const SDL_GL_CONTEXT_PROFILE_ES)

pattern SDL_GL_CONTEXT_DEBUG_FLAG = (#const SDL_GL_CONTEXT_DEBUG_FLAG)
pattern SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = (#const SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG)
pattern SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG = (#const SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG)
pattern SDL_GL_CONTEXT_RESET_ISOLATION_FLAG = (#const SDL_GL_CONTEXT_RESET_ISOLATION_FLAG)

pattern SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE = (#const SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE)
pattern SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH = (#const SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH)

pattern SDL_PIXELFORMAT_UNKNOWN = (#const SDL_PIXELFORMAT_UNKNOWN)
pattern SDL_PIXELFORMAT_INDEX1LSB = (#const SDL_PIXELFORMAT_INDEX1LSB)
pattern SDL_PIXELFORMAT_INDEX1MSB = (#const SDL_PIXELFORMAT_INDEX1MSB)
pattern SDL_PIXELFORMAT_INDEX4LSB = (#const SDL_PIXELFORMAT_INDEX4LSB)
pattern SDL_PIXELFORMAT_INDEX4MSB = (#const SDL_PIXELFORMAT_INDEX4MSB)
pattern SDL_PIXELFORMAT_INDEX8 = (#const SDL_PIXELFORMAT_INDEX8)
pattern SDL_PIXELFORMAT_RGB332 = (#const SDL_PIXELFORMAT_RGB332)
pattern SDL_PIXELFORMAT_RGB444 = (#const SDL_PIXELFORMAT_RGB444)
pattern SDL_PIXELFORMAT_RGB555 = (#const SDL_PIXELFORMAT_RGB555)
pattern SDL_PIXELFORMAT_BGR555 = (#const SDL_PIXELFORMAT_BGR555)
pattern SDL_PIXELFORMAT_ARGB4444 = (#const SDL_PIXELFORMAT_ARGB4444)
pattern SDL_PIXELFORMAT_RGBA4444 = (#const SDL_PIXELFORMAT_RGBA4444)
pattern SDL_PIXELFORMAT_ABGR4444 = (#const SDL_PIXELFORMAT_ABGR4444)
pattern SDL_PIXELFORMAT_BGRA4444 = (#const SDL_PIXELFORMAT_BGRA4444)
pattern SDL_PIXELFORMAT_ARGB1555 = (#const SDL_PIXELFORMAT_ARGB1555)
pattern SDL_PIXELFORMAT_RGBA5551 = (#const SDL_PIXELFORMAT_RGBA5551)
pattern SDL_PIXELFORMAT_ABGR1555 = (#const SDL_PIXELFORMAT_ABGR1555)
pattern SDL_PIXELFORMAT_BGRA5551 = (#const SDL_PIXELFORMAT_BGRA5551)
pattern SDL_PIXELFORMAT_RGB565 = (#const SDL_PIXELFORMAT_RGB565)
pattern SDL_PIXELFORMAT_BGR565 = (#const SDL_PIXELFORMAT_BGR565)
pattern SDL_PIXELFORMAT_RGB24 = (#const SDL_PIXELFORMAT_RGB24)
pattern SDL_PIXELFORMAT_BGR24 = (#const SDL_PIXELFORMAT_BGR24)
pattern SDL_PIXELFORMAT_RGB888 = (#const SDL_PIXELFORMAT_RGB888)
pattern SDL_PIXELFORMAT_RGBX8888 = (#const SDL_PIXELFORMAT_RGBX8888)
pattern SDL_PIXELFORMAT_BGR888 = (#const SDL_PIXELFORMAT_BGR888)
pattern SDL_PIXELFORMAT_BGRX8888 = (#const SDL_PIXELFORMAT_BGRX8888)
pattern SDL_PIXELFORMAT_ARGB8888 = (#const SDL_PIXELFORMAT_ARGB8888)
pattern SDL_PIXELFORMAT_RGBA8888 = (#const SDL_PIXELFORMAT_RGBA8888)
pattern SDL_PIXELFORMAT_ABGR8888 = (#const SDL_PIXELFORMAT_ABGR8888)
pattern SDL_PIXELFORMAT_BGRA8888 = (#const SDL_PIXELFORMAT_BGRA8888)
pattern SDL_PIXELFORMAT_ARGB2101010 = (#const SDL_PIXELFORMAT_ARGB2101010)
pattern SDL_PIXELFORMAT_YV12 = (#const SDL_PIXELFORMAT_YV12)
pattern SDL_PIXELFORMAT_IYUV = (#const SDL_PIXELFORMAT_IYUV)
pattern SDL_PIXELFORMAT_YUY2 = (#const SDL_PIXELFORMAT_YUY2)
pattern SDL_PIXELFORMAT_UYVY = (#const SDL_PIXELFORMAT_UYVY)
pattern SDL_PIXELFORMAT_YVYU = (#const SDL_PIXELFORMAT_YVYU)

pattern SDL_RENDERER_SOFTWARE = (#const SDL_RENDERER_SOFTWARE)
pattern SDL_RENDERER_ACCELERATED = (#const SDL_RENDERER_ACCELERATED)
pattern SDL_RENDERER_PRESENTVSYNC = (#const SDL_RENDERER_PRESENTVSYNC)
pattern SDL_RENDERER_TARGETTEXTURE = (#const SDL_RENDERER_TARGETTEXTURE)

pattern SDL_TEXTUREACCESS_STATIC = (#const SDL_TEXTUREACCESS_STATIC)
pattern SDL_TEXTUREACCESS_STREAMING = (#const SDL_TEXTUREACCESS_STREAMING)
pattern SDL_TEXTUREACCESS_TARGET = (#const SDL_TEXTUREACCESS_TARGET)

pattern SDL_TEXTUREMODULATE_NONE = (#const SDL_TEXTUREMODULATE_NONE)
pattern SDL_TEXTUREMODULATE_COLOR = (#const SDL_TEXTUREMODULATE_COLOR)
pattern SDL_TEXTUREMODULATE_ALPHA = (#const SDL_TEXTUREMODULATE_ALPHA)

pattern SDL_TOUCH_MOUSEID = (#const SDL_TOUCH_MOUSEID)

pattern SDL_WINDOWEVENT_NONE = (#const SDL_WINDOWEVENT_NONE)
pattern SDL_WINDOWEVENT_SHOWN = (#const SDL_WINDOWEVENT_SHOWN)
pattern SDL_WINDOWEVENT_HIDDEN = (#const SDL_WINDOWEVENT_HIDDEN)
pattern SDL_WINDOWEVENT_EXPOSED = (#const SDL_WINDOWEVENT_EXPOSED)
pattern SDL_WINDOWEVENT_MOVED = (#const SDL_WINDOWEVENT_MOVED)
pattern SDL_WINDOWEVENT_RESIZED = (#const SDL_WINDOWEVENT_RESIZED)
pattern SDL_WINDOWEVENT_SIZE_CHANGED = (#const SDL_WINDOWEVENT_SIZE_CHANGED)
pattern SDL_WINDOWEVENT_MINIMIZED = (#const SDL_WINDOWEVENT_MINIMIZED)
pattern SDL_WINDOWEVENT_MAXIMIZED = (#const SDL_WINDOWEVENT_MAXIMIZED)
pattern SDL_WINDOWEVENT_RESTORED = (#const SDL_WINDOWEVENT_RESTORED)
pattern SDL_WINDOWEVENT_ENTER = (#const SDL_WINDOWEVENT_ENTER)
pattern SDL_WINDOWEVENT_LEAVE = (#const SDL_WINDOWEVENT_LEAVE)
pattern SDL_WINDOWEVENT_FOCUS_GAINED = (#const SDL_WINDOWEVENT_FOCUS_GAINED)
pattern SDL_WINDOWEVENT_FOCUS_LOST = (#const SDL_WINDOWEVENT_FOCUS_LOST)
pattern SDL_WINDOWEVENT_CLOSE = (#const SDL_WINDOWEVENT_CLOSE)

pattern SDL_WINDOW_FULLSCREEN = (#const SDL_WINDOW_FULLSCREEN)
pattern SDL_WINDOW_OPENGL = (#const SDL_WINDOW_OPENGL)
pattern SDL_WINDOW_SHOWN = (#const SDL_WINDOW_SHOWN)
pattern SDL_WINDOW_HIDDEN = (#const SDL_WINDOW_HIDDEN)
pattern SDL_WINDOW_BORDERLESS = (#const SDL_WINDOW_BORDERLESS)
pattern SDL_WINDOW_RESIZABLE = (#const SDL_WINDOW_RESIZABLE)
pattern SDL_WINDOW_MINIMIZED = (#const SDL_WINDOW_MINIMIZED)
pattern SDL_WINDOW_MAXIMIZED = (#const SDL_WINDOW_MAXIMIZED)
pattern SDL_WINDOW_INPUT_GRABBED = (#const SDL_WINDOW_INPUT_GRABBED)
pattern SDL_WINDOW_INPUT_FOCUS = (#const SDL_WINDOW_INPUT_FOCUS)
pattern SDL_WINDOW_MOUSE_FOCUS = (#const SDL_WINDOW_MOUSE_FOCUS)
pattern SDL_WINDOW_FULLSCREEN_DESKTOP = (#const SDL_WINDOW_FULLSCREEN_DESKTOP)
pattern SDL_WINDOW_FOREIGN = (#const SDL_WINDOW_FOREIGN)
pattern SDL_WINDOW_ALLOW_HIGHDPI = (#const SDL_WINDOW_ALLOW_HIGHDPI)
pattern SDL_WINDOW_MOUSE_CAPTURE = (#const SDL_WINDOW_MOUSE_CAPTURE)
pattern SDL_WINDOW_VULKAN = (#const SDL_WINDOW_VULKAN)

pattern SDL_WINDOWPOS_UNDEFINED = (#const SDL_WINDOWPOS_UNDEFINED)
pattern SDL_WINDOWPOS_CENTERED = (#const SDL_WINDOWPOS_CENTERED)

pattern SDL_HAPTIC_CONSTANT = (#const SDL_HAPTIC_CONSTANT)
