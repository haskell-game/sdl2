2.5.2.0
=======

* Correct SDL.Raw.Video.vkLoadLibrary to correctly call vkLoadLibraryFFI, rather
  than setClipboardTextFFI. See https://github.com/haskell-game/sdl2/pull/209.

2.5.1.0
=======

* Support `linear-1.21`


2.5.0.0
=======

* Version 2.0.6 of the SDL2 C library is now required.

* Added Vulkan support. See `SDL.Video.WindowGraphicsContext` data type and `SDL.Video.Vulkan` module.

* Support `StateVar` < 1.3


2.4.1.0
=======

* More SDL functions no longer allocate. See
  https://github.com/haskell-game/sdl2/pull/179 and
  https://github.com/haskell-game/sdl2/issues/178. Thanks to @chrisdone for this
  work.


* Fixed an off-by-one bug in `SDL.Input.Mouse.getMouseButtons`. See
  https://github.com/haskell-game/sdl2/pull/177 for more information. Thanks to
  @Linearity for identifying and fixing this bug.


2.4.0.1
=======
* Raise upper bounds for `exceptions` to <0.11


2.4.0
=====
* `getRelativeMouseLocation` now returns `Vector` instead of `Point`.
* `getModalMouseLocation` returns either a `Vector` or a `Point`, wrapped in (new) `ModalLocation`.
* `WindowSizeChangedEventData` includes the new window size.
* Added an invalid state to the `ControllerButtonState` enum, and switched to a more reliable state detection method.
* Raise upper bounds for `exceptions` to <0.10


2.3.0
=====

* Windows builds now use `-D_SDL_main_h`. See https://github.com/haskell-game/sdl2/issues/139 for more discussion.
* Some basic support for game controller events have been added. See `SDL.Input.GameController` and changes to `ControllerDeviceEventData`.
* Support for event watching: `addEventWatch` and `delEventWatch`.
* High-level bindings now distinguish between finger down / motion / up.
  See `SDL.Event.TouchFingerEvent` and `SDL.Event.TouchFingerMotionEvent`.
* Several event payloads now have their `Window` fields modified to use `Maybe Window`, substituting `Nothing` for null pointers.
* High-level structure for controller button state: `ControllerButtonState`.
* High-level structure for controller buttons: `ControllerButton`.
* High-level structure for controller connection: `ControllerDeviceConnection`.
* High-level structure for joystick device connection: `JoyDeviceConnection`.
* High-level structure for joystick button state: `JoyButtonState`.
* Support for user defined events: `registerEvent`, `pushRegisteredEvent`, and `getRegisteredEvent`.
* Initial window visibility can be specified in `WindowConfig` for `createWindow` function.
* `WarpMouseOrigin` is now fully exported and can warp to global coordinates.
* It's possible to retrieve palette information with `paletteNColors`, `paletteColors` and `palletColor`.


2.2.0
=====

* Version 2.0.4 of the SDL2 C library is now required:
  * `SDL.Event`:
    * Add `AudioDeviceEvent` constructor to `Event`
    * Add `KeymapChangedEvent` constructor to `EventPayload`
    * Add `mouseWheelEventDirection` field to `MouseWheelEventData`
  * `SDL.Input.Mouse`:
    * Add `MouseScrollDirection` enumeration
  * `SDL.Raw.Audio`:
    * Add `clearQueuedAudio` function
    * Add `getQueuedAudioSize` function
    * Add `queueAudio` function
  * `SDL.Raw.Enum`:
    * Add `SDL_GL_CONTEXT_RELEASE_BEHAVIOR` pattern synonym
    * Add `JoystickPowerLevel` pattern synonyms
    * Add `SDL_MOUSEWHEEL_NORMAL` and `SDL_MOUSEWHEEL_FLIPPED` pattern synonyms
    * Add `SDL_KEYMAPCHANGED`, `SDL_AUDIODEVICEADDED`, `SDL_AUDIODEVICEREMOVED`,
      `SDL_RENDER_TARGETS_RESET`, and `SDL_RENDER_DEVICE_RESET` pattern synonyms
    * Add `SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE` and
      `SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH` pattern synonyms
    * Add `SDL_WINDOW_MOUSE_CAPTURE` pattern synonym
  * `SDL.Raw.Event`:
    * Add `captureMouse` function
    * Add `getGlobalMouseState` function
    * Add `warpMouseGlobal` function
    * Add `joystickCurrentPowerLevel` function
    * Add `joystickFromInstanceID` function
    * Add `gameControllerFromInstanceID` function
  * `SDL.Raw.Event`:
    * Add `AudioDeviceEvent` constructor to `Event`
    * Add `KeymapChangedEvent` constructor to `Event`
    * Add `mouseWheelEventDirection` field to `MouseMotionEvent` constructor

* Add `SDL.Exception` module, exposing `SDLException`
* Add new function, `createSoftwareRenderer`, to render onto a surface
* Add joystick POV hat support
* Remove deprecated functionality:
  * `InitEverything` enumeration in `SDL.Init`
  * `mapRGB` in `SDL.Video.Renderer`
  * `setRelativeMouseMode` in `SDL.Input.Mouse`
  * `getRelativeMouseMode` in `SDL.Input.Mouse`
  * `getMouseLocation` in `SDL.Input.Mouse`
* Remove `ClipboardUpdateEventData`
* Merge `isScreenSaverEnabled`, `enableScreenSaver`, and `disableScreenSaver`
  into a `screenSaverEnabled` StateVar.
* Make function `surfaceBlit` in `SDL.Video.Renderer` return final blit
  rectangle post-clipping.
* Make all fields in EventData constructors strict
* Fix issue with `setWindowMode` transitions not working properly between
  fullscreen and windowed modes.

2.1.3.1
=======

* Raise upper bounds for `vector` to <0.13

2.1.3
=====

* Cabal flag `no-linear` removes dependency on `linear` (and thus, transiently,
  `lens`). See `SDL.Vect` for details.
* Remove 'lens' dependency from all examples.
* Add Cabal flag `opengl-example` to separate that target from `examples`,
  because it is now the only example with an extra dependency (OpenGL).
* Make `hlint` happy with examples.
* Add `updateTexture` wrapper for native `SDL_UpdateTexture`.
* Expose `glGetDrawableSize` (can differ from window size in some environments).
* Correct `hintToString` output to match SDL hint tokens, rather than the names
  of the CPP macros defining them.
* Removed `ghc-options: -Wall` until we drop support for GHC 7.8. (>1300 warnings!)
* Various documentation updates.

2.1.2.1
=======

* Raise upper bounds for `lens` to <4.15 (affects examples only)

2.1.2
=====

* Revise some documentation examples to be more idomatic
* Update `.gitignore` to cover `stack` and other tools
* Raise upper bounds for `transformers` to <0.6
* Lower required SDL2 version to 2.0.2.
* Fix decoding of `TextEditingEvent` where the Raw bindings failed to stop reading
  character data past the null terminator.

2.1.1
=====

* `SDL.Input.Mouse` new has a new API for fetching the location of the mouse. This
  API gives you greater control over finding the mouse position with respect to the
  various "modes" a mouse can be in. The old API still exists, but will be removed
  at some point in the future.
* `SDL.Raw` now has a binding to `SDL_free`.

2.1.0
=====

* Introduce `initializeAll` and deprecate `InitEverything`. To fix this deprecation
  warning, change `initialize [InitEverything]` to `initializeAll`.
* `surfaceColorKey`, `surfaceFillRect` and `surfaceFillRects` now all operate on
  on RGBA `V4 Word8` values. They all implicitly map and unmap (using `SDL_MapRGBA`
  and `SDL_GetRGBA` respectively).
* `SDL.mapRGB` is now deprecated, as this conversion is always done for you.
  If you still need this routine, use `SDL.Raw.mapRGB`.
* Fix a runtime crash when reading the current BlendMode of a texture. Thanks to
  @seppeljordan for discovering and fixing this bug.

2.0.0
=====
* Introduce a set of comprehensive high-level bindings to SDL. These bindings
  should allow users to work with SDL while writing idiomatic Haskell code. They
  take care of pointer manipulation, and wrap up values in much more "natural"
  data types. The high-level bindings live in the `SDL` namespace, and have been
  extensively documented.
* Raw bindings have been moved from `Graphics.UI.SDL` to `SDL.Raw`.


1.3.1
=====
* Correct type signature of `getSurfaceBlendMode`

1.3.0
=====
* Use pattern synonyms exclusively
  * `Graphics.UI.SDL.Enum.Pattern` overrides `Graphics.UI.SDL.Enum`
* Generalize all IO functions over MonadIO
* Add convenience wrapper functions for constructing FunPtr callbacks
* Add Typeable instances to all type classes
* Add strictness annotations to all data structure fields
* Add missing `SDLK_AUDIOPREV` enumeration
* Correct deserialization of `SDL_TEXTINPUT` event
  * Data beyond the null terminator was returned previously

1.2.0
=====
* Add support for pattern synonyms as an alternative for SDL enumerations
  * Only present when compiling with GHC 7.8 or newer
* Add missing enumerations:
  * `keymodShift`, `keymodCtrl`, `keymodAlt`, `keymodGUI`
  * `keyPressend`, `keyReleased`
  * `toucheMouseID`
* Specialize init flags over `InitFlag`, a `Word32`
* Generalize `keymod*` enumerations over `Num`
  * The C API is inconsistent on their types
* Fix foreign imports on `Graphics.UI.SDL.Thread`
* Correct type signature of `getRenderDrawBlendMode`
* Correct type signature of `queryTexture`
* Remove export of `Keycode` from `Graphics.UI.SDL.Types`
  * `Graphics.UI.SDL.Enum` already exports `Keycode`

1.1.3
=====
* Add missing `Keycode` enumerations
* Add missing enumerations:
  * `audioAllowFrequencyChange`, `audioAllowFormatChange`
  * `audioAllowChannelsChange`, `audioAllowAnyChange`

1.1.2
=====
* Add `ClipboardUpdateEvent` to `Event` data structure
* Add `UnknownEvent` to `Event` data structure

1.1.1
=====
* Add `Graphics.UI.SDL.Platform` module
* Add `Graphics.UI.SDL.Thread` module and associated types and enumerations
* Add `getWindowWMInfo`
* Add `setError`
* Add additional logging functions

1.1.0
=====
* Require SDL 2.0.3
  * Add `gameControllerAddMappingsFromFile`
  * Add `gameControllerAddMappingsFromRW`
  * Add `glResetAttributes`
  * Add `mouseButtonEventClicks` field to `WindowEvent`
* Add missing mouse button enumerations

1.0.2
=====
* Correct type signature of `getNumTouchFingers`

1.0.1
=====
* Factor type of `addHintCallback` and `delHintCallback` into `HintCallback`
* Add `Version` data structure
