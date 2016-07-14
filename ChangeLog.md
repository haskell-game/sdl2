2.1.2.2
=======
* Minor documentation updates

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
