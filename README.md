This package provides Haskell bindings for the SDL2 library.

# What is SDL2?

SDL (Simple DirectMedia Layer) is a library for cross-platform development of interactive applications.
SDL provides routines for managing windows, rendering graphics, processing sound, collecting input data, and much more.

The Haskell sdl2 library provides both a high- and low-level API to interface with SDL.

You may also want to check out:

- [sdl2-image](https://hackage.haskell.org/package/sdl2-image) - For handling different image formats such as `jpg` and `png`.
- [sdl2-mixer](https://hackage.haskell.org/package/sdl2-mixer) - For playing audio.
- [sdl2-gfx](https://hackage.haskell.org/package/sdl2-gfx) - For drawing graphics primitives such as circles and polygons.
- [sdl2-ttf](https://hackage.haskell.org/package/sdl2-ttf) - For handling true type fonts.


# Building

[![Build Status](https://travis-ci.org/haskell-game/sdl2.svg?branch=master)](https://travis-ci.org/haskell-game/sdl2)

If you don't have SDL 2.0.6 or higher on your system via your
package manager, you can install it from the
[official SDL site](https://www.libsdl.org/download-2.0.php).

On Ubuntu you can install from source with a simple

    ./configure && make -j4 && sudo make install

On OSX you can install SDL with [homebrew](http://brew.sh/). pkg-config is also recommended.

    brew install sdl2 pkg-config

On Windows you can install SDL with `pacman` under [MSYS2](https://msys2.github.io/) (or use  [stack's embedded MSYS2](https://www.reddit.com/r/haskellgamedev/comments/4jpthu/windows_sdl2_is_now_almost_painless_via_stack/)).

    pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2

# Get Started

Take a look at the [getting started guide](https://hackage.haskell.org/package/sdl2/docs/SDL.html).

# Contributing

We need your help! The SDL API is fairly large, and the more hands we have, the
quicker we can reach full coverage and release this to Hackage. There are a few
ways you can help:

1. Browse http://wiki.libsdl.org/CategoryAPI and find functions that aren't
   exposed in the high-level bindings.

2. The above can be somewhat laborious - an easier way to find out what's
   missing is to write code.

   * http://www.willusher.io/pages/sdl2/ is a collection of tutorials for C++.
   * http://lazyfoo.net/tutorials/SDL/index.php is another collection of C++
     tutorials.

   Both of these would be useful if they were translated to Haskell, and we'd be
   happy to store this code in this repository.

3. Documentation is welcome, but may not be the best use of your time as we are
   currently in a period of rapid development as we find the most productive
   API.

# Development

## Using `cabal repl`

You can use `cabal repl` as a development tool, but you'll need to configure the project in a slightly non-standard way first:

```
cabal configure --ghc-option=-fPIC
```

You only need to do this once (unless you reconfigure). From this point, `cabal repl` should Just Work.
