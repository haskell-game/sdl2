# Building

[![Build Status](https://travis-ci.org/haskell-game/sdl2.svg?branch=master)](https://travis-ci.org/haskell-game/sdl2)

If you don't have SDL 2.0.2 or higher on your system via your
package manager, you can install it from the
[official SDL site](https://www.libsdl.org/download-2.0.php).

On Ubuntu you can install from source with a simple

    ./configure && make -j4 && sudo make install

On OSX you can install SDL with [homebrew](http://brew.sh/). pkg-config is also recommended.

    brew install sdl2 pkg-config

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
