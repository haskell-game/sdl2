{ cabal, linear, vector, StateVar, text, SDL2 }:
cabal.mkDerivation (self: {
  pname = "sdl2";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ linear vector StateVar text ];
  extraLibraries = [ SDL2 ];
  pkgconfigDepends = [ SDL2 ];
})
