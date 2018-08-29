{ mkDerivation, base, bytestring, lens, linear, SDL2, StateVar, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "sdl2";
  version = "2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring lens linear StateVar text transformers vector
  ];
  description = "Both high- and low-level bindings to the SDL library (version 2.0.6).";
  license = stdenv.lib.licenses.bsd3;
  librarySystemDepends = [ SDL2 ];
  libraryPkgconfigDepends = [ SDL2 ];
}
