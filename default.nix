{ mkDerivation, base, bytestring, lens, linear, SDL2, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "sdl2";
  version = "2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base bytestring lens linear text transformers vector
  ];
  extraLibraries = [ SDL2 ];
  pkgconfigDepends = [ SDL2 ];
  description = "Both high- and low-level bindings to the SDL library (version 2.0.3).";
  license = stdenv.lib.licenses.bsd3;
}
