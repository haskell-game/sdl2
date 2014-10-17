let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      sdl2 = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.sdl2.name;
     buildInputs = [
       pkgs.curl
       pkgs.pkgconfig
       pkgs.SDL2
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.sdl2.propagatedNativeBuildInputs)))
     ];
   }