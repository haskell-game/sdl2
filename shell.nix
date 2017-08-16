let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sdl2 = self.callPackage ./. { SDL2 = pkgs.SDL2.dev; };
    };
  };
in haskellPackages.sdl2.env
