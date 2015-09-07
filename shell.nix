let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sdl2 = self.callPackage ./. {};
    };
  };
in haskellPackages.sdl2.env
