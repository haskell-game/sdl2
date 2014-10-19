with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
      overrides = self: super: {
        sdl2 = self.callPackage ./. {};
        foreign-var = self.callPackage ../foreign-var {};
      };
    };
in modifiedHaskellPackages.sdl2.env
