{ nixpkgs ? import <nixpkgs> {} }:

let
  pkgs = nixpkgs.pkgs.haskell.packages.ghc883;
  pkgsWithHoogle = pkgs.override {
    overrides = (self: super:
      {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      }
    );
  };
in
  pkgsWithHoogle.callPackage ./lam.nix {}
