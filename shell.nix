{ nixpkgs ? import <nixpkgs> {} }:

let inherit (nixpkgs) pkgs;
    inherit (pkgs) haskellPackages;
    project = import ./default.nix { nixpkgs = nixpkgs; };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.brittany
 ];
}
