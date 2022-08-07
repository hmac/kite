let
  name = "kite";
  compiler = "ghc902";

  sources = import ./nix/sources.nix { };
  nixpkgs = import sources.nixpkgs { };
  gitignore = nixpkgs.nix-gitignore.gitignoreSource [ ];

  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "${name}" = self.callCabal2nix "${name}" (gitignore ./.) { };
    };
  };

  shell = haskellPackages.shellFor {
    packages = p: [ p."${name}" ];
    buildInputs = with nixpkgs.pkgs.haskell.packages.${compiler}; [
      haskellPackages.cabal-install
      ghcid
      # Use the standard brittany derivation because it requires GHC > 9.0
      nixpkgs.haskellPackages.brittany
      haskell-language-server
      hlint
      nixpkgs.nixpkgs-fmt
      nixpkgs.chez
      # The experimental evaluator is written in Rust
      nixpkgs.rustc
      nixpkgs.cargo
      nixpkgs.rls
      nixpkgs.clippy
      nixpkgs.rustfmt
    ];
    withHoogle = true;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (haskellPackages."${name}");

in
{
  inherit shell;
  inherit exe;
  inherit haskellPackages;
  "${name}" = haskellPackages."${name}";
}
