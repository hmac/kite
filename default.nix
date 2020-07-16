let
  name = "lam";
  compiler = "ghc883";

  sources = import ./nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "${name}" = self.callCabal2nix "${name}" (gitignore ./.) {};
      # hspec-hedgehog = self.callHackage "hspec-hedgehog" "0.0.1.2" {};
    };
  };

  shell = haskellPackages.shellFor {
    packages = p: [p."${name}"];
    buildInputs = with nixpkgs.haskellPackages; [
      haskellPackages.cabal-install
      ghcid
      brittany
      hlint
      nixpkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (haskellPackages."${name}");

in {
  inherit shell;
  inherit exe;
  inherit haskellPackages;
  "${name}" = haskellPackages."${name}";
}
