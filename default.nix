let
  name = "kite";
  compiler = "ghc8107";

  sources = import ./nix/sources.nix { };
  nixpkgs = import sources.nixpkgs { };
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

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
      brittany
      hlint
      nixpkgs.nixpkgs-fmt
      nixpkgs.chez
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
