let
  packages = import ./.;
  inherit (packages) pkgs vesting;
  inherit (vesting) haskell;

  cardano-cli-balance-fixer =
    let
      p = import (import ./nix/sources.nix { inherit pkgs; }).cardano-cli-balance-fixer;
    in
      p.cardano-cli-balance-fixer.components.exes.cardano-cli-balance-fixer;
in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with vesting; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      packages.plutus.plutus-apps.cardano-cli
      packages.plutus.plutus-apps.cardano-node
      cardano-cli-balance-fixer
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }
