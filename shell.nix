let
  packages = import ./.;
  inherit (packages) pkgs vesting cardano-node;
  inherit (vesting) haskell;

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
      cardano-node.cardano-cli
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }
