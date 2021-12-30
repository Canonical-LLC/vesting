{ pkgs
, sources
, plutus
}:
let
  haskell = pkgs.callPackage ./haskell {
    inherit sources;
    inherit (plutus.plutus-apps.haskell) compiler-nix-name; # Use the same GHC version as plutus
  };
in {
  inherit haskell;

  inherit (plutus.plutus-apps) hlint cabal-install stylish-haskell haskell-language-server cardano-repo-tool;
}
