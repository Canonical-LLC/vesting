{ system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system; }
}:
let
  # We're going to get everything from the main plutus repository. This ensures
  # we're using the same version of multiple dependencies such as nipxkgs,
  # haskell-nix, cabal-install, compiler-nix-name, etc.
  plutus = import sources.plutus-apps { inherit system; };

  pkgs = plutus.pkgs;

  haskell-nix = pkgs.haskell-nix;

  vesting = import ./pkgs {
    inherit pkgs haskell-nix sources plutus;
  };
in {
  inherit pkgs vesting plutus;
}
