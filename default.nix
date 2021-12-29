{ system ? builtins.currentSystem
, sources ? import ./nix/sources.nix { inherit system; }
}:
let
  packages = import ./nix { inherit system sources; };
in {
  inherit (packages) pkgs vesting plutus;

  inherit (packages.vesting.haskell) project;
}
