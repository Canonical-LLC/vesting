let
  packages = import ./nix;
in {
  inherit (packages) pkgs vesting plutus cardano-node;

  inherit (packages.vesting.haskell) project;
}
