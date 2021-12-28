let
  packages = import ./nix;
in {
  inherit (packages) pkgs vesting plutus;

  inherit (packages.vesting.haskell) project;
}
