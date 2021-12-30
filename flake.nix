{ description = "A Plutus vesting smart contract";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    plutus-apps = {
      url = "github:Canonical-LLC/plutus-apps/ghc8.10.7";
      flake = false;
    };

    cardano-cli-balance-fixer = {
      url = "github:Canonical-LLC/cardano-cli-balance-fixer";
      flake = false;
    };
  };

  outputs = { self, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        packages = import ./nix {
          inherit system;
          sources = inputs;
        };
        inherit (packages.vesting.haskell) project;
      in project.flake {} // {
        defaultPackage = project.vesting.components.exes.vesting-sc;
      }
    );
}
