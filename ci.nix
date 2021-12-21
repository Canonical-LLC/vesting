let
  inherit (import ./.) project;
in
  project.vesting.components.exes.vesting-sc
