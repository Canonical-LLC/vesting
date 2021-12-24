let
  inherit (import ./.) project;
in
  project.vesting.components.exes.create-sc
