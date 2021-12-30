{ lib
, haskell-nix
, sources
, compiler-nix-name
, libsodium-vrf
}:
let
  # The haskell project created by haskell-nix.cabalProject'
  project = import ./haskell.nix {
    inherit lib haskell-nix compiler-nix-name libsodium-vrf;
  };

  # All the packages defined by our project, including dependencies
  packages = project.hsPkgs;

  # Just the packages in the project
  projectPackages = haskell-nix.haskellLib.selectProjectPackages packages;
in {
  inherit project projectPackages packages;
}
