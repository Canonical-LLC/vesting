############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, compiler-nix-name
, lib
, libsodium-vrf
}:

let
  project = haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "vesting";
      src = ../../../.;
    };

    inherit compiler-nix-name;

    modules = [
      {
        packages = {
          # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
          plutus-ledger.doHaddock = false;
          plutus-use-cases.doHaddock = false;

          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      }
    ];
  };
in
  project
