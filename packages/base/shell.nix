{ nixpkgs ? import ./nixpkgs.nix
  # ^ Use the specific nixpkgs commit specified in ./nixpkgs.nix.
, ghc ? nixpkgs.ghc
  # ^ The `stack --nix` command will pass in a specific ghc derivation.
}:

with nixpkgs;

let leapyearCabal =
      fetchgit {
        url = https://github.com/LeapYear/cabal.git;
        rev = "7226221e957d5d05a72afdb9c9caba476b140857";
        sha256 = "04sz0zgijg9353bvsy8hmrv042jqpixdvllkb042wkwy7h132rxp";
      };
    Cabal =
      pkgs.lib.overrideDerivation haskellPackages.Cabal_1_24_1_0 (drv: {
        src = "${leapyearCabal}/Cabal";
      });
    ghc = pkgs.lib.overrideDerivation
            (haskellPackages.ghcWithPackages (p: [Cabal]))
            (drv: { postBuild = ''
                     ${drv.postBuild}
                     $out/bin/ghc-pkg unregister Cabal-1.24.0.0
                    ''; });
in haskell.lib.buildStackProject {
  inherit ghc;
  name = "hmatrix";
  buildInputs =
    [ stack
      # cabal-install  # Tentatively support cabal new-build
      which            # To issue 'which' in build script
      nix              # So that `stack --nix` can find `nix-shell`
      (if stdenv.isLinux
         then openblas
         else darwin.apple_sdk.frameworks.Accelerate)
    ];
  LANG = "en_US.UTF-8";  # Without this, tests fail!
}
