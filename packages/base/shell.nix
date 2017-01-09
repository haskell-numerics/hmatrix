{ nixpkgs ? import ./nixpkgs.nix
, ghc ? nixpkgs.ghc
  # ^ The `stack --nix` command will pass in a specific ghc derivation.
}:

with nixpkgs ;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "hmatrix";
  buildInputs =
    [ stack
      cabal-install  # Tentatively support cabal new-build
      which          # To issue 'which' in build script
      nix            # So that `stack --nix` can find `nix-shell`
    ] ++
    (if stdenv.isLinux
     then [
       liblapack
       blas
       gfortran.cc  # For libfortran
     ]
     else [
       darwin.apple_sdk.frameworks.Accelerate
     ]
    ) ;
  LANG = "en_US.UTF-8" ;  # Without this, tests fail!
}
