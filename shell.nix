# From Mathieu's PR

# {ghc}:

# with import <nixpkgs> {};

# let
#   gfortranlibdir = "${gfortran7.cc.lib}";
#   # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
#   libHack = if stdenv.isDarwin then {
#       DYLD_LIBRARY_PATH = [gfortranlibdir];
#     } else {
#       LD_LIBRARY_PATH = [gfortranlibdir];
#     };
# in
# haskell.lib.buildStackProject ({
#   name = "haskell-sundials";
#   buildInputs = [blas liblapack sundials zlib];
#   inherit ghc;
#   # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
#   extraArgs = ["--extra-lib-dirs=${gfortranlibdir}"];
# } // libHack)


# Dominic's original version

# { nixpkgs ? import <nixpkgs>
#   {}
# , compiler ? "ghc822"
# , doBenchmark ? false }:

# let

#   inherit (nixpkgs) pkgs;

# f = { mkDerivation, ad, base, diagrams-lib, diagrams-rasterific, gcc
#     , hmatrix, hmatrix-gsl, HUnit, inline-c, plots, pretty, QuickCheck, stdenv, sundials, vector }:

# mkDerivation {
#   pname = "haskell-sundials";
#   version = "0.1.0.0";
#   src = ./.;
#   isLibrary = false;
#   isExecutable = true;
#   executableHaskellDepends = [
#     ad
#     base
#     diagrams-lib
#     diagrams-rasterific
#     gcc
#     hmatrix
#     hmatrix-gsl
#     HUnit
#     inline-c
#     plots
#     QuickCheck
#     pretty
#     vector
#   ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa] else []);
#   executableSystemDepends = [
#     pkgs.blas
#     pkgs.gfortran.cc
#     pkgs.liblapack
#     pkgs.gsl
#     pkgs.glpk
#     pkgs.pkgconfig
#     pkgs.stack
#     sundials
#     pkgs.zlib
#   ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Accelerate] else []);
#   license = stdenv.lib.licenses.bsd3;
# };

# haskellPackages = if compiler == "default"
#   then pkgs.haskellPackages
#   else pkgs.haskell.packages.${compiler};

# variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

# drv = variant (haskellPackages.callPackage f {});

# in

#   if pkgs.lib.inNixShell then drv.env else drv

let
  pkgs = import ./nixpkgs.nix {};

  # List of dependencies, these are provided as
  #  - extra-lib-dirs to stack for linking
  #  - buildInputs so that the headers are available during compilation
  extraLibs =
    [
      pkgs.blas
      pkgs.gfortran7.cc
      pkgs.glpk
      pkgs.gsl
      pkgs.liblapack
      pkgs.sundials
      pkgs.zlib # the library
      pkgs.zlib.dev # the headers
    ];

  # Wrapped stack executable that uses the nix-provided GHC
  stack = pkgs.stdenv.mkDerivation {
      name = "stack-system-ghc";
      builder = pkgs.writeScript "stack" ''
        source $stdenv/setup
        mkdir -p $out/bin
        makeWrapper ${pkgs.stack}/bin/stack $out/bin/stack \
          --add-flags "--extra-lib-dirs=${pkgs.lib.makeLibraryPath extraLibs}"
      '';
      buildInputs = [ pkgs.makeWrapper ];
    };
  ghc = pkgs.haskell.compiler.ghc822;
  ldLibraryPath = if pkgs.stdenv.isDarwin then "DYLD_LIBRARY_PATH" else "LD_LIBRARY_PATH";

in pkgs.mkShell
  { buildInputs = [ stack ghc ] ++ extraLibs;

    # Cabal only accepts gsl through pkg-config
    PKG_CONFIG_PATH="${pkgs.gsl}/lib/pkgconfig";

    # For reasons that are beyond me stack fails to pass in zlib as an extra
    # lib
    ${ldLibraryPath} = "${pkgs.zlib}/lib";

    # For some reasons that are beyond me stack fails to pass in liblapack as a
    # static lib to the linker
    LIBRARY_PATH="${pkgs.liblapack}/lib";
  }
