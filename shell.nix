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
  

{ nixpkgs ? import <nixpkgs>
  {}
, compiler ? "ghc822"
, doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

f = { mkDerivation, ad, base, diagrams-lib, diagrams-rasterific, gcc
    , hmatrix, hmatrix-gsl, HUnit, inline-c, plots, pretty, QuickCheck, stdenv, sundials, vector }:

mkDerivation {
  pname = "haskell-sundials";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ad
    base
    diagrams-lib
    diagrams-rasterific
    gcc
    hmatrix
    hmatrix-gsl
    HUnit
    inline-c
    plots
    QuickCheck
    pretty
    vector
  ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa] else []);
  executableSystemDepends = [
    pkgs.blas
    pkgs.gfortran.cc
    pkgs.liblapack
    pkgs.gsl
    pkgs.glpk
    pkgs.pkgconfig
    sundials
    pkgs.zlib
  ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Accelerate] else []);
  license = stdenv.lib.licenses.bsd3;
};

haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
