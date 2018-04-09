{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

f = { mkDerivation, base, containers, diagrams-lib, diagrams-rasterific, gcc
    , hmatrix, hmatrix-gsl, HUnit, inline-c, plots, stdenv
    , template-haskell, temporary, vector }:

mkDerivation {
  pname = "hmatrix-sundials";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    hmatrix
    inline-c
    template-haskell
    vector
  ];
  executableHaskellDepends = [
    base
    diagrams-lib
    diagrams-rasterific
    gcc
    hmatrix
    hmatrix-gsl
    HUnit
    inline-c
    template-haskell
    temporary
    vector
  ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Cocoa] else []);
  executableSystemDepends = [
    pkgs.blas
    pkgs.gfortran.cc
    pkgs.liblapack
    pkgs.gsl
    pkgs.glpk
    pkgs.pkgconfig
    pkgs.stack
    pkgs.sundials
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

