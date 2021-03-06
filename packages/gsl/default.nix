{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

f = { mkDerivation, array, base, gsl, hmatrix, process, random, stdenv, vector }:

mkDerivation {
  pname = "hmatrix-gsl";
  version = "0.18.0.1";
  src = ./.;
  libraryHaskellDepends = [
    array base hmatrix process random vector
  ];
  librarySystemDepends = [ gsl ];
  libraryPkgconfigDepends = [ gsl ];
  homepage = "https://github.com/albertoruiz/hmatrix";
  description = "Numerical computation";
  license = "GPL";
};

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
