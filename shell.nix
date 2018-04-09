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
    pkgs.liblapack
    patched-gsl
    sundials
    pkgs.zlib
  ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Accelerate] else []);
  license = stdenv.lib.licenses.bsd3;
};

haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

patched-hmatrix = pkgs.haskellPackages.hmatrix.overrideAttrs (oldAttrs: rec {
  src = nixpkgs.fetchgit {
    url = git://github.com/albertoruiz/hmatrix;
    rev = "d83b17190029c11e3ab8b504e5cdc917f5863120";
    sha256 = "11wr59wg21rky59j3kkd3ba6aqns9gkh0r1fnhwhn3fp7zfhanqn";
  };
  postUnpack = ''
    sourceRoot=''${sourceRoot}/packages/base
    echo Source root reset to ''${sourceRoot}
  '';
});

patched-hmatrix-gsl = pkgs.haskellPackages.hmatrix-gsl.overrideAttrs (oldAttrs: rec {
  src = nixpkgs.fetchgit {
    url = git://github.com/albertoruiz/hmatrix;
    rev = "d83b17190029c11e3ab8b504e5cdc917f5863120";
    sha256 = "11wr59wg21rky59j3kkd3ba6aqns9gkh0r1fnhwhn3fp7zfhanqn";
  };
  postUnpack = ''
    sourceRoot=''${sourceRoot}/packages/gsl
    echo Source root reset to ''${sourceRoot}
  '';
});

patched-gsl = pkgs.gsl.overrideAttrs (oldAttrs: rec {
  src = nixpkgs.fetchgit {
    url = git://github.com/idontgetoutmuch/gsl;
    rev = "c2035977d65cd804169ff3370da6723cf879be75";
    sha256 = "1fqp77gp9nl3av1z58cwg8fivik4rff394wgjzc76ayd04y0d1k7";
    };
  version = "2.5";
  name = "gsl-${version}";
  doCheck = false;
  CFLAGS = "-DDEBUG";
});

drv = variant (haskellPackages.callPackage f {
  hmatrix = patched-hmatrix;
  hmatrix-gsl = patched-hmatrix-gsl;
});

in

  if pkgs.lib.inNixShell then drv.env else drv
