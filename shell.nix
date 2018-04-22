{ nixpkgs ? import <nixpkgs> {} }:
let p = nixpkgs; in

  let
  myStack = p.writeShellScriptBin "stack" ''
    exec ${p.stack}/bin/stack --no-nix --no-docker --system-ghc $STACK_IN_NIX_EXTRA_ARGS "$@"
  '';
in
p.haskell.lib.buildStackProject {
  name = "hmatrix";

  buildInputs = [
    p.zlib
    p.sundials
    p.blas
    p.gfortran.cc
    p.liblapack
    p.gsl
    p.glpk
    p.pkgconfig
    p.stack
    p.sundials
    myStack
  ] ++ (if p.stdenv.isDarwin then [p.darwin.apple_sdk.frameworks.Accelerate] else [])
    ++ (if p.stdenv.isDarwin then [p.darwin.apple_sdk.frameworks.Cocoa] else []);
}
