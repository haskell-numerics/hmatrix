{ghc}:

with import <nixpkgs> {};

let
  gfortranlibdir = "${gfortran7.cc.lib}";
  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  libHack = if stdenv.isDarwin then {
      DYLD_LIBRARY_PATH = [gfortranlibdir];
    } else {
      LD_LIBRARY_PATH = [gfortranlibdir];
    };
in
haskell.lib.buildStackProject ({
  name = "haskell-sundials";
  buildInputs = [blas liblapack sundials zlib];
  inherit ghc;
  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  extraArgs = ["--extra-lib-dirs=${gfortranlibdir}"];
} // libHack)
