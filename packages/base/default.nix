{ mkDerivation, array, base, binary, bytestring, darwin, deepseq
, openblasCompat, primitive, random, semigroups, split, stdenv
, storable-complex, vector
}:
mkDerivation {
  pname = "hmatrix";
  version = "0.20.2";
  src = ./.;
  configureFlags = [ "-fdisable-default-paths" "-fopenblas" ];
  libraryHaskellDepends = [
    array base binary bytestring deepseq primitive random semigroups
    split storable-complex vector
  ];
  buildDepends = [ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.Accelerate] else []) ];
  librarySystemDepends = [ openblasCompat ];
  homepage = "https://github.com/haskell-numerics/hmatrix";
  description = "Numeric Linear Algebra";
  license = stdenv.lib.licenses.bsd3;
}
