{ pkgs }: with pkgs; let

  ghcCharged =  haskellPackages.ghcWithPackages (p: with p; [
                  haskell-language-server
                  ghcid
                ]);
  ghcid-bin = haskellPackages.ghcid.bin;

  ghcid-bin-with-openblas = let
    ghcid = "${ghcid-bin}/bin/ghcid";
    out = "$out/bin/ghcid";
  in runCommand "ghcid" { buildInputs = [ makeWrapper ]; } ''
    makeWrapper ${ghcid} ${out} --add-flags \
      "--command='cd packages/base && \
                    cabal repl lib:hmatrix \
                    --flags=openblas \
                    --extra-lib-dirs=${openblasCompat}/lib \
                    --extra-include-dir=${openblasCompat}/include \
                 '"
  '';

in mkShell {
  buildInputs =  haskellPackages.hmatrix.env.nativeBuildInputs ++
                 [ ghcCharged
                   ghcid-bin-with-openblas
                   cabal-install
                   openblasCompat
                 ];
}
