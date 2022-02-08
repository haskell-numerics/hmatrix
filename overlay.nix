final: prev: with final; {

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = lib.composeManyExtensions [
                  (old.overrides or (_: _: {}))
                  (haskell.lib.packageSourceOverrides {
                    hmatrix = ./packages/base;
                    hmatrix-glpk = ./packages/glpk;
                    hmatrix-gsl = ./packages/gsl;
                    hmatrix-sparse = ./packages/sparse;
                    hmatrix-special = ./packages/special;
                    hmatrix-tests = ./packages/tests;
                  })
                ];
  });

}
