0.14.1.0
--------

- In Numeric.LinearAlgebra.Util:
    convolution: corr, conv, corr2, conv2, separable, corrMin
    kronecker: vec, vech, dup, vtrans

0.14.0.0
--------

- integration over infinite intervals

- msadams and msbdf methods for ode

- Numeric.LinearAlgebra.Util

- (<\>) extended to multiple right-hand sides

- orth

0.13.0.0
--------

- tests moved to new package hmatrix-tests

0.11.2.0
--------

- geigSH' (symmetric generalized eigensystem)

- mapVectorWithIndex


0.11.1.0
--------

- exported Mul

- mapMatrixWithIndex{,M,M_}

0.11.0.0
--------

- flag -fvector default = True

- invlndet (inverse and log of determinant)

- step, cond

- find

- assoc, accum

0.10.0.0
--------

- Module reorganization

- Support for Float and Complex Float elements (excluding LAPACK computations)

- Binary instances for Vector and Matrix

- optimiseMult

- mapVectorM, mapVectorWithIndexM, unzipVectorWith, and related functions.

- diagRect admits diagonal vectors of any length without producing an error,
  and takes an additional argument for the off-diagonal elements.

- different signatures in some functions

0.9.3.0
--------

- flag -fvector to optionally use Data.Vector.Storable.Vector
  without any conversion.

- Simpler module structure.

- toBlocks, toBlocksEvery

- cholSolve, mbCholSH

- GSL Nonlinear Least-Squares fitting using Levenberg-Marquardt.

- GSL special functions moved to separate package hmatrix-special.

- Added offset of Vector, allowing fast, noncopy subVector (slice).
  Vector is now identical to Roman Leshchinskiy's Data.Vector.Storable.Vector,
  so we can convert from/to them in O(1).

- Removed Data.Packed.Convert, see examples/vector.hs

0.8.3.0
--------

- odeSolve

- Matrix arithmetic automatically replicates matrix with single row/column

- latexFormat, dispcf

0.8.2.0
--------

- fromRows/fromColumns now automatically expand vectors of dim 1
  to match the common dimension.
  fromBlocks also replicates single row/column matrices.
  Previously all dimensions had to be exactly the same.

- display utilities: dispf, disps, vecdisp

- scalar

- minimizeV, minimizeVD, using Vector instead of lists.

0.8.1.0
--------

- runBenchmarks

0.8.0.0
--------

- singularValues, fullSVD, thinSVD, compactSVD, leftSV, rightSV
  and complete interface to [d|z]gesdd.
  Algorithms based on the SVD of large matrices can now be
  significantly faster.

- eigenvalues, eigenvaluesSH

- linearSolveLS, rq

0.7.2.0
--------

- ranksv

0.7.1.0
--------

- buildVector/buildMatrix

- removed NFData instances

0.6.0.0
--------

- added randomVector, gaussianSample, uniformSample, meanCov

- added rankSVD, nullspaceSVD

- rank, nullspacePrec, and economy svd defined in terms of ranksvd.

- economy svd now admits zero rank matrices and return a "degenerate
  rank 1" decomposition with zero singular value.

- added NFData instances for Matrix and Vector.

- liftVector, liftVector2 replaced by mapVector, zipVector.

