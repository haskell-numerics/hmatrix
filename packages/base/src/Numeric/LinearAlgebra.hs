{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-15
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional


-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra (

    -- * Basic types and data manipulation
    -- | This package works with 2D ('Matrix') and 1D ('Vector')
    -- arrays of real ('R') or complex ('C') double precision numbers.
    -- Single precision and machine integers are also supported for
    -- basic arithmetic and data manipulation.
    module Numeric.LinearAlgebra.Data,

    -- * Numeric classes
    -- |
    -- The standard numeric classes are defined elementwise (commonly referred to
    -- as the Hadamard product or the Schur product):
    --
    -- >>>  vector [1,2,3] * vector [3,0,-2]
    -- [3.0,0.0,-6.0]
    -- it :: Vector R
    --
    -- >>> matrix 3 [1..9] * ident 3
    -- (3><3)
    --  [ 1.0, 0.0, 0.0
    --  , 0.0, 5.0, 0.0
    --  , 0.0, 0.0, 9.0 ]

    -- * Autoconformable dimensions
    -- |
    -- In most operations, single-element vectors and matrices
    -- (created from numeric literals or using 'scalar'), and matrices
    -- with just one row or column, automatically
    -- expand to match the dimensions of the other operand:
    --
    -- >>> 5 + 2*ident 3 :: Matrix Double
    -- (3><3)
    --  [ 7.0, 5.0, 5.0
    --  , 5.0, 7.0, 5.0
    --  , 5.0, 5.0, 7.0 ]
    --
    -- >>> (4><3) [1..] + row [10,20,30]
    -- (4><3)
    --  [ 11.0, 22.0, 33.0
    --  , 14.0, 25.0, 36.0
    --  , 17.0, 28.0, 39.0
    --  , 20.0, 31.0, 42.0 ]
    --

    -- * Products
    -- ** Dot
    dot, (<.>),
    -- ** Matrix-vector
    (#>), (<#), (!#>),
    -- ** Matrix-matrix
    (<>),
    -- | The matrix product is also implemented in the "Data.Monoid" instance, where
    -- single-element matrices (created from numeric literals or using 'scalar')
    -- are used for scaling.
    --
    -- >>> import Data.Monoid as M
    -- >>>  let m = matrix 3 [1..6]
    -- >>> m M.<> 2 M.<> diagl[0.5,1,0]
    -- (2><3)
    --  [ 1.0,  4.0, 0.0
    --  , 4.0, 10.0, 0.0 ]
    --
    -- 'mconcat' uses 'optimiseMult' to get the optimal association order.


    -- ** Other
    outer, kronecker, cross,
    scale, add,
    sumElements, prodElements,

    -- * Linear systems
    -- ** General
    (<\>),
    linearSolveLS,
    linearSolveSVD,
    -- ** Determined
    linearSolve,
    luSolve, luPacked,
    luSolve', luPacked',
    -- ** Symmetric indefinite
    ldlSolve, ldlPacked,
    -- ** Positive definite
    cholSolve,
    -- ** Triangular
    UpLo(..),
    triSolve,
    -- ** Tridiagonal
    triDiagSolve,
    -- ** Sparse
    cgSolve,
    cgSolve',

    -- * Inverse and pseudoinverse
    inv, pinv, pinvTol,

    -- * Determinant and rank
    rcond, rank,
    det, invlndet,

    -- * Norms
    Normed(..),
    norm_Frob, norm_nuclear,

    -- * Nullspace and range
    orth,
    nullspace, null1, null1sym,

    -- * Singular value decomposition
    svd,
    thinSVD,
    compactSVD,
    compactSVDTol,
    singularValues,
    leftSV, rightSV,

    -- * Eigendecomposition
    eig, geig, eigSH,
    eigenvalues, geigenvalues, eigenvaluesSH,
    geigSH,

    -- * QR
    qr, thinQR, rq, thinRQ, qrRaw, qrgr,

    -- * Cholesky
    chol, mbChol,

    -- * LU
    lu, luFact,

    -- * Hessenberg
    hess,

    -- * Schur
    schur,

    -- * Matrix functions
    expm,
    sqrtm,
    matFunc,

    -- * Correlation and convolution
    corr, conv, corrMin, corr2, conv2,

    -- * Random arrays

    Seed, RandDist(..), randomVector, rand, randn, gaussianSample, uniformSample,

    -- * Misc
    meanCov, rowOuters, pairwiseD2, normalize, peps, relativeError, magnit,
    haussholder, optimiseMult, udot, nullspaceSVD, orthSVD, ranksv,
    iC, sym, mTm, trustSym, unSym,
    -- * Auxiliary classes
    Element, Container, Product, Numeric, LSDiv, Herm,
    Complexable, RealElement,
    RealOf, ComplexOf, SingleOf, DoubleOf,
    IndexOf,
    Field, Linear(), Additive(),
    Transposable,
    LU(..),
    LDL(..),
    QR(..),
    CGState(..),
    Testable(..)
) where

import Numeric.LinearAlgebra.Data

import Numeric.Matrix()
import Numeric.Vector()
import Internal.Matrix
import Internal.Container hiding ((<>))
import Internal.Numeric hiding (mul)
import Internal.Algorithms hiding (linearSolve,Normed,orth,luPacked',linearSolve',luSolve',ldlPacked')
import qualified Internal.Algorithms as A
import Internal.Util
import Internal.Random
import Internal.Sparse((!#>))
import Internal.CG
import Internal.Conversion
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

{- | dense matrix product

>>> let a = (3><5) [1..]
>>> a
(3><5)
 [  1.0,  2.0,  3.0,  4.0,  5.0
 ,  6.0,  7.0,  8.0,  9.0, 10.0
 , 11.0, 12.0, 13.0, 14.0, 15.0 ]

>>> let b = (5><2) [1,3, 0,2, -1,5, 7,7, 6,0]
>>> b
(5><2)
 [  1.0, 3.0
 ,  0.0, 2.0
 , -1.0, 5.0
 ,  7.0, 7.0
 ,  6.0, 0.0 ]

>>> a <> b
(3><2)
 [  56.0,  50.0
 , 121.0, 135.0
 , 186.0, 220.0 ]

-}
(<>) :: Numeric t => Matrix t -> Matrix t -> Matrix t
(<>) = mXm
infixr 8 <>


{- | Solve a linear system (for square coefficient matrix and several right-hand sides) using the LU decomposition, returning Nothing for a singular system. For underconstrained or overconstrained systems use 'linearSolveLS' or 'linearSolveSVD'.

@
a = (2><2)
 [ 1.0, 2.0
 , 3.0, 5.0 ]
@

@
b = (2><3)
 [  6.0, 1.0, 10.0
 , 15.0, 3.0, 26.0 ]
@

>>> linearSolve a b
Just (2><3)
 [ -1.4802973661668753e-15,     0.9999999999999997, 1.999999999999997
 ,       3.000000000000001, 1.6653345369377348e-16, 4.000000000000002 ]

>>> let Just x = it
>>> disp 5 x
2x3
-0.00000  1.00000  2.00000
 3.00000  0.00000  4.00000

>>> a <> x
(2><3)
 [  6.0, 1.0, 10.0
 , 15.0, 3.0, 26.0 ]

-}
linearSolve m b = A.mbLinearSolve m b

-- | return an orthonormal basis of the null space of a matrix. See also 'nullspaceSVD'.
nullspace m = nullspaceSVD (Left (1*eps)) m (rightSV m)

-- | return an orthonormal basis of the range space of a matrix. See also 'orthSVD'.
orth m = orthSVD (Left (1*eps)) m (leftSV m)

