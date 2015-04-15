-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.HMatrix
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra.HMatrix (

    -- * Basic types and data processing
    module Numeric.LinearAlgebra.Data,

    -- * Arithmetic and numeric classes
    -- |
    -- The standard numeric classes are defined elementwise:
    --
    -- >>>  vector [1,2,3] * vector [3,0,-2]
    -- fromList [3.0,0.0,-6.0]
    --
    -- >>> matrix 3 [1..9] * ident 3
    -- (3><3)
    --  [ 1.0, 0.0, 0.0
    --  , 0.0, 5.0, 0.0
    --  , 0.0, 0.0, 9.0 ]
    --
    -- In arithmetic operations single-element vectors and matrices
    -- (created from numeric literals or using 'scalar') automatically
    -- expand to match the dimensions of the other operand:
    --
    -- >>> 5 + 2*ident 3 :: Matrix Double
    -- (3><3)
    --  [ 7.0, 5.0, 5.0
    --  , 5.0, 7.0, 5.0
    --  , 5.0, 5.0, 7.0 ]
    --
    -- >>> matrix 3 [1..9] + matrix 1 [10,20,30]
    -- (3><3)
    --  [ 11.0, 12.0, 13.0
    --  , 24.0, 25.0, 26.0
    --  , 37.0, 38.0, 39.0 ]
    --

    -- * Products
    -- ** dot
    dot, (<·>),
    -- ** matrix-vector
    app, (#>), (<#), (!#>),
    -- ** matrix-matrix
    mul, (<>),
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


    -- ** other
    outer, kronecker, cross,
    scale,
    sumElements, prodElements,

    -- * Linear Systems
    (<\>),
    linearSolve,
    linearSolveLS,
    linearSolveSVD,
    luSolve,
    cholSolve,
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

    -- * SVD
    svd,
    thinSVD,
    compactSVD,
    singularValues,
    leftSV, rightSV,

    -- * Eigensystems
    eig, eigSH, eigSH',
    eigenvalues, eigenvaluesSH, eigenvaluesSH',
    geigSH',

    -- * QR
    qr, rq, qrRaw, qrgr,

    -- * Cholesky
    chol, cholSH, mbCholSH,

    -- * Hessenberg
    hess,

    -- * Schur
    schur,

    -- * LU
    lu, luPacked,

    -- * Matrix functions
    expm,
    sqrtm,
    matFunc,

    -- * Correlation and convolution
    corr, conv, corrMin, corr2, conv2,

    -- * Random arrays

    Seed, RandDist(..), randomVector, rand, randn, gaussianSample, uniformSample,

    -- * Misc
    meanCov, rowOuters, peps, relativeError, haussholder, optimiseMult, udot, nullspaceSVD, orthSVD, ranksv,
    ℝ,ℂ,iC,
    -- * Auxiliary classes
    Element, Container, Product, Numeric, LSDiv,
    Complexable, RealElement,
    RealOf, ComplexOf, SingleOf, DoubleOf,
    IndexOf,
    Field,
--    Normed,
    Transposable,
    CGState(..),
    Testable(..)
) where

import Numeric.LinearAlgebra.Data

import Numeric.Matrix()
import Numeric.Vector()
import Data.Packed.Numeric hiding ((<>), mul)
import Numeric.LinearAlgebra.Algorithms hiding (linearSolve,Normed,orth)
import qualified Numeric.LinearAlgebra.Algorithms as A
import Numeric.LinearAlgebra.Util
import Numeric.LinearAlgebra.Random
import Numeric.Sparse((!#>))
import Numeric.LinearAlgebra.Util.CG

{- | infix synonym of 'mul'

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

-- | dense matrix product
mul :: Numeric t => Matrix t -> Matrix t -> Matrix t
mul = mXm


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

