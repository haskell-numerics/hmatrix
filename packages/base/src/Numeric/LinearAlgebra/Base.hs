-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra.Base (

    -- * Basic types and data processing    
    module Numeric.LinearAlgebra.Data,
    
    -- | The standard numeric classes are defined elementwise:
    --
    -- >>> fromList [1,2,3] * fromList [3,0,-2 :: Double]
    -- fromList [3.0,0.0,-6.0]
    -- 
    -- >>> (3><3) [1..9] * ident 3 :: Matrix Double
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

    -- * Products
    (<.>),
    
    -- | The matrix product is also implemented in the "Data.Monoid" instance for Matrix, where
    -- single-element matrices (created from numeric literals or using 'scalar')
    -- are used for scaling.
    --
    -- >>> let m = (2><3)[1..] :: Matrix Double
    -- >>> m <> 2 <> diagl[0.5,1,0]
    -- (2><3)
    -- [ 1.0,  4.0, 0.0
    -- , 4.0, 10.0, 0.0 ]
    --
    -- mconcat uses 'optimiseMult' to get the optimal association order.
 
    (◇),
    outer, kronecker, cross,
    scale,
    sumElements, prodElements, absSum,
    
    -- * Linear Systems
    (<\>),
    linearSolve,
    linearSolveLS,
    linearSolveSVD,
    luSolve,
    cholSolve,
    
    -- * Inverse and pseudoinverse
    inv, pinv, pinvTol,

    -- * Determinant and rank
    rcond, rank, ranksv, 
    det, invlndet,
    
    -- * Singular value decomposition
    svd,
    fullSVD,
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

    -- * Nullspace
    nullspacePrec,
    nullVector,
    nullspaceSVD,
    null1, null1sym,
    
    orth,

    -- * Norms
    norm1, norm2, normInf, pnorm, NormType(..),

    -- * Correlation and Convolution
    corr, conv, corrMin, corr2, conv2,

    -- * Random arrays

    -- |    rand, randn, RandDist(..), randomVector, gaussianSample, uniformSample
    
    -- * Misc
    meanCov, peps, relativeError, haussholder, optimiseMult, udot
) where

import Numeric.LinearAlgebra.Data

import Numeric.Matrix()
import Numeric.Vector()
import Numeric.Container
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Util



