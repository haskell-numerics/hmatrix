-----------------------------------------------------------------------------
{- |
Module      :  Numeric.HMatrix
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  GPL

Maintainer  :  Alberto Ruiz
Stability   :  provisional

This module reexports the most common Linear Algebra functions.

-}
-----------------------------------------------------------------------------
module Numeric.HMatrix (

    -- * Basic types and data processing    
    module Numeric.HMatrix.Data,
    
    -- | The standard numeric classes are defined elementwise.
    --
    -- >>> fromList [1,2,3] * fromList [3,0,-2 :: Double]
    -- fromList [3.0,0.0,-6.0]
    -- 
    -- In arithmetic operations single-element vectors and matrices automatically
    -- expand to match the dimensions of the other operand.
    -- 
    -- >>> 2 * ident 3
    -- 2 * ident 3 :: Matrix Double
    -- (3><3)
    -- [ 2.0, 0.0, 0.0
    -- , 0.0, 2.0, 0.0
    -- , 0.0, 0.0, 2.0 ]
    --

    -- * Products
    (<>), (·), outer, kronecker, cross,
    optimiseMult, scale,
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
    qr, rq,

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
    norm1, norm2, normInf,

    -- * Correlation and Convolution
    corr, conv, corrMin, corr2, conv2,

    -- * Random arrays
    rand, randn, RandDist(..), randomVector, gaussianSample, uniformSample,
    
    -- * Misc
    meanCov, peps, relativeError, haussholder
) where

import Numeric.HMatrix.Data

import Numeric.Matrix()
import Numeric.Vector()
import Numeric.Container
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Util


