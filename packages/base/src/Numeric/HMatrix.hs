-----------------------------------------------------------------------------
{- |
Module      :  Numeric.HMatrix
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------
module Numeric.HMatrix (

    -- * Basic types and data processing    
    module Numeric.LinearAlgebra.Data,
    
    -- * Arithmetic and numeric classes
    -- |
    -- The standard numeric classes are defined elementwise:
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

    -- * Matrix product
    (<.>),

    -- | The overloaded multiplication operators may need type annotations to remove
    -- ambiguity. In those cases we can also use the specific functions 'mXm', 'mXv', and 'dot'.
    --
    -- The matrix x matrix product is also implemented in the "Data.Monoid" instance, where
    -- single-element matrices (created from numeric literals or using 'scalar')
    -- are used for scaling.
    --
    -- >>> let m = (2><3)[1..] :: Matrix Double
    -- >>> m <> 2 <> diagl[0.5,1,0]
    -- (2><3)
    -- [ 1.0,  4.0, 0.0
    -- , 4.0, 10.0, 0.0 ]
    --
    -- 'mconcat' uses 'optimiseMult' to get the optimal association order.

     
    -- * Other products
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
    norm_0, norm_1, norm_2, norm_Inf,
    mnorm_0, mnorm_1, mnorm_2, mnorm_Inf,
    norm_Frob, norm_nuclear,

    -- * Correlation and convolution
    corr, conv, corrMin, corr2, conv2,

    -- * Random arrays

    RandDist(..), randomVector, rand, randn, gaussianSample, uniformSample,
    
    -- * Misc
    meanCov, peps, relativeError, haussholder, optimiseMult, dot, udot, mXm, mXv, smXv, (<>), (◇), Seed, checkT,
    -- * Auxiliary classes
    Element, Container, Product, Contraction, LSDiv,
    Complexable, RealElement,
    RealOf, ComplexOf, SingleOf, DoubleOf,
    IndexOf,
    Field,
    Normed,
    CGMat, Transposable,
    ℕ,ℤ,ℝ,ℂ,ℝn,ℂn, 𝑖, i_C --ℍ
) where

import Numeric.LinearAlgebra.Data

import Numeric.Matrix()
import Numeric.Vector()
import Data.Packed.Numeric
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Util
import Numeric.LinearAlgebra.Random
import Numeric.Sparse(smXv)
import Numeric.LinearAlgebra.Util.CG


