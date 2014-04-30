--------------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Data
Copyright   :  (c) Alberto Ruiz 2014
License     :  GPL

Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
--------------------------------------------------------------------------------

module Numeric.LinearAlgebra.Data(
    -- * Vector
    -- | 1D arrays are storable vectors from the vector package.
    
    Vector, (|>), dim, (@>),
    
    -- * Matrix
    Matrix, (><), size, (@@>), trans, ctrans,
    
    -- * Construction functions
   
    scalar, konst, build, assoc, accum, linspace, -- ones, zeros,

    -- * Data manipulation
    
    fromList, toList, subVector, takesV, vjoin,
         
    flatten, reshape, asRow, asColumn, row, col,

    fromRows, toRows, fromColumns, toColumns, fromLists, toLists,
    
    takeRows, dropRows, takeColumns, dropColumns, subMatrix, (?), (¿), fliprl, flipud,
    
    -- * Diagonal matrices
    
    ident, diag, diagl, diagRect, takeDiag,
   
    -- * Block matrices

    fromBlocks, (¦), (——), diagBlock, repmat, toBlocks, toBlocksEvery,

    -- * Mapping functions
   
    conj, cmap, step, cond,
    
    -- * Find elements
    
    find, maxIndex, minIndex, maxElement, minElement, atIndex,

    -- * Products
    
    (<>), (·), outer, kronecker, cross,
    sumElements, prodElements, absSum,
    optimiseMult,
    
    corr, conv, corrMin, corr2, conv2,
    
    (<\>),
    
    -- * Random arrays

    rand, randn, RandDist(..), randomVector, gaussianSample, uniformSample,
    
    -- * IO
    
    disp, dispf, disps, dispcf, vecdisp, latexFormat, format,
    loadMatrix, saveMatrix, fromFile, fileDimensions,
    readMatrix,
    fscanfVector, fprintfVector, freadVector, fwriteVector,

-- * Element conversion
    Convert(..),
    Complexable(),
    RealElement(),

    RealOf, ComplexOf, SingleOf, DoubleOf,

    IndexOf,
    
    module Data.Complex,

    -- * Misc
    scale, meanCov, arctan2,
    rows, cols,
    separable,
    fromArray2D

) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.Container
import Numeric.LinearAlgebra.Util
import Data.Complex

