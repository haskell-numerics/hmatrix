--------------------------------------------------------------------------------
{- |
Module      :  Numeric.HMatrix.Data
Copyright   :  (c) Alberto Ruiz 2014
License     :  GPL

Maintainer  :  Alberto Ruiz
Stability   :  provisional

Basic data processing.

-}
--------------------------------------------------------------------------------

module Numeric.HMatrix.Data(

    -- * Vector
    -- | 1D arrays are storable vectors from the vector package.
    
    Vector, (|>), dim, (@>),
    
    -- * Matrix
    Matrix, (><), size, (@@>), trans, ctrans,
    
    -- * Construction
    scalar, konst, build, assoc, accum, linspace, -- ones, zeros,

    -- * Diagonal
    ident, diag, diagl, diagRect, takeDiag,

    -- * Data manipulation
    fromList, toList, subVector, takesV, vjoin,
    flatten, reshape, asRow, asColumn, row, col,
    fromRows, toRows, fromColumns, toColumns, fromLists, toLists, fromArray2D,
    takeRows, dropRows, takeColumns, dropColumns, subMatrix, (?), (¿), fliprl, flipud,
  
    -- * Block matrix
    fromBlocks, (¦), (——), diagBlock, repmat, toBlocks, toBlocksEvery,

    -- * Mapping functions
    conj, cmap, step, cond,
    
    -- * Find elements
    find, maxIndex, minIndex, maxElement, minElement, atIndex,

    -- * IO
    disp, dispf, disps, dispcf, vecdisp, latexFormat, format,
    loadMatrix, saveMatrix, fromFile, fileDimensions,
    readMatrix,
    fscanfVector, fprintfVector, freadVector, fwriteVector,

-- * Conversion
    Convert(..),
    
    -- * Misc
    arctan2,
    rows, cols,
    separable,

    module Data.Complex

) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.Container
import Numeric.LinearAlgebra.Util
import Data.Complex

