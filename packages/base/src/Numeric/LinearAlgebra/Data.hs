--------------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Data
Copyright   :  (c) Alberto Ruiz 2014
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Basic data processing.

-}
--------------------------------------------------------------------------------

module Numeric.LinearAlgebra.Data(

    -- * Vector
    -- | 1D arrays are storable vectors from the vector package.
    
    vect, (|>),

    -- * Matrix
    
    mat, (><), tr,
    
    -- * Indexing
    
    size,
    Indexable(..),
    
    -- * Construction
    scalar, Konst(..), Build(..), assoc, accum, linspace, -- ones, zeros,

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

    -- * Sparse
    AssocMatrix, toDense,
    mkSparse, mkDiagR, mkDense,
    
    -- * IO
    disp,
    loadMatrix, saveMatrix,
    latexFormat,
    dispf, disps, dispcf, format,

-- * Conversion
    Convert(..),
    
    -- * Misc
    arctan2,
    rows, cols,
    separable,

    module Data.Complex,

    Vector, Matrix, GMatrix, nRows, nCols

) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.Numeric
import Numeric.LinearAlgebra.Util hiding ((&),(#))
import Data.Complex
import Numeric.Sparse


