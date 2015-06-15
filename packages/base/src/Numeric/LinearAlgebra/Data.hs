{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Data
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Basic data processing.

-}
--------------------------------------------------------------------------------

module Numeric.LinearAlgebra.Data(

    -- * Vector
    -- | 1D arrays are storable vectors from the vector package. There is no distinction
    --   between row and column vectors.

    fromList, toList, (|>), vector, range, idxs,

    -- * Matrix

    (><), matrix, tr, tr',

    -- * Dimensions

    size, rows, cols,

    -- * Conversion from\/to lists

    fromLists, toLists,
    row, col,

    -- * Conversions vector\/matrix

    flatten, reshape, asRow, asColumn,
    fromRows, toRows, fromColumns, toColumns,

    -- * Indexing

    atIndex,
    Indexable(..),

    -- * Construction
    scalar, Konst(..), Build(..), assoc, accum, linspace,  -- ones, zeros,

    -- * Diagonal
    ident, diag, diagl, diagRect, takeDiag,

    -- * Vector extraction
    subVector, takesV, vjoin,

    -- * Matrix extraction
    Extractor(..), (??),

    takeRows, dropRows, takeColumns, dropColumns,
    subMatrix, (?), (¿), fliprl, flipud,

    remap,

    -- * Block matrix
    fromBlocks, (|||), (===), diagBlock, repmat, toBlocks, toBlocksEvery,

    -- * Mapping functions
    conj, cmap, cmod,

    step, cond,

    -- * Find elements
    find, maxIndex, minIndex, maxElement, minElement,
    sortVector, sortIndex,

    -- * Sparse
    AssocMatrix, toDense,
    mkSparse, mkDiagR, mkDense,

    -- * IO
    disp,
    loadMatrix, loadMatrix', saveMatrix,
    latexFormat,
    dispf, disps, dispcf, format,
    dispDots, dispBlanks, dispShort,
-- * Element conversion
    Convert(..),
    roundVector,
    fromInt,toInt,fromZ,toZ,
    -- * Misc
    arctan2,
    separable,
    fromArray2D,
    module Data.Complex,
    R,C,I,Z,Mod, type(./.),
    Vector, Matrix, GMatrix, nRows, nCols

) where

import Internal.Vector
import Internal.Vectorized
import Internal.Matrix hiding (size)
import Internal.Element
import Internal.IO
import Internal.Numeric
import Internal.Container
import Internal.Util hiding ((&),(#))
import Data.Complex
import Internal.Sparse
import Internal.Modular


