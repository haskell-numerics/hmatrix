{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Data
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

This module provides functions for creation and manipulation of vectors and matrices, IO, and other utilities.

-}
--------------------------------------------------------------------------------

module Numeric.LinearAlgebra.Data(

    -- * Elements
    R,C,I,Z,type(./.),

    -- * Vector
    {- | 1D arrays are storable vectors directly reexported from the vector package.
    -}

    fromList, toList, (|>), vector, range, idxs,

    -- * Matrix

    {- | The main data type of hmatrix is a 2D dense array defined on top of
         a storable vector. The internal representation is suitable for direct
         interface with standard numeric libraries.
    -}

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

    (?), (¿), fliprl, flipud,

    subMatrix, takeRows, dropRows, takeColumns, dropColumns,

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
    Mod,
    Vector, Matrix, GMatrix, nRows, nCols

) where

import Internal.Vector
import Internal.Vectorized
import Internal.Matrix hiding (size)
import Internal.Element
import Internal.IO
import Internal.Numeric
import Internal.Container
import Internal.Util hiding ((&))
import Data.Complex
import Internal.Sparse
import Internal.Modular


