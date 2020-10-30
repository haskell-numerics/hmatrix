--------------------------------------------------------------------------------
{- |
Module      :  Numeric.HMatrix.Devel
Copyright   :  (c) Alberto Ruiz 2014
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

The library can be easily extended using the tools in this module.

-}
--------------------------------------------------------------------------------

module Numeric.LinearAlgebra.Devel(
    -- * FFI tools
    -- | See @examples/devel@ in the repository.

    createVector, createMatrix,
    TransArray(..),
    MatrixOrder(..), orderOf, cmat, fmat,
    matrixFromVector,
    unsafeFromForeignPtr,
    unsafeToForeignPtr,
    check, (//), (#|),
    at', atM', fi, ti,

    -- * ST
    -- | In-place manipulation inside the ST monad.
    -- See @examples/inplace.hs@ in the repository.

    -- ** Mutable Vectors
    STVector, newVector, thawVector, freezeVector, runSTVector,
    readVector, writeVector, modifyVector, liftSTVector,
    -- ** Mutable Matrices
    STMatrix, newMatrix, thawMatrix, freezeMatrix, runSTMatrix,
    readMatrix, writeMatrix, modifyMatrix, liftSTMatrix,
    mutable, extractMatrix, setMatrix, rowOper, RowOper(..), RowRange(..), ColRange(..), gemmm, Slice(..),
    -- ** Unsafe functions
    newUndefinedVector,
    unsafeReadVector, unsafeWriteVector,
    unsafeThawVector, unsafeFreezeVector,
    newUndefinedMatrix,
    unsafeReadMatrix, unsafeWriteMatrix,
    unsafeThawMatrix, unsafeFreezeMatrix,

    -- * Special maps and zips
    mapVectorWithIndex, zipVector, zipVectorWith, unzipVector, unzipVectorWith,
    mapVectorM, mapVectorM_, mapVectorWithIndexM, mapVectorWithIndexM_,
    foldLoop, foldVector, foldVectorG, foldVectorWithIndex,
    mapMatrixWithIndex, mapMatrixWithIndexM, mapMatrixWithIndexM_,
    liftMatrix, liftMatrix2, liftMatrix2Auto,

    -- * Sparse representation
    CSR(..), fromCSR, mkCSR, impureCSR,
    GMatrix(..),

    -- * Misc
    toByteString, fromByteString, showInternal, reorderVector

) where

import Internal.Devel
import Internal.ST
import Internal.Vector
import Internal.Matrix
import Internal.Element
import Internal.Sparse

