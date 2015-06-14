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
    -- * FFI helpers
    -- | Sample usage, to upload a perspective matrix to a shader.
    --
    -- @ glUniformMatrix4fv 0 1 (fromIntegral gl_TRUE) \`appMatrix\` perspective 0.01 100 (pi\/2) (4\/3)
    -- @
    module Internal.Foreign,

    -- * FFI tools
    -- | See @examples/devel@ in the repository.
    
    createVector, createMatrix,
    vec, mat, omat,
    app1, app2, app3, app4,
    app5, app6, app7, app8, app9, app10,
    MatrixOrder(..), orderOf, cmat, fmat,
    matrixFromVector,
    unsafeFromForeignPtr,
    unsafeToForeignPtr,
    check, (//),
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
    mutable, extractMatrix, setMatrix, rowOper, RowOper(..), RowRange(..), ColRange(..),
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
    CSR(..), fromCSR, mkCSR,
    GMatrix(..),

    -- * Misc
    toByteString, fromByteString

) where

import Internal.Foreign
import Internal.Devel
import Internal.ST
import Internal.Vector
import Internal.Matrix
import Internal.Element
import Internal.Sparse

