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
    module Data.Packed.Foreign,

    -- * FFI tools
    -- | Illustrative usage examples can be found
    --   in the @examples\/devel@ folder included in the package.
    module Data.Packed.Development,

    -- * ST
    -- | In-place manipulation inside the ST monad.
    -- See examples\/inplace.hs in the distribution.
    
    -- ** Mutable Vectors
    STVector, newVector, thawVector, freezeVector, runSTVector,
    readVector, writeVector, modifyVector, liftSTVector,
    -- ** Mutable Matrices
    STMatrix, newMatrix, thawMatrix, freezeMatrix, runSTMatrix,
    readMatrix, writeMatrix, modifyMatrix, liftSTMatrix,
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

    -- * Misc
    CSR(..), fromCSR, mkCSR,
    GMatrix(..)

) where

import Data.Packed.Foreign
import Data.Packed.Development
import Data.Packed.ST
import Data.Packed
import Numeric.Sparse

