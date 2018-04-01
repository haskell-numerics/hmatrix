{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Vector
-- Copyright   :  (c) Alberto Ruiz 2007-14
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-----------------------------------------------------------------------------

module Numeric.GSL.Vector (
    randomVector,
    saveMatrix,
    fwriteVector, freadVector, fprintfVector, fscanfVector
) where

import Numeric.LinearAlgebra.HMatrix hiding(randomVector, saveMatrix)
import Numeric.GSL.Internal hiding (TV,TM,TCV,TCM)

import Foreign.Marshal.Alloc(free)
import Foreign.Ptr(Ptr)
import Foreign.C.Types
import Foreign.C.String(newCString)
import System.IO.Unsafe(unsafePerformIO)

fromei x = fromIntegral (fromEnum x) :: CInt

-----------------------------------------------------------------------

-- | Obtains a vector of pseudorandom elements from the the mt19937 generator in GSL, with a given seed. Use randomIO to get a random seed.
randomVector :: Int      -- ^ seed
             -> RandDist -- ^ distribution
             -> Int      -- ^ vector size
             -> Vector Double
randomVector seed dist n = unsafePerformIO $ do
    r <- createVector n
    (r `applyRaw` id) (c_random_vector_GSL (fi seed) ((fi.fromEnum) dist)) #|"randomVectorGSL"
    return r

foreign import ccall unsafe "random_vector_GSL" c_random_vector_GSL :: CInt -> CInt -> TV

--------------------------------------------------------------------------------

-- | Saves a matrix as 2D ASCII table.
saveMatrix :: FilePath
           -> String     -- ^ format (%f, %g, %e)
           -> Matrix Double
           -> IO ()
saveMatrix filename fmt m = do
    charname <- newCString filename
    charfmt <- newCString fmt
    let o = if orderOf m == RowMajor then 1 else 0
    (m `applyRaw` id) (matrix_fprintf charname charfmt o) #|"matrix_fprintf"
    free charname
    free charfmt

foreign import ccall unsafe "matrix_fprintf" matrix_fprintf :: Ptr CChar -> Ptr CChar -> CInt -> TM

--------------------------------------------------------------------------------

-- | Loads a vector from an ASCII file (the number of elements must be known in advance).
fscanfVector :: FilePath -> Int -> IO (Vector Double)
fscanfVector filename n = do
    charname <- newCString filename
    res <- createVector n
    (res `applyRaw` id) (gsl_vector_fscanf charname) #|"gsl_vector_fscanf"
    free charname
    return res

foreign import ccall unsafe "vector_fscanf" gsl_vector_fscanf:: Ptr CChar -> TV

-- | Saves the elements of a vector, with a given format (%f, %e, %g), to an ASCII file.
fprintfVector :: FilePath -> String -> Vector Double -> IO ()
fprintfVector filename fmt v = do
    charname <- newCString filename
    charfmt <- newCString fmt
    (v `applyRaw` id) (gsl_vector_fprintf charname charfmt) #|"gsl_vector_fprintf"
    free charname
    free charfmt

foreign import ccall unsafe "vector_fprintf" gsl_vector_fprintf :: Ptr CChar -> Ptr CChar -> TV

-- | Loads a vector from a binary file (the number of elements must be known in advance).
freadVector :: FilePath -> Int -> IO (Vector Double)
freadVector filename n = do
    charname <- newCString filename
    res <- createVector n
    (res `applyRaw` id) (gsl_vector_fread charname) #|"gsl_vector_fread"
    free charname
    return res

foreign import ccall unsafe "vector_fread" gsl_vector_fread:: Ptr CChar -> TV

-- | Saves the elements of a vector to a binary file.
fwriteVector :: FilePath -> Vector Double -> IO ()
fwriteVector filename v = do
    charname <- newCString filename
    (v `applyRaw` id) (gsl_vector_fwrite charname) #|"gsl_vector_fwrite"
    free charname

foreign import ccall unsafe "vector_fwrite" gsl_vector_fwrite :: Ptr CChar -> TV

type PD = Ptr Double                            --
type TV = CInt -> PD -> IO CInt                 --
type TM = CInt -> CInt -> PD -> IO CInt         --

