{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.LinearAlgebra
-- Copyright   :  (c) Alberto Ruiz 2007-14
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-----------------------------------------------------------------------------

module Numeric.GSL.LinearAlgebra (
    RandDist(..), randomVector,
    saveMatrix,
    fwriteVector, freadVector, fprintfVector, fscanfVector,
    fileDimensions, loadMatrix, fromFile
) where

import Numeric.LinearAlgebra.HMatrix hiding (RandDist,randomVector,saveMatrix,loadMatrix)
import Numeric.GSL.Internal hiding (TV,TM,TCV,TCM)

import Foreign.Marshal.Alloc(free)
import Foreign.Ptr(Ptr)
import Foreign.C.Types
import Foreign.C.String(newCString)
import System.IO.Unsafe(unsafePerformIO)
import System.Process(readProcess)

fromei x = fromIntegral (fromEnum x) :: CInt

-----------------------------------------------------------------------

data RandDist = Uniform  -- ^ uniform distribution in [0,1)
              | Gaussian -- ^ normal distribution with mean zero and standard deviation one
              deriving Enum

-- | Obtains a vector of pseudorandom elements from the the mt19937 generator in GSL, with a given seed. Use randomIO to get a random seed.
randomVector :: Int      -- ^ seed
             -> RandDist -- ^ distribution
             -> Int      -- ^ vector size
             -> Vector Double
randomVector seed dist n = unsafePerformIO $ do
    r <- createVector n
    (r `applyRaw` id) (c_random_vector (fi seed) ((fi.fromEnum) dist))  #|"randomVector"
    return r

foreign import ccall unsafe "random_vector" c_random_vector :: CInt -> CInt -> TV

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
    (m `applyRaw` id) (matrix_fprintf charname charfmt o)  #|"matrix_fprintf"
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
    (res `applyRaw` id) (gsl_vector_fread charname) #| "gsl_vector_fread"
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

--------------------------------------------------------------------------------

{- |  obtains the number of rows and columns in an ASCII data file
      (provisionally using unix's wc).
-}
fileDimensions :: FilePath -> IO (Int,Int)
fileDimensions fname = do
    wcres <- readProcess "wc" ["-w",fname] ""
    contents <- readFile fname
    let tot = read . head . words $ wcres
        c   = length . head . dropWhile null . map words . lines $ contents
    if tot > 0
        then return (tot `div` c, c)
        else return (0,0)

-- | Loads a matrix from an ASCII file formatted as a 2D table.
loadMatrix :: FilePath -> IO (Matrix Double)
loadMatrix file = fromFile file =<< fileDimensions file

-- | Loads a matrix from an ASCII file (the number of rows and columns must be known in advance).
fromFile :: FilePath -> (Int,Int) -> IO (Matrix Double)
fromFile filename (r,c) = reshape c `fmap` fscanfVector filename (r*c)

