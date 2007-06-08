{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LAPACK.Internal
-- Copyright   :  (c) Alberto Ruiz 2006-7
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Wrappers for a few LAPACK functions (<http://www.netlib.org/lapack>).
--
-----------------------------------------------------------------------------

module LAPACK.Internal where

import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Complex
import Foreign
import Foreign.C.Types
import Foreign.C.String

-----------------------------------------------------------------------------
-- dgesvd
foreign import ccall "lapack-aux.h svd_l_R"
    dgesvd :: Double ::> Double ::> (Double :> Double ::> IO Int)

-- | Wrapper for LAPACK's /dgesvd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=svdR m@ so that @m=u \<\> s \<\> 'trans' v@.
svdR :: Matrix Double -> (Matrix Double, Matrix Double , Matrix Double)
svdR x@M {rows = r, cols = c} = (u, diagRect s r c, v) where (u,s,v) = svdR' x

svdR' x@M {rows = r, cols = c} = unsafePerformIO $ do
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    dgesvd // mat fdat x // mat dat u // vec s // mat dat v // check "svdR" [fdat x]
    return (u,s,trans v)

-----------------------------------------------------------------------------
-- dgesdd
foreign import ccall "lapack-aux.h svd_l_Rdd"
    dgesdd :: Double ::> Double ::> (Double :> Double ::> IO Int)

-----------------------------------------------------------------------------
-- zgesvd
foreign import ccall "lapack-aux.h svd_l_C"
    zgesvd :: (Complex Double) ::> (Complex Double) ::> (Double :> (Complex Double) ::> IO Int)

-- | Wrapper for LAPACK's /zgesvd/, which computes the full svd decomposition of a complex matrix.
--
-- @(u,s,v)=svdC m@ so that @m=u \<\> s \<\> 'trans' v@.
svdC :: Matrix (Complex Double)
     -> (Matrix (Complex Double), Matrix Double, Matrix (Complex Double))
svdC x@M {rows = r, cols = c} = (u, diagRect s r c, v) where (u,s,v) = svdC' x

svdC' x@M {rows = r, cols = c} = unsafePerformIO $ do
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    zgesvd // mat fdat x // mat dat u // vec s // mat dat v // check "svdC" [fdat x]
    return (u,s,trans v)

-----------------------------------------------------------------------------
-- zgeev
foreign import ccall "lapack-aux.h eig_l_C"
    zgeev :: (Complex Double) ::> (Complex Double) ::> ((Complex Double) :> (Complex Double) ::> IO Int)

-- | Wrapper for LAPACK's /zgeev/, which computes the eigenvalues and right eigenvectors of a general complex matrix:
--
-- if @(l,v)=eigC m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigC :: Matrix (Complex Double) -> (Vector (Complex Double), Matrix (Complex Double))
eigC (m@M {rows = r}) = unsafePerformIO $ do
    l <- createVector r
    v <- createMatrix ColumnMajor r r
    dummy <- createMatrix ColumnMajor 1 1
    zgeev // mat fdat m // mat dat dummy // vec l // mat dat v // check "eigC" [fdat m]
    return (l,v)

-----------------------------------------------------------------------------
-- dgeev
foreign import ccall "lapack-aux.h eig_l_R"
    dgeev :: Double ::> Double ::> ((Complex Double) :> Double ::> IO Int)

-----------------------------------------------------------------------------
-- dsyev
foreign import ccall "lapack-aux.h eig_l_S"
    dsyev :: Double ::> (Double :> Double ::> IO Int)

-- | Wrapper for LAPACK's /dsyev/, which computes the eigenvalues and right eigenvectors of a symmetric real matrix:
--
-- if @(l,v)=eigSl m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are sorted in descending order (use eigS' for ascending order).
eigS :: Matrix Double -> (Vector Double, Matrix Double)
eigS m = (s', fliprl v)
    where (s,v) = eigS' m
          s' = fromList . reverse . toList $  s

eigS' (m@M {rows = r}) = unsafePerformIO $ do
    l <- createVector r
    v <- createMatrix ColumnMajor r r
    dsyev // mat fdat m // vec l // mat dat v // check "eigS" [fdat m]
    return (l,v)

-----------------------------------------------------------------------------
-- zheev
foreign import ccall "lapack-aux.h eig_l_H"
    zheev :: (Complex Double) ::> (Double :> (Complex Double) ::> IO Int)

-----------------------------------------------------------------------------
-- dgesv
foreign import ccall "lapack-aux.h linearSolveR_l"
    dgesv :: Double ::> Double ::> Double ::> IO Int

-----------------------------------------------------------------------------
-- zgesv
foreign import ccall "lapack-aux.h linearSolveC_l"
    zgesv :: (Complex Double) ::> (Complex Double) ::> (Complex Double) ::> IO Int

-----------------------------------------------------------------------------------
-- dgels
foreign import ccall "lapack-aux.h linearSolveLSR_l"
    dgels :: Double ::> Double ::> Double ::> IO Int

-- | Wrapper for LAPACK's /dgels/, which obtains the least squared error solution of an overconstrained real linear system or the minimum norm solution of an underdetermined system, for several right-hand sides. For rank deficient systems use 'linearSolveSVDR'.
linearSolveLSR :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveLSR a b = subMatrix (0,0) (cols a, cols b) $ linearSolveLSR_l a b

linearSolveLSR_l a@(M {rows = m, cols = n}) b@(M {cols = nrhs}) = unsafePerformIO $ do
    r <- createMatrix ColumnMajor (max m n) nrhs
    dgels // mat fdat a // mat fdat b // mat dat r // check "linearSolveLSR" [fdat a, fdat b]
    return r

-----------------------------------------------------------------------------------
-- zgels
foreign import ccall "lapack-aux.h linearSolveLSC_l"
    zgels :: (Complex Double) ::> (Complex Double) ::> (Complex Double) ::> IO Int

-----------------------------------------------------------------------------------
-- dgelss
foreign import ccall "lapack-aux.h linearSolveSVDR_l"
    dgelss :: Double -> Double ::> Double ::> Double ::> IO Int

-----------------------------------------------------------------------------------
-- zgelss
foreign import ccall "lapack-aux.h linearSolveSVDC_l"
    zgelss :: Double -> (Complex Double) ::> (Complex Double) ::> (Complex Double) ::> IO Int
