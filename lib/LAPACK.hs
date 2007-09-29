{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LAPACK
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

module LAPACK (
    svdR, svdRdd, svdC,
    eigC, eigR, eigS, eigH,
    linearSolveR, linearSolveC,
    linearSolveLSR, linearSolveLSC,
    linearSolveSVDR, linearSolveSVDC,
    cholS, cholH,
    qrR, qrC
) where

import Data.Packed.Internal
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Data.Packed.Vector
import Data.Packed.Matrix
import GSL.Vector(vectorMapValR, FunCodeSV(Scale))
import Complex
import Foreign

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h svd_l_R" dgesvd :: TMMVM

-- | Wrapper for LAPACK's /dgesvd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full svdR m@ so that @m=u \<\> s \<\> 'trans' v@.
svdR :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svdR x = unsafePerformIO $ do
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    dgesvd // mat fdat x // mat dat u // vec s // mat dat v // check "svdR" [fdat x]
    return (u,s,trans v)
  where r = rows x
        c = cols x
-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h svd_l_Rdd" dgesdd :: TMMVM

-- | Wrapper for LAPACK's /dgesvd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full svdRdd m@ so that @m=u \<\> s \<\> 'trans' v@.
svdRdd :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svdRdd x = unsafePerformIO $ do
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    dgesdd // mat fdat x // mat dat u // vec s // mat dat v // check "svdRdd" [fdat x]
    return (u,s,trans v)
  where r = rows x
        c = cols x

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h svd_l_C" zgesvd :: TCMCMVCM

-- | Wrapper for LAPACK's /zgesvd/, which computes the full svd decomposition of a complex matrix.
--
-- @(u,s,v)=full svdC m@ so that @m=u \<\> comp s \<\> 'trans' v@.
svdC :: Matrix (Complex Double) -> (Matrix (Complex Double), Vector Double, Matrix (Complex Double))
svdC x = unsafePerformIO $ do
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    zgesvd // mat fdat x // mat dat u // vec s // mat dat v // check "svdC" [fdat x]
    return (u,s,trans v)
  where r = rows x
        c = cols x


-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h eig_l_C" zgeev :: TCMCMCVCM

-- | Wrapper for LAPACK's /zgeev/, which computes the eigenvalues and right eigenvectors of a general complex matrix:
--
-- if @(l,v)=eigC m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigC :: Matrix (Complex Double) -> (Vector (Complex Double), Matrix (Complex Double))
eigC m
    | r == 1 = (fromList [cdat m `at` 0], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        dummy <- createMatrix ColumnMajor 1 1
        zgeev // mat fdat m // mat dat dummy // vec l // mat dat v // check "eigC" [fdat m]
        return (l,v)
  where r = rows m

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h eig_l_R" dgeev :: TMMCVM

-- | Wrapper for LAPACK's /dgeev/, which computes the eigenvalues and right eigenvectors of a general real matrix:
--
-- if @(l,v)=eigR m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigR :: Matrix Double -> (Vector (Complex Double), Matrix (Complex Double))
eigR m = (s', v'')
    where (s,v) = eigRaux m
          s' = toComplex (subVector 0 r (asReal s), subVector r r (asReal s))
          v' = toRows $ trans v
          v'' = fromColumns $ fixeig (toList s') v'
          r = rows m

eigRaux :: Matrix Double -> (Vector (Complex Double), Matrix Double)
eigRaux m
    | r == 1 = (fromList [(cdat m `at` 0):+0], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        dummy <- createMatrix ColumnMajor 1 1
        dgeev // mat fdat m // mat dat dummy // vec l // mat dat v // check "eigR" [fdat m]
        return (l,v)
  where r = rows m

fixeig  []  _ =  []
fixeig [r] [v] = [comp v]
fixeig ((r1:+i1):(r2:+i2):r) (v1:v2:vs)
    | r1 == r2 && i1 == (-i2) = toComplex (v1,v2) : toComplex (v1,scale (-1) v2) : fixeig r vs
    | otherwise = comp v1 : fixeig ((r2:+i2):r) (v2:vs)
  where scale = vectorMapValR Scale

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h eig_l_S" dsyev :: TMVM

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

eigS' m
    | r == 1 = (fromList [cdat m `at` 0], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        dsyev // mat fdat m // vec l // mat dat v // check "eigS" [fdat m]
        return (l,v)
  where r = rows m

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h eig_l_H" zheev :: TCMVCM

-- | Wrapper for LAPACK's /zheev/, which computes the eigenvalues and right eigenvectors of a hermitian complex matrix:
--
-- if @(l,v)=eigH m@ then @m \<\> s v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are sorted in descending order.
eigH :: Matrix (Complex Double) -> (Vector Double, Matrix (Complex Double))
eigH m = (s', fliprl v)
    where (s,v) = eigH' m
          s' = fromList . reverse . toList $  s

eigH' m
    | r == 1 = (fromList [realPart (cdat m `at` 0)], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        zheev // mat fdat m // vec l // mat dat v // check "eigH" [fdat m]
        return (l,v)
  where r = rows m

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveR_l" dgesv :: TMMM

-- | Wrapper for LAPACK's /dgesv/, which solves a general real linear system (for several right-hand sides) internally using the lu decomposition.
linearSolveR :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveR  a b
    | n1==n2 && n1==r = unsafePerformIO $ do
        s <- createMatrix ColumnMajor r c
        dgesv // mat fdat a // mat fdat b // mat dat s // check "linearSolveR" [fdat a, fdat b]
        return s
    | otherwise = error "linearSolveR of nonsquare matrix"
  where n1 = rows a
        n2 = cols a
        r  = rows b
        c  = cols b

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveC_l" zgesv :: TCMCMCM

-- | Wrapper for LAPACK's /zgesv/, which solves a general complex linear system (for several right-hand sides) internally using the lu decomposition.
linearSolveC :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
linearSolveC  a b
    | n1==n2 && n1==r = unsafePerformIO $ do
        s <- createMatrix ColumnMajor r c
        zgesv // mat fdat a // mat fdat b // mat dat s // check "linearSolveC" [fdat a, fdat b]
        return s
    | otherwise = error "linearSolveC of nonsquare matrix"
  where n1 = rows a
        n2 = cols a
        r  = rows b
        c  = cols b

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveLSR_l" dgels :: TMMM

-- | Wrapper for LAPACK's /dgels/, which obtains the least squared error solution of an overconstrained real linear system or the minimum norm solution of an underdetermined system, for several right-hand sides. For rank deficient systems use 'linearSolveSVDR'.
linearSolveLSR :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveLSR a b = subMatrix (0,0) (cols a, cols b) $ linearSolveLSR_l a b

linearSolveLSR_l a b = unsafePerformIO $ do
    r <- createMatrix ColumnMajor (max m n) nrhs
    dgels // mat fdat a // mat fdat b // mat dat r // check "linearSolveLSR" [fdat a, fdat b]
    return r
  where m = rows a
        n = cols a
        nrhs = cols b

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveLSC_l" zgels :: TCMCMCM

-- | Wrapper for LAPACK's /zgels/, which obtains the least squared error solution of an overconstrained complex linear system or the minimum norm solution of an underdetermined system, for several right-hand sides. For rank deficient systems use 'linearSolveSVDC'.
linearSolveLSC :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
linearSolveLSC a b = subMatrix (0,0) (cols a, cols b) $ linearSolveLSC_l a b

linearSolveLSC_l a b = unsafePerformIO $ do
    r <- createMatrix ColumnMajor (max m n) nrhs
    zgels // mat fdat a // mat fdat b // mat dat r // check "linearSolveLSC" [fdat a, fdat b]
    return r
  where m = rows a
        n = cols a
        nrhs = cols b

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveSVDR_l" dgelss :: Double -> TMMM

-- | Wrapper for LAPACK's /dgelss/, which obtains the minimum norm solution to a real linear least squares problem Ax=B using the svd, for several right-hand sides. Admits rank deficient systems but it is slower than 'linearSolveLSR'. The effective rank of A is determined by treating as zero those singular valures which are less than rcond times the largest singular value. If rcond == Nothing machine precision is used.
linearSolveSVDR :: Maybe Double   -- ^ rcond
                -> Matrix Double  -- ^ coefficient matrix
                -> Matrix Double  -- ^ right hand sides (as columns)
                -> Matrix Double  -- ^ solution vectors (as columns)
linearSolveSVDR (Just rcond) a b = subMatrix (0,0) (cols a, cols b) $ linearSolveSVDR_l rcond a b
linearSolveSVDR Nothing a b = linearSolveSVDR (Just (-1)) a b

linearSolveSVDR_l rcond a b = unsafePerformIO $ do
    r <- createMatrix ColumnMajor (max m n) nrhs
    dgelss rcond // mat fdat a // mat fdat b // mat dat r // check "linearSolveSVDR" [fdat a, fdat b]
    return r
  where m = rows a
        n = cols a
        nrhs = cols b

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveSVDC_l" zgelss :: Double -> TCMCMCM

-- | Wrapper for LAPACK's /zgelss/, which obtains the minimum norm solution to a complex linear least squares problem Ax=B using the svd, for several right-hand sides. Admits rank deficient systems but it is slower than 'linearSolveLSC'. The effective rank of A is determined by treating as zero those singular valures which are less than rcond times the largest singular value. If rcond == Nothing machine precision is used.
linearSolveSVDC :: Maybe Double            -- ^ rcond
                -> Matrix (Complex Double) -- ^ coefficient matrix
                -> Matrix (Complex Double) -- ^ right hand sides (as columns)
                -> Matrix (Complex Double) -- ^ solution vectors (as columns)
linearSolveSVDC (Just rcond) a b = subMatrix (0,0) (cols a, cols b) $ linearSolveSVDC_l rcond a b
linearSolveSVDC Nothing a b = linearSolveSVDC (Just (-1)) a b

linearSolveSVDC_l rcond  a b = unsafePerformIO $ do
    r <- createMatrix ColumnMajor (max m n) nrhs
    zgelss rcond // mat fdat a // mat fdat b // mat dat r // check "linearSolveSVDC" [fdat a, fdat b]
    return r
  where m = rows a
        n = cols a
        nrhs = cols b

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h chol_l_H" zpotrf :: TCMCM

-- | Wrapper for LAPACK's /zpotrf/,which computes the Cholesky factorization of a
-- complex Hermitian positive definite matrix.
cholH :: Matrix (Complex Double) -> Matrix (Complex Double)
cholH a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor n n
    zpotrf // mat fdat a // mat dat r // check "cholH" [fdat a]
    return r
  where n = rows a

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h chol_l_S" dpotrf :: TMM

-- | Wrapper for LAPACK's /dpotrf/,which computes the Cholesky factorization of a
-- real symmetric positive definite matrix.
cholS :: Matrix Double -> Matrix Double
cholS a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor n n
    dpotrf // mat fdat a // mat dat r // check "cholS" [fdat a]
    return r
  where n = rows a

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h qr_l_R" dgeqr2 :: TMVM

-- | Wrapper for LAPACK's /dgeqr2/,which computes a QR factorization of a real matrix.
qrR :: Matrix Double -> (Matrix Double, Vector Double)
qrR a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor m n
    tau <- createVector mn
    dgeqr2 // mat fdat a // vec tau // mat dat r // check "qrR" [fdat a]
    return (r,tau)
  where m = rows a
        n = cols a
        mn = min m n

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h qr_l_C" zgeqr2 :: TCMCVCM

-- | Wrapper for LAPACK's /zgeqr2/,which computes a QR factorization of a complex matrix.
qrC :: Matrix (Complex Double) -> (Matrix (Complex Double), Vector (Complex Double))
qrC a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor m n
    tau <- createVector mn
    zgeqr2 // mat fdat a // vec tau // mat dat r // check "qrC" [fdat a]
    return (r,tau)
  where m = rows a
        n = cols a
        mn = min m n

