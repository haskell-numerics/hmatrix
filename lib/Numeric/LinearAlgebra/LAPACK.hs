{-# OPTIONS_GHC #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LinearAlgebra.LAPACK
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

module Numeric.LinearAlgebra.LAPACK (
    svdR, svdRdd, svdC,
    eigC, eigR, eigS, eigH, eigS', eigH',
    linearSolveR, linearSolveC,
    linearSolveLSR, linearSolveLSC,
    linearSolveSVDR, linearSolveSVDC,
    luR, luC, lusR, lusC,
    cholS, cholH,
    qrR, qrC,
    hessR, hessC,
    schurR, schurC
) where

import Data.Packed.Internal
import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.GSL.Vector(vectorMapValR, FunCodeSV(Scale))
import Complex
import Foreign
import Foreign.C.Types (CInt)

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h svd_l_R" dgesvd :: TMMVM
foreign import ccall "LAPACK/lapack-aux.h svd_l_C" zgesvd :: TCMCMVCM
foreign import ccall "LAPACK/lapack-aux.h svd_l_Rdd" dgesdd :: TMMVM

-- | Wrapper for LAPACK's /dgesvd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full svdR m@ so that @m=u \<\> s \<\> 'trans' v@.
svdR :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svdR = svdAux dgesvd "svdR" . fmat

-- | Wrapper for LAPACK's /dgesvd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full svdRdd m@ so that @m=u \<\> s \<\> 'trans' v@.
svdRdd :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svdRdd = svdAux dgesdd "svdRdd" . fmat

-- | Wrapper for LAPACK's /zgesvd/, which computes the full svd decomposition of a complex matrix.
--
-- @(u,s,v)=full svdC m@ so that @m=u \<\> comp s \<\> 'trans' v@.
svdC :: Matrix (Complex Double) -> (Matrix (Complex Double), Vector Double, Matrix (Complex Double))
svdC = svdAux zgesvd "svdC" . fmat

svdAux f st x = unsafePerformIO $ do
    u <- createMatrix ColumnMajor r r
    s <- createVector (min r c)
    v <- createMatrix ColumnMajor c c
    app4 f mat x mat u vec s mat v st
    return (u,s,trans v)
  where r = rows x
        c = cols x

-----------------------------------------------------------------------------
eigAux f st m
    | r == 1 = (fromList [flatten m `at` 0], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        dummy <- createMatrix ColumnMajor 1 1
        app4 f mat m mat dummy vec l mat v st
        return (l,v)
  where r = rows m


foreign import ccall "LAPACK/lapack-aux.h eig_l_C" zgeev :: TCMCMCVCM
foreign import ccall "LAPACK/lapack-aux.h eig_l_R" dgeev :: TMMCVM
foreign import ccall "LAPACK/lapack-aux.h eig_l_S" dsyev :: TMVM
foreign import ccall "LAPACK/lapack-aux.h eig_l_H" zheev :: TCMVCM

-- | Wrapper for LAPACK's /zgeev/, which computes the eigenvalues and right eigenvectors of a general complex matrix:
--
-- if @(l,v)=eigC m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigC :: Matrix (Complex Double) -> (Vector (Complex Double), Matrix (Complex Double))
eigC = eigAux zgeev "eigC" . fmat

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /dgeev/, which computes the eigenvalues and right eigenvectors of a general real matrix:
--
-- if @(l,v)=eigR m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigR :: Matrix Double -> (Vector (Complex Double), Matrix (Complex Double))
eigR m = (s', v'')
    where (s,v) = eigRaux (fmat m)
          s' = toComplex (subVector 0 r (asReal s), subVector r r (asReal s))
          v' = toRows $ trans v
          v'' = fromColumns $ fixeig (toList s') v'
          r = rows m

eigRaux :: Matrix Double -> (Vector (Complex Double), Matrix Double)
eigRaux m
    | r == 1 = (fromList [(flatten m `at` 0):+0], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        dummy <- createMatrix ColumnMajor 1 1
        app4 dgeev mat m mat dummy vec l mat v "eigR"
        return (l,v)
  where r = rows m

fixeig  []  _ =  []
fixeig [_] [v] = [comp v]
fixeig ((r1:+i1):(r2:+i2):r) (v1:v2:vs)
    | r1 == r2 && i1 == (-i2) = toComplex (v1,v2) : toComplex (v1,scale (-1) v2) : fixeig r vs
    | otherwise = comp v1 : fixeig ((r2:+i2):r) (v2:vs)
  where scale = vectorMapValR Scale
fixeig _ _ = error "fixeig with impossible inputs"

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /dsyev/, which computes the eigenvalues and right eigenvectors of a symmetric real matrix:
--
-- if @(l,v)=eigSl m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are sorted in descending order (use eigS' for ascending order).
eigS :: Matrix Double -> (Vector Double, Matrix Double)
eigS m = (s', fliprl v)
    where (s,v) = eigS' (fmat m)
          s' = fromList . reverse . toList $  s

eigS' m
    | r == 1 = (fromList [flatten m `at` 0], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        app3 dsyev mat m vec l mat v "eigS"
        return (l,v)
  where r = rows m

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /zheev/, which computes the eigenvalues and right eigenvectors of a hermitian complex matrix:
--
-- if @(l,v)=eigH m@ then @m \<\> s v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are sorted in descending order (use eigH' for ascending order).
eigH :: Matrix (Complex Double) -> (Vector Double, Matrix (Complex Double))
eigH m = (s', fliprl v)
    where (s,v) = eigH' (fmat m)
          s' = fromList . reverse . toList $  s

eigH' m
    | r == 1 = (fromList [realPart (flatten m `at` 0)], singleton 1)
    | otherwise = unsafePerformIO $ do
        l <- createVector r
        v <- createMatrix ColumnMajor r r
        app3 zheev mat m vec l mat v "eigH"
        return (l,v)
  where r = rows m

-----------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveR_l" dgesv :: TMMM
foreign import ccall "LAPACK/lapack-aux.h linearSolveC_l" zgesv :: TCMCMCM

linearSolveSQAux f st a b
    | n1==n2 && n1==r = unsafePerformIO $ do
        s <- createMatrix ColumnMajor r c
        app3 f mat a mat b mat s st
        return s
    | otherwise = error $ st ++ " of nonsquare matrix"
  where n1 = rows a
        n2 = cols a
        r  = rows b
        c  = cols b

-- | Wrapper for LAPACK's /dgesv/, which solves a general real linear system (for several right-hand sides) internally using the lu decomposition.
linearSolveR :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveR a b = linearSolveSQAux dgesv "linearSolveR" (fmat a) (fmat b)

-- | Wrapper for LAPACK's /zgesv/, which solves a general complex linear system (for several right-hand sides) internally using the lu decomposition.
linearSolveC :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
linearSolveC a b = linearSolveSQAux zgesv "linearSolveC" (fmat a) (fmat b)

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h linearSolveLSR_l" dgels :: TMMM
foreign import ccall "LAPACK/lapack-aux.h linearSolveLSC_l" zgels :: TCMCMCM
foreign import ccall "LAPACK/lapack-aux.h linearSolveSVDR_l" dgelss :: Double -> TMMM
foreign import ccall "LAPACK/lapack-aux.h linearSolveSVDC_l" zgelss :: Double -> TCMCMCM

linearSolveAux f st a b = unsafePerformIO $ do
    r <- createMatrix ColumnMajor (max m n) nrhs
    app3 f mat a mat b mat r st
    return r
  where m = rows a
        n = cols a
        nrhs = cols b

-- | Wrapper for LAPACK's /dgels/, which obtains the least squared error solution of an overconstrained real linear system or the minimum norm solution of an underdetermined system, for several right-hand sides. For rank deficient systems use 'linearSolveSVDR'.
linearSolveLSR :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveLSR a b = subMatrix (0,0) (cols a, cols b) $
                     linearSolveAux dgels "linearSolverLSR" (fmat a) (fmat b)

-- | Wrapper for LAPACK's /zgels/, which obtains the least squared error solution of an overconstrained complex linear system or the minimum norm solution of an underdetermined system, for several right-hand sides. For rank deficient systems use 'linearSolveSVDC'.
linearSolveLSC :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
linearSolveLSC a b = subMatrix (0,0) (cols a, cols b) $
                     linearSolveAux zgels "linearSolveLSC" (fmat a) (fmat b)

-- | Wrapper for LAPACK's /dgelss/, which obtains the minimum norm solution to a real linear least squares problem Ax=B using the svd, for several right-hand sides. Admits rank deficient systems but it is slower than 'linearSolveLSR'. The effective rank of A is determined by treating as zero those singular valures which are less than rcond times the largest singular value. If rcond == Nothing machine precision is used.
linearSolveSVDR :: Maybe Double   -- ^ rcond
                -> Matrix Double  -- ^ coefficient matrix
                -> Matrix Double  -- ^ right hand sides (as columns)
                -> Matrix Double  -- ^ solution vectors (as columns)
linearSolveSVDR (Just rcond) a b = subMatrix (0,0) (cols a, cols b) $
                                   linearSolveAux (dgelss rcond) "linearSolveSVDR" (fmat a) (fmat b)
linearSolveSVDR Nothing a b = linearSolveSVDR (Just (-1)) (fmat a) (fmat b)

-- | Wrapper for LAPACK's /zgelss/, which obtains the minimum norm solution to a complex linear least squares problem Ax=B using the svd, for several right-hand sides. Admits rank deficient systems but it is slower than 'linearSolveLSC'. The effective rank of A is determined by treating as zero those singular valures which are less than rcond times the largest singular value. If rcond == Nothing machine precision is used.
linearSolveSVDC :: Maybe Double            -- ^ rcond
                -> Matrix (Complex Double) -- ^ coefficient matrix
                -> Matrix (Complex Double) -- ^ right hand sides (as columns)
                -> Matrix (Complex Double) -- ^ solution vectors (as columns)
linearSolveSVDC (Just rcond) a b = subMatrix (0,0) (cols a, cols b) $
                                   linearSolveAux (zgelss rcond) "linearSolveSVDC" (fmat a) (fmat b)
linearSolveSVDC Nothing a b = linearSolveSVDC (Just (-1)) (fmat a) (fmat b)

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h chol_l_H" zpotrf :: TCMCM
foreign import ccall "LAPACK/lapack-aux.h chol_l_S" dpotrf :: TMM

-- | Wrapper for LAPACK's /zpotrf/, which computes the Cholesky factorization of a
-- complex Hermitian positive definite matrix.
cholH :: Matrix (Complex Double) -> Matrix (Complex Double)
cholH = cholAux zpotrf "cholH" . fmat

-- | Wrapper for LAPACK's /dpotrf/, which computes the Cholesky factorization of a
-- real symmetric positive definite matrix.
cholS :: Matrix Double -> Matrix Double
cholS = cholAux dpotrf "cholS" . fmat

cholAux f st a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor n n
    app2 f mat a mat r st
    return r
  where n = rows a

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h qr_l_R" dgeqr2 :: TMVM
foreign import ccall "LAPACK/lapack-aux.h qr_l_C" zgeqr2 :: TCMCVCM

-- | Wrapper for LAPACK's /dgeqr2/, which computes a QR factorization of a real matrix.
qrR :: Matrix Double -> (Matrix Double, Vector Double)
qrR = qrAux dgeqr2 "qrR" . fmat

-- | Wrapper for LAPACK's /zgeqr2/, which computes a QR factorization of a complex matrix.
qrC :: Matrix (Complex Double) -> (Matrix (Complex Double), Vector (Complex Double))
qrC = qrAux zgeqr2 "qrC" . fmat

qrAux f st a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor m n
    tau <- createVector mn
    app3 f mat a vec tau mat r st
    return (r,tau)
  where m = rows a
        n = cols a
        mn = min m n

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h hess_l_R" dgehrd :: TMVM
foreign import ccall "LAPACK/lapack-aux.h hess_l_C" zgehrd :: TCMCVCM

-- | Wrapper for LAPACK's /dgehrd/, which computes a Hessenberg factorization of a square real matrix.
hessR :: Matrix Double -> (Matrix Double, Vector Double)
hessR = hessAux dgehrd "hessR" . fmat

-- | Wrapper for LAPACK's /zgehrd/, which computes a Hessenberg factorization of a square complex matrix.
hessC :: Matrix (Complex Double) -> (Matrix (Complex Double), Vector (Complex Double))
hessC = hessAux zgehrd "hessC" . fmat

hessAux f st a = unsafePerformIO $ do
    r <- createMatrix ColumnMajor m n
    tau <- createVector (mn-1)
    app3 f mat a vec tau mat r st
    return (r,tau)
  where m = rows a
        n = cols a
        mn = min m n

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h schur_l_R" dgees :: TMMM
foreign import ccall "LAPACK/lapack-aux.h schur_l_C" zgees :: TCMCMCM

-- | Wrapper for LAPACK's /dgees/, which computes a Schur factorization of a square real matrix.
schurR :: Matrix Double -> (Matrix Double, Matrix Double)
schurR = schurAux dgees "schurR" . fmat

-- | Wrapper for LAPACK's /zgees/, which computes a Schur factorization of a square complex matrix.
schurC :: Matrix (Complex Double) -> (Matrix (Complex Double), Matrix (Complex Double))
schurC = schurAux zgees "schurC" . fmat

schurAux f st a = unsafePerformIO $ do
    u <- createMatrix ColumnMajor n n
    s <- createMatrix ColumnMajor n n
    app3 f mat a mat u mat s st
    return (u,s)
  where n = rows a

-----------------------------------------------------------------------------------
foreign import ccall "LAPACK/lapack-aux.h lu_l_R" dgetrf :: TMVM
foreign import ccall "LAPACK/lapack-aux.h lu_l_C" zgetrf :: TCMVCM

-- | Wrapper for LAPACK's /dgetrf/, which computes a LU factorization of a general real matrix.
luR :: Matrix Double -> (Matrix Double, [Int])
luR = luAux dgetrf "luR" . fmat

-- | Wrapper for LAPACK's /zgees/, which computes a Schur factorization of a square complex matrix.
luC :: Matrix (Complex Double) -> (Matrix (Complex Double), [Int])
luC = luAux zgetrf "luC" . fmat

luAux f st a = unsafePerformIO $ do
    lu <- createMatrix ColumnMajor n m
    piv <- createVector (min n m)
    app3 f mat a vec piv mat lu st
    return (lu, map (pred.round) (toList piv))
  where n = rows a
        m = cols a

-----------------------------------------------------------------------------------
type TW a = CInt -> PD -> a
type TQ a = CInt -> CInt -> PC -> a

foreign import ccall "LAPACK/lapack-aux.h luS_l_R" dgetrs :: TMVMM
foreign import ccall "LAPACK/lapack-aux.h luS_l_C" zgetrs :: TQ (TW (TQ (TQ (IO CInt))))

-- | Wrapper for LAPACK's /dgetrs/, which solves a general real linear system (for several right-hand sides) from a precomputed LU decomposition.
lusR :: Matrix Double -> [Int] -> Matrix Double -> Matrix Double
lusR a piv b = lusAux dgetrs "lusR" (fmat a) piv (fmat b)

-- | Wrapper for LAPACK's /zgetrs/, which solves a general real linear system (for several right-hand sides) from a precomputed LU decomposition.
lusC :: Matrix (Complex Double) -> [Int] -> Matrix (Complex Double) -> Matrix (Complex Double)
lusC a piv b = lusAux zgetrs "lusC" (fmat a) piv (fmat b)

lusAux f st a piv b
    | n1==n2 && n2==n =unsafePerformIO $ do
         x <- createMatrix ColumnMajor n m
         app4 f mat a vec piv' mat b mat x st
         return x
    | otherwise = error $ st ++ " on LU factorization of nonsquare matrix"
  where n1 = rows a
        n2 = cols a
        n = rows b
        m = cols b
        piv' = fromList (map (fromIntegral.succ) piv) :: Vector Double
