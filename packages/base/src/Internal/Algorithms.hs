{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-----------------------------------------------------------------------------
{- |
Module      :  Internal.Algorithms
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

High level generic interface to common matrix computations.

Specific functions for particular base types can also be explicitly
imported from "Numeric.LinearAlgebra.LAPACK".

-}
-----------------------------------------------------------------------------

module Internal.Algorithms (
  module Internal.Algorithms,
  UpLo(..)
) where

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

import Internal.Vector
import Internal.Matrix
import Internal.Element
import Internal.Conversion
import Internal.LAPACK
import Internal.Numeric
import Data.List(foldl1')
import qualified Data.Array as A
import qualified Data.Vector.Storable as Vector
import Internal.ST
import Internal.Vectorized(range)
import Control.DeepSeq

{- | Generic linear algebra functions for double precision real and complex matrices.

(Single precision data can be converted using 'single' and 'double').

-}
class (Numeric t,
       Convert t,
       Normed Matrix t,
       Normed Vector t,
       Floating t,
       Linear t Vector,
       Linear t Matrix,
       Additive (Vector t),
       Additive (Matrix t),
       RealOf t ~ Double) => Field t where
    svd'         :: Matrix t -> (Matrix t, Vector Double, Matrix t)
    thinSVD'     :: Matrix t -> (Matrix t, Vector Double, Matrix t)
    sv'          :: Matrix t -> Vector Double
    luPacked'    :: Matrix t -> (Matrix t, [Int])
    luSolve'     :: (Matrix t, [Int]) -> Matrix t -> Matrix t
    mbLinearSolve' :: Matrix t -> Matrix t -> Maybe (Matrix t)
    linearSolve' :: Matrix t -> Matrix t -> Matrix t
    cholSolve'   :: Matrix t -> Matrix t -> Matrix t
    triSolve'   :: UpLo -> Matrix t -> Matrix t -> Matrix t
    triDiagSolve' :: Vector t -> Vector t -> Vector t -> Matrix t -> Matrix t
    ldlPacked'   :: Matrix t -> (Matrix t, [Int])
    ldlSolve'    :: (Matrix t, [Int]) -> Matrix t -> Matrix t
    linearSolveSVD' :: Matrix t -> Matrix t -> Matrix t
    linearSolveLS'  :: Matrix t -> Matrix t -> Matrix t
    eig'         :: Matrix t -> (Vector (Complex Double), Matrix (Complex Double))
    geig'        :: Matrix t -> Matrix t -> (Vector (Complex Double), Vector t, Matrix (Complex Double))
    eigSH''      :: Matrix t -> (Vector Double, Matrix t)
    eigOnly      :: Matrix t -> Vector (Complex Double)
    geigOnly     :: Matrix t -> Matrix t -> (Vector (Complex Double), Vector t)
    eigOnlySH    :: Matrix t -> Vector Double
    cholSH'      :: Matrix t -> Matrix t
    mbCholSH'    :: Matrix t -> Maybe (Matrix t)
    qr'          :: Matrix t -> (Matrix t, Vector t)
    qrgr'        :: Int -> (Matrix t, Vector t) -> Matrix t
    hess'        :: Matrix t -> (Matrix t, Matrix t)
    schur'       :: Matrix t -> (Matrix t, Matrix t)


instance Field Double where
    svd' = svdRd
    thinSVD' = thinSVDRd
    sv' = svR
    luPacked' = luR
    luSolve' (l_u,perm) = lusR l_u perm
    linearSolve' = linearSolveR                 -- (luSolve . luPacked) ??
    mbLinearSolve' = mbLinearSolveR
    cholSolve' = cholSolveR
    triSolve' = triSolveR
    triDiagSolve' = triDiagSolveR
    linearSolveLS' = linearSolveLSR
    linearSolveSVD' = linearSolveSVDR Nothing
    eig' = eigR
    eigSH'' = eigS
    geig' = eigG
    eigOnly = eigOnlyR
    geigOnly = eigOnlyG
    eigOnlySH = eigOnlyS
    cholSH' = cholS
    mbCholSH' = mbCholS
    qr' = qrR
    qrgr' = qrgrR
    hess' = unpackHess hessR
    schur' = schurR
    ldlPacked' = ldlR
    ldlSolve'= uncurry ldlsR

instance Field (Complex Double) where
#ifdef NOZGESDD
    svd' = svdC
    thinSVD' = thinSVDC
#else
    svd' = svdCd
    thinSVD' = thinSVDCd
#endif
    sv' = svC
    luPacked' = luC
    luSolve' (l_u,perm) = lusC l_u perm
    linearSolve' = linearSolveC
    mbLinearSolve' = mbLinearSolveC
    cholSolve' = cholSolveC
    triSolve' = triSolveC
    triDiagSolve' = triDiagSolveC
    linearSolveLS' = linearSolveLSC
    linearSolveSVD' = linearSolveSVDC Nothing
    eig' = eigC
    geig' = eigGC
    eigOnly = eigOnlyC
    geigOnly = eigOnlyGC
    eigSH'' = eigH
    eigOnlySH = eigOnlyH
    cholSH' = cholH
    mbCholSH' = mbCholH
    qr' = qrC
    qrgr' = qrgrC
    hess' = unpackHess hessC
    schur' = schurC
    ldlPacked' = ldlC
    ldlSolve' = uncurry ldlsC

--------------------------------------------------------------

square m = rows m == cols m

vertical m = rows m >= cols m

exactHermitian m = m `equal` ctrans m

--------------------------------------------------------------

{- | Full singular value decomposition.

@
a = (5><3)
 [  1.0,  2.0,  3.0
 ,  4.0,  5.0,  6.0
 ,  7.0,  8.0,  9.0
 , 10.0, 11.0, 12.0
 , 13.0, 14.0, 15.0 ] :: Matrix Double
@

>>> let (u,s,v) = svd a

>>> disp 3 u
5x5
-0.101   0.768   0.614   0.028  -0.149
-0.249   0.488  -0.503   0.172   0.646
-0.396   0.208  -0.405  -0.660  -0.449
-0.543  -0.072  -0.140   0.693  -0.447
-0.690  -0.352   0.433  -0.233   0.398

>>> s
[35.18264833189422,1.4769076999800903,1.089145439970417e-15]
it :: Vector Double

>>> disp 3 v
3x3
-0.519  -0.751   0.408
-0.576  -0.046  -0.816
-0.632   0.659   0.408

>>> let d = diagRect 0 s 5 3
>>> disp 3 d
5x3
35.183  0.000  0.000
 0.000  1.477  0.000
 0.000  0.000  0.000
 0.000  0.000  0.000

>>> disp 3 $ u <> d <> tr v
5x3
 1.000   2.000   3.000
 4.000   5.000   6.000
 7.000   8.000   9.000
10.000  11.000  12.000
13.000  14.000  15.000

-}
svd :: Field t => Matrix t -> (Matrix t, Vector Double, Matrix t)
svd = {-# SCC "svd" #-} g . svd'
  where
    g (u,s,v) = (u,s,tr v)

{- | A version of 'svd' which returns only the @min (rows m) (cols m)@ singular vectors of @m@.

If @(u,s,v) = thinSVD m@ then @m == u \<> diag s \<> tr v@.

@
a = (5><3)
 [  1.0,  2.0,  3.0
 ,  4.0,  5.0,  6.0
 ,  7.0,  8.0,  9.0
 , 10.0, 11.0, 12.0
 , 13.0, 14.0, 15.0 ] :: Matrix Double
@

>>> let (u,s,v) = thinSVD a

>>> disp 3 u
5x3
-0.101   0.768   0.614
-0.249   0.488  -0.503
-0.396   0.208  -0.405
-0.543  -0.072  -0.140
-0.690  -0.352   0.433

>>> s
[35.18264833189422,1.4769076999800903,1.089145439970417e-15]
it :: Vector Double

>>> disp 3 v
3x3
-0.519  -0.751   0.408
-0.576  -0.046  -0.816
-0.632   0.659   0.408

>>> disp 3 $ u <> diag s <> tr v
5x3
 1.000   2.000   3.000
 4.000   5.000   6.000
 7.000   8.000   9.000
10.000  11.000  12.000
13.000  14.000  15.000

-}
thinSVD :: Field t => Matrix t -> (Matrix t, Vector Double, Matrix t)
thinSVD = {-# SCC "thinSVD" #-} g . thinSVD'
  where
    g (u,s,v) = (u,s,tr v)


-- | Singular values only.
singularValues :: Field t => Matrix t -> Vector Double
singularValues = {-# SCC "singularValues" #-} sv'

-- | A version of 'svd' which returns an appropriate diagonal matrix with the singular values.
--
-- If @(u,d,v) = fullSVD m@ then @m == u \<> d \<> tr v@.
fullSVD :: Field t => Matrix t -> (Matrix t, Matrix Double, Matrix t)
fullSVD m = (u,d,v) where
    (u,s,v) = svd m
    d = diagRect 0 s r c
    r = rows m
    c = cols m

{- | Similar to 'thinSVD', returning only the nonzero singular values and the corresponding singular vectors.

@
a = (5><3)
 [  1.0,  2.0,  3.0
 ,  4.0,  5.0,  6.0
 ,  7.0,  8.0,  9.0
 , 10.0, 11.0, 12.0
 , 13.0, 14.0, 15.0 ] :: Matrix Double
@

>>> let (u,s,v) = compactSVD a

>>> disp 3 u
5x2
-0.101   0.768
-0.249   0.488
-0.396   0.208
-0.543  -0.072
-0.690  -0.352

>>> s
[35.18264833189422,1.476907699980091]
it :: Vector Double

>>> disp 3 u
5x2
-0.101   0.768
-0.249   0.488
-0.396   0.208
-0.543  -0.072
-0.690  -0.352

>>> disp 3 $ u <> diag s <> tr v
5x3
 1.000   2.000   3.000
 4.000   5.000   6.000
 7.000   8.000   9.000
10.000  11.000  12.000
13.000  14.000  15.000

-}
compactSVD :: Field t  => Matrix t -> (Matrix t, Vector Double, Matrix t)
compactSVD = compactSVDTol 1

-- | @compactSVDTol r@ is similar to 'compactSVD' (for which @r=1@), but uses tolerance @tol=r*g*eps*(max rows cols)@ to distinguish nonzero singular values, where @g@ is the greatest singular value. If @g<r*eps@, then only one singular value is returned.
compactSVDTol :: Field t  => Double -> Matrix t -> (Matrix t, Vector Double, Matrix t)
compactSVDTol r m = (u', subVector 0 d s, v') where
    (u,s,v) = thinSVD m
    d = rankSVD (r*eps) m s `max` 1
    u' = takeColumns d u
    v' = takeColumns d v


-- | Singular values and all right singular vectors (as columns).
rightSV :: Field t => Matrix t -> (Vector Double, Matrix t)
rightSV m | vertical m = let (_,s,v) = thinSVD m in (s,v)
          | otherwise  = let (_,s,v) = svd m     in (s,v)

-- | Singular values and all left singular vectors (as columns).
leftSV :: Field t => Matrix t -> (Matrix t, Vector Double)
leftSV m  | vertical m = let (u,s,_) = svd m     in (u,s)
          | otherwise  = let (u,s,_) = thinSVD m in (u,s)


--------------------------------------------------------------

-- | LU decomposition of a matrix in a compact format.
data LU t = LU (Matrix t) [Int] deriving Show

instance (NFData t, Numeric t) => NFData (LU t)
  where
    rnf (LU m _) = rnf m

-- | Obtains the LU decomposition of a matrix in a compact data structure suitable for 'luSolve'.
luPacked :: Field t => Matrix t -> LU t
luPacked x = {-# SCC "luPacked" #-} LU m p
  where
    (m,p) = luPacked' x

-- | Solution of a linear system (for several right hand sides) from the precomputed LU factorization obtained by 'luPacked'.
luSolve :: Field t => LU t -> Matrix t -> Matrix t
luSolve (LU m p) = {-# SCC "luSolve" #-} luSolve' (m,p)

-- | Solve a linear system (for square coefficient matrix and several right-hand sides) using the LU decomposition. For underconstrained or overconstrained systems use 'linearSolveLS' or 'linearSolveSVD'.
-- It is similar to 'luSolve' . 'luPacked', but @linearSolve@ raises an error if called on a singular system.
linearSolve :: Field t => Matrix t -> Matrix t -> Matrix t
linearSolve = {-# SCC "linearSolve" #-} linearSolve'

-- | Solve a linear system (for square coefficient matrix and several right-hand sides) using the LU decomposition, returning Nothing for a singular system. For underconstrained or overconstrained systems use 'linearSolveLS' or 'linearSolveSVD'.
mbLinearSolve :: Field t => Matrix t -> Matrix t -> Maybe (Matrix t)
mbLinearSolve = {-# SCC "linearSolve" #-} mbLinearSolve'

-- | Solve a symmetric or Hermitian positive definite linear system using a precomputed Cholesky decomposition obtained by 'chol'.
cholSolve
    :: Field t
    => Matrix t -- ^ Cholesky decomposition of the coefficient matrix
    -> Matrix t -- ^ right hand sides
    -> Matrix t -- ^ solution
cholSolve = {-# SCC "cholSolve" #-} cholSolve'

-- | Solve a triangular linear system. If `Upper` is specified then
-- all elements below the diagonal are ignored; if `Lower` is
-- specified then all elements above the diagonal are ignored.
triSolve
  :: Field t
  => UpLo     -- ^ `Lower` or `Upper`
  -> Matrix t -- ^ coefficient matrix
  -> Matrix t -- ^ right hand sides
  -> Matrix t -- ^ solution
triSolve = {-# SCC "triSolve" #-} triSolve'

-- | Solve a tridiagonal linear system. Suppose you wish to solve \(Ax = b\) where
--
-- \[
-- A =
-- \begin{bmatrix}
--    1.0 & 4.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0
-- \\ 3.0 & 1.0 & 4.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0
-- \\ 0.0 & 3.0 & 1.0 & 4.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0
-- \\ 0.0 & 0.0 & 3.0 & 1.0 & 4.0 & 0.0 & 0.0 & 0.0 & 0.0
-- \\ 0.0 & 0.0 & 0.0 & 3.0 & 1.0 & 4.0 & 0.0 & 0.0 & 0.0
-- \\ 0.0 & 0.0 & 0.0 & 0.0 & 3.0 & 1.0 & 4.0 & 0.0 & 0.0
-- \\ 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 3.0 & 1.0 & 4.0 & 0.0
-- \\ 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 3.0 & 1.0 & 4.0
-- \\ 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 0.0 & 3.0 & 1.0
-- \end{bmatrix}
-- \quad
-- b =
-- \begin{bmatrix}
--    1.0 &  1.0 &  1.0
-- \\ 1.0 & -1.0 &  2.0
-- \\ 1.0 &  1.0 &  3.0
-- \\ 1.0 & -1.0 &  4.0
-- \\ 1.0 &  1.0 &  5.0
-- \\ 1.0 & -1.0 &  6.0
-- \\ 1.0 &  1.0 &  7.0
-- \\ 1.0 & -1.0 &  8.0
-- \\ 1.0 &  1.0 &  9.0
-- \end{bmatrix}
-- \]
--
-- then
--
-- @
-- dL =  fromList [3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0]
-- d  =  fromList [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
-- dU =  fromList [4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0]
--
-- b = (9><3)
--     [
--       1.0,   1.0,   1.0,
--       1.0,  -1.0,   2.0,
--       1.0,   1.0,   3.0,
--       1.0,  -1.0,   4.0,
--       1.0,   1.0,   5.0,
--       1.0,  -1.0,   6.0,
--       1.0,   1.0,   7.0,
--       1.0,  -1.0,   8.0,
--       1.0,   1.0,   9.0
--     ]
--
-- x = triDiagSolve dL d dU b
-- @
--
triDiagSolve
  :: Field t
  => Vector t -- ^ lower diagonal: \(n - 1\) elements
  -> Vector t -- ^ diagonal: \(n\) elements
  -> Vector t -- ^ upper diagonal: \(n - 1\) elements
  -> Matrix t -- ^ right hand sides
  -> Matrix t -- ^ solution
triDiagSolve = {-# SCC "triDiagSolve" #-} triDiagSolve'

-- | Minimum norm solution of a general linear least squares problem Ax=B using the SVD. Admits rank-deficient systems but it is slower than 'linearSolveLS'. The effective rank of A is determined by treating as zero those singular valures which are less than 'eps' times the largest singular value.
linearSolveSVD :: Field t => Matrix t -> Matrix t -> Matrix t
linearSolveSVD = {-# SCC "linearSolveSVD" #-} linearSolveSVD'


-- | Least squared error solution of an overconstrained linear system, or the minimum norm solution of an underconstrained system. For rank-deficient systems use 'linearSolveSVD'.
linearSolveLS :: Field t => Matrix t -> Matrix t -> Matrix t
linearSolveLS = {-# SCC "linearSolveLS" #-} linearSolveLS'

--------------------------------------------------------------------------------

-- | LDL decomposition of a complex Hermitian or real symmetric matrix in a compact format.
data LDL t = LDL (Matrix t) [Int] deriving Show

instance (NFData t, Numeric t) => NFData (LDL t)
  where
    rnf (LDL m _) = rnf m

-- | Similar to 'ldlPacked', without checking that the input matrix is hermitian or symmetric. It works with the lower triangular part.
ldlPackedSH :: Field t => Matrix t -> LDL t
ldlPackedSH x = {-# SCC "ldlPacked" #-} LDL m p
  where
   (m,p) = ldlPacked' x

-- | Obtains the LDL decomposition of a matrix in a compact data structure suitable for 'ldlSolve'.
ldlPacked :: Field t => Herm t -> LDL t
ldlPacked (Herm m) = ldlPackedSH m

-- | Solution of a linear system (for several right hand sides) from a precomputed LDL factorization obtained by 'ldlPacked'.
--
--   Note: this can be slower than the general solver based on the LU decomposition.
ldlSolve :: Field t => LDL t -> Matrix t -> Matrix t
ldlSolve (LDL m p) = {-# SCC "ldlSolve" #-} ldlSolve' (m,p)

--------------------------------------------------------------

{- | Eigenvalues (not ordered) and eigenvectors (as columns) of a general square matrix.

If @(s,v) = eig m@ then @m \<> v == v \<> diag s@

@
a = (3><3)
 [ 3, 0, -2
 , 4, 5, -1
 , 3, 1,  0 ] :: Matrix Double
@

>>> let (l, v) = eig a

>>> putStr . dispcf 3 . asRow $ l
1x3
1.925+1.523i  1.925-1.523i  4.151

>>> putStr . dispcf 3 $ v
3x3
-0.455+0.365i  -0.455-0.365i   0.181
        0.603          0.603  -0.978
 0.033+0.543i   0.033-0.543i  -0.104

>>> putStr . dispcf 3 $ complex a <> v
3x3
-1.432+0.010i  -1.432-0.010i   0.753
 1.160+0.918i   1.160-0.918i  -4.059
-0.763+1.096i  -0.763-1.096i  -0.433

>>> putStr . dispcf 3 $ v <> diag l
3x3
-1.432+0.010i  -1.432-0.010i   0.753
 1.160+0.918i   1.160-0.918i  -4.059
-0.763+1.096i  -0.763-1.096i  -0.433

-}
eig :: Field t => Matrix t -> (Vector (Complex Double), Matrix (Complex Double))
eig = {-# SCC "eig" #-} eig'

-- | Generalized eigenvalues (not ordered) and eigenvectors (as columns) of a pair of nonsymmetric matrices.
-- Eigenvalues are represented as pairs of alpha, beta, where eigenvalue = alpha / beta. Alpha is always
-- complex, but betas has the same type as the input matrix.
--
-- If @(alphas, betas, v) = geig a b@, then @a \<> v == b \<> v \<> diag (alphas / betas)@
--
-- Note that beta can be 0 and that has reasonable interpretation.
geig :: Field t => Matrix t -> Matrix t -> (Vector (Complex Double), Vector t, Matrix (Complex Double))
geig = {-# SCC "geig" #-} geig'

-- | Eigenvalues (not ordered) of a general square matrix.
eigenvalues :: Field t => Matrix t -> Vector (Complex Double)
eigenvalues = {-# SCC "eigenvalues" #-} eigOnly

-- | Generalized eigenvalues of a pair of matrices. Represented as pairs of alpha, beta,
-- where eigenvalue is alpha / beta as in 'geig'.
geigenvalues :: Field t => Matrix t -> Matrix t -> (Vector (Complex Double), Vector t)
geigenvalues = {-# SCC "geigenvalues" #-} geigOnly

-- | Similar to 'eigSH' without checking that the input matrix is hermitian or symmetric. It works with the upper triangular part.
eigSH' :: Field t => Matrix t -> (Vector Double, Matrix t)
eigSH' = {-# SCC "eigSH'" #-} eigSH''

-- | Similar to 'eigenvaluesSH' without checking that the input matrix is hermitian or symmetric. It works with the upper triangular part.
eigenvaluesSH' :: Field t => Matrix t -> Vector Double
eigenvaluesSH' = {-# SCC "eigenvaluesSH'" #-} eigOnlySH

{- | Eigenvalues and eigenvectors (as columns) of a complex hermitian or real symmetric matrix, in descending order.

If @(s,v) = eigSH m@ then @m == v \<> diag s \<> tr v@

@
a = (3><3)
 [ 1.0, 2.0, 3.0
 , 2.0, 4.0, 5.0
 , 3.0, 5.0, 6.0 ]
@

>>> let (l, v) = eigSH a

>>> l
[11.344814282762075,0.17091518882717918,-0.5157294715892575]

>>> disp 3 $ v <> diag l <> tr v
3x3
1.000  2.000  3.000
2.000  4.000  5.000
3.000  5.000  6.000

-}
eigSH :: Field t => Herm t -> (Vector Double, Matrix t)
eigSH (Herm m) = eigSH' m

-- | Eigenvalues (in descending order) of a complex hermitian or real symmetric matrix.
eigenvaluesSH :: Field t => Herm t -> Vector Double
eigenvaluesSH (Herm m) = eigenvaluesSH' m

--------------------------------------------------------------

-- | QR decomposition of a matrix in compact form. (The orthogonal matrix is not explicitly formed.)
data QR t = QR (Matrix t) (Vector t)

instance (NFData t, Numeric t) => NFData (QR t)
  where
    rnf (QR m _) = rnf m


-- | QR factorization.
--
-- If @(q,r) = qr m@ then @m == q \<> r@, where q is unitary and r is upper triangular.
-- Note: the current implementation is very slow for large matrices. 'thinQR' is much faster.
qr :: Field t => Matrix t -> (Matrix t, Matrix t)
qr = {-# SCC "qr" #-} unpackQR . qr'

-- | A version of 'qr' which returns only the @min (rows m) (cols m)@ columns of @q@ and rows of @r@.
thinQR :: Field t => Matrix t -> (Matrix t, Matrix t)
thinQR = {-# SCC "thinQR" #-} thinUnpackQR . qr'

-- | Compute the QR decomposition of a matrix in compact form.
qrRaw :: Field t => Matrix t -> QR t
qrRaw m = QR x v
  where
    (x,v) = qr' m

-- | generate a matrix with k orthogonal columns from the compact QR decomposition obtained by 'qrRaw'.
--
qrgr :: Field t => Int -> QR t -> Matrix t
qrgr n (QR a t)
    | dim t > min (cols a) (rows a) || n < 0 || n > dim t = error "qrgr expects k <= min(rows,cols)"
    | otherwise = qrgr' n (a,t)

-- | RQ factorization.
--
-- If @(r,q) = rq m@ then @m == r \<> q@, where q is unitary and r is upper triangular.
-- Note: the current implementation is very slow for large matrices. 'thinRQ' is much faster.
rq :: Field t => Matrix t -> (Matrix t, Matrix t)
rq = {-# SCC "rq" #-} rqFromQR qr

-- | A version of 'rq' which returns only the @min (rows m) (cols m)@ columns of @r@ and rows of @q@.
thinRQ :: Field t => Matrix t -> (Matrix t, Matrix t)
thinRQ = {-# SCC "thinQR" #-} rqFromQR thinQR

rqFromQR :: Field t => (Matrix t -> (Matrix t, Matrix t)) -> Matrix t -> (Matrix t, Matrix t)
rqFromQR qr0 m = (r,q) where
    (q',r') = qr0 $ trans $ rev1 m
    r = rev2 (trans r')
    q = rev2 (trans q')
    rev1 = flipud . fliprl
    rev2 = fliprl . flipud

-- | Hessenberg factorization.
--
-- If @(p,h) = hess m@ then @m == p \<> h \<> tr p@, where p is unitary
-- and h is in upper Hessenberg form (it has zero entries below the first subdiagonal).
hess        :: Field t => Matrix t -> (Matrix t, Matrix t)
hess = hess'

-- | Schur factorization.
--
-- If @(u,s) = schur m@ then @m == u \<> s \<> tr u@, where u is unitary
-- and s is a Shur matrix. A complex Schur matrix is upper triangular. A real Schur matrix is
-- upper triangular in 2x2 blocks.
--
-- \"Anything that the Jordan decomposition can do, the Schur decomposition
-- can do better!\" (Van Loan)
schur       :: Field t => Matrix t -> (Matrix t, Matrix t)
schur = schur'


-- | Similar to 'cholSH', but instead of an error (e.g., caused by a matrix not positive definite) it returns 'Nothing'.
mbCholSH :: Field t => Matrix t -> Maybe (Matrix t)
mbCholSH = {-# SCC "mbCholSH" #-} mbCholSH'

-- | Similar to 'chol', without checking that the input matrix is hermitian or symmetric. It works with the upper triangular part.
cholSH      :: Field t => Matrix t -> Matrix t
cholSH = cholSH'

-- | Cholesky factorization of a positive definite hermitian or symmetric matrix.
--
-- If @c = chol m@ then @c@ is upper triangular and @m == tr c \<> c@.
chol :: Field t => Herm t ->  Matrix t
chol (Herm m) = {-# SCC "chol" #-} cholSH' m

-- | Similar to 'chol', but instead of an error (e.g., caused by a matrix not positive definite) it returns 'Nothing'.
mbChol :: Field t => Herm t -> Maybe (Matrix t)
mbChol (Herm m) = {-# SCC "mbChol" #-} mbCholSH' m



-- | Joint computation of inverse and logarithm of determinant of a square matrix.
invlndet :: Field t
         => Matrix t
         -> (Matrix t, (t, t)) -- ^ (inverse, (log abs det, sign or phase of det))
invlndet m | square m = (im,(ladm,sdm))
           | otherwise = error $ "invlndet of nonsquare "++ shSize m ++ " matrix"
  where
    lp@(LU lup perm) = luPacked m
    s = signlp (rows m) perm
    dg = toList $ takeDiag $ lup
    ladm = sum $ map (log.abs) dg
    sdm = s* product (map signum dg)
    im = luSolve lp (ident (rows m))


-- | Determinant of a square matrix. To avoid possible overflow or underflow use 'invlndet'.
det :: Field t => Matrix t -> t
det m | square m = {-# SCC "det" #-} s * (product $ toList $ takeDiag $ lup)
      | otherwise = error $ "det of nonsquare "++ shSize m ++ " matrix"
    where
      LU lup perm = luPacked m
      s = signlp (rows m) perm

-- | Explicit LU factorization of a general matrix.
--
-- If @(l,u,p,s) = lu m@ then @m == p \<> l \<> u@, where l is lower triangular,
-- u is upper triangular, p is a permutation matrix and s is the signature of the permutation.
lu :: Field t => Matrix t -> (Matrix t, Matrix t, Matrix t, t)
lu = luFact . luPacked

-- | Inverse of a square matrix. See also 'invlndet'.
inv :: Field t => Matrix t -> Matrix t
inv m | square m = m `linearSolve` ident (rows m)
      | otherwise = error $ "inv of nonsquare "++ shSize m ++ " matrix"


-- | Pseudoinverse of a general matrix with default tolerance ('pinvTol' 1, similar to GNU-Octave).
pinv :: Field t => Matrix t -> Matrix t
pinv = pinvTol 1

{- | @pinvTol r@ computes the pseudoinverse of a matrix with tolerance @tol=r*g*eps*(max rows cols)@, where g is the greatest singular value.

@
m = (3><3) [ 1, 0,    0
           , 0, 1,    0
           , 0, 0, 1e-10] :: Matrix Double
@

>>> pinv m
1. 0.           0.
0. 1.           0.
0. 0. 10000000000.

>>> pinvTol 1E8 m
1. 0. 0.
0. 1. 0.
0. 0. 1.

-}

pinvTol :: Field t => Double -> Matrix t -> Matrix t
pinvTol t m = v' `mXm` diag s' `mXm` ctrans u' where
    (u,s,v) = thinSVD m
    sl@(g:_) = toList s
    s' = real . fromList . map rec $ sl
    rec x = if x <= g*tol then x else 1/x
    tol = (fromIntegral (max r c) * g * t * eps)
    r = rows m
    c = cols m
    d = dim s
    u' = takeColumns d u
    v' = takeColumns d v


-- | Numeric rank of a matrix from the SVD decomposition.
rankSVD :: Element t
        => Double   -- ^ numeric zero (e.g. 1*'eps')
        -> Matrix t -- ^ input matrix m
        -> Vector Double -- ^ 'sv' of m
        -> Int      -- ^ rank of m
rankSVD teps m s = ranksv teps (max (rows m) (cols m)) (toList s)

-- | Numeric rank of a matrix from its singular values.
ranksv ::  Double   -- ^ numeric zero (e.g. 1*'eps')
        -> Int      -- ^ maximum dimension of the matrix
        -> [Double] -- ^ singular values
        -> Int      -- ^ rank of m
ranksv teps maxdim s = k where
    g = maximum s
    tol = fromIntegral maxdim * g * teps
    s' = filter (>tol) s
    k = if g > teps then length s' else 0

-- | The machine precision of a Double: @eps = 2.22044604925031e-16@ (the value used by GNU-Octave).
eps :: Double
eps =  2.22044604925031e-16


-- | 1 + 0.5*peps == 1,  1 + 0.6*peps /= 1
peps :: RealFloat x => x
peps = x where x = 2.0 ** fromIntegral (1 - floatDigits x)

-----------------------------------------------------------------------

-- | The nullspace of a matrix from its precomputed SVD decomposition.
nullspaceSVD :: Field t
             => Either Double Int -- ^ Left \"numeric\" zero (eg. 1*'eps'),
                                  --   or Right \"theoretical\" matrix rank.
             -> Matrix t          -- ^ input matrix m
             -> (Vector Double, Matrix t) -- ^ 'rightSV' of m
             -> Matrix t          -- ^ nullspace
nullspaceSVD hint a (s,v) = vs where
    tol = case hint of
        Left t -> t
        _      -> eps
    k = case hint of
        Right t -> t
        _       -> rankSVD tol a s
    vs = dropColumns k v


-- | The nullspace of a matrix. See also 'nullspaceSVD'.
nullspacePrec :: Field t
              => Double     -- ^ relative tolerance in 'eps' units (e.g., use 3 to get 3*'eps')
              -> Matrix t   -- ^ input matrix
              -> [Vector t] -- ^ list of unitary vectors spanning the nullspace
nullspacePrec t m = toColumns $ nullspaceSVD (Left (t*eps)) m (rightSV m)

-- | The nullspace of a matrix, assumed to be one-dimensional, with machine precision.
nullVector :: Field t => Matrix t -> Vector t
nullVector = last . nullspacePrec 1

-- | The range space a matrix from its precomputed SVD decomposition.
orthSVD :: Field t
             => Either Double Int -- ^ Left \"numeric\" zero (eg. 1*'eps'),
                                  --   or Right \"theoretical\" matrix rank.
             -> Matrix t          -- ^ input matrix m
             -> (Matrix t, Vector Double) -- ^ 'leftSV' of m
             -> Matrix t          -- ^ orth
orthSVD hint a (v,s) = vs where
    tol = case hint of
        Left t -> t
        _      -> eps
    k = case hint of
        Right t -> t
        _       -> rankSVD tol a s
    vs = takeColumns k v


orth :: Field t => Matrix t -> [Vector t]
-- ^ Return an orthonormal basis of the range space of a matrix
orth m = take r $ toColumns u
  where
    (u,s,_) = compactSVD m
    r = ranksv eps (max (rows m) (cols m)) (toList s)

------------------------------------------------------------------------

-- many thanks, quickcheck!

haussholder :: (Field a) => a -> Vector a -> Matrix a
haussholder tau v = ident (dim v) `sub` (tau `scale` (w `mXm` ctrans w))
    where w = asColumn v


zh k v = fromList $ replicate (k-1) 0 ++ (1:drop k xs)
              where xs = toList v

zt 0 v = v
zt k v = vjoin [subVector 0 (dim v - k) v, konst' 0 k]


unpackQR :: (Field t) => (Matrix t, Vector t) -> (Matrix t, Matrix t)
unpackQR (pq, tau) =  {-# SCC "unpackQR" #-} (q,r)
    where cs = toColumns pq
          m = rows pq
          n = cols pq
          mn = min m n
          r = fromColumns $ zipWith zt ([m-1, m-2 .. 1] ++ repeat 0) cs
          vs = zipWith zh [1..mn] cs
          hs = zipWith haussholder (toList tau) vs
          q = foldl1' mXm hs

thinUnpackQR :: (Field t) => (Matrix t, Vector t) -> (Matrix t, Matrix t)
thinUnpackQR (pq, tau) = (q, r)
  where mn = uncurry min $ size pq
        q = qrgr mn $ QR pq tau
        r = fromRows $ zipWith (\i v -> Vector.replicate i 0 Vector.++ Vector.drop i v) [0..mn-1] (toRows pq)

unpackHess :: (Field t) => (Matrix t -> (Matrix t,Vector t)) -> Matrix t -> (Matrix t, Matrix t)
unpackHess hf m
    | rows m == 1 = ((1><1)[1],m)
    | otherwise = (uH . hf) m

uH (pq, tau) = (p,h)
    where cs = toColumns pq
          m = rows pq
          n = cols pq
          mn = min m n
          h = fromColumns $ zipWith zt ([m-2, m-3 .. 1] ++ repeat 0) cs
          vs = zipWith zh [2..mn] cs
          hs = zipWith haussholder (toList tau) vs
          p = foldl1' mXm hs

--------------------------------------------------------------------------

-- | Reciprocal of the 2-norm condition number of a matrix, computed from the singular values.
rcond :: Field t => Matrix t -> Double
rcond m = last s / head s
    where s = toList (singularValues m)

-- | Number of linearly independent rows or columns. See also 'ranksv'
rank :: Field t => Matrix t -> Int
rank m = rankSVD eps m (singularValues m)

{-
expm' m = case diagonalize (complex m) of
    Just (l,v) -> v `mXm` diag (exp l) `mXm` inv v
    Nothing -> error "Sorry, expm not yet implemented for non-diagonalizable matrices"
  where exp = vectorMapC Exp
-}

diagonalize m = if rank v == n
                    then Just (l,v)
                    else Nothing
    where n = rows m
          (l,v) = if exactHermitian m
                    then let (l',v') = eigSH (trustSym m) in (real l', v')
                    else eig m

-- | Generic matrix functions for diagonalizable matrices. For instance:
--
-- @logm = matFunc log@
--
matFunc :: (Complex Double -> Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
matFunc f m = case diagonalize m of
    Just (l,v) -> v `mXm` diag (mapVector f l) `mXm` inv v
    Nothing -> error "Sorry, matFunc requires a diagonalizable matrix"

--------------------------------------------------------------

golubeps :: Integer -> Integer -> Double
golubeps p q = a * fromIntegral b / fromIntegral c where
    a = 2^^(3-p-q)
    b = fact p * fact q
    c = fact (p+q) * fact (p+q+1)
    fact n = product [1..n]

epslist :: [(Int,Double)]
epslist = [ (fromIntegral k, golubeps k k) | k <- [1..]]

geps delta = head [ k | (k,g) <- epslist, g<delta]


{- | Matrix exponential. It uses a direct translation of Algorithm 11.3.1 in Golub & Van Loan,
     based on a scaled Pade approximation.
-}
expm :: Field t => Matrix t -> Matrix t
expm = expGolub

expGolub :: Field t => Matrix t -> Matrix t
expGolub m = iterate msq f !! j
    where j = max 0 $ floor $ logBase 2 $ pnorm Infinity m
          a = m */ fromIntegral ((2::Int)^j)
          q = geps eps -- 7 steps
          eye = ident (rows m)
          work (k,c,x,n,d) = (k',c',x',n',d')
              where k' = k+1
                    c' = c * fromIntegral (q-k+1) / fromIntegral ((2*q-k+1)*k)
                    x' = a <> x
                    n' = n |+| (c' .* x')
                    d' = d |+| (((-1)^k * c') .* x')
          (_,_,_,nf,df) = iterate work (1,1,eye,eye,eye) !! q
          f = linearSolve df nf
          msq x = x <> x

          (<>) = multiply
          v */ x = scale (recip x) v
          (.*) = scale
          (|+|) = add

--------------------------------------------------------------

{- | Matrix square root. Currently it uses a simple iterative algorithm described in Wikipedia.
It only works with invertible matrices that have a real solution.

@m = (2><2) [4,9
           ,0,4] :: Matrix Double@

>>> sqrtm m
(2><2)
 [ 2.0, 2.25
 , 0.0,  2.0 ]

For diagonalizable matrices you can try 'matFunc' @sqrt@:

>>> matFunc sqrt ((2><2) [1,0,0,-1])
(2><2)
 [ 1.0 :+ 0.0, 0.0 :+ 0.0
 , 0.0 :+ 0.0, 0.0 :+ 1.0 ]

-}
sqrtm ::  Field t => Matrix t -> Matrix t
sqrtm = sqrtmInv

sqrtmInv x = fst $ fixedPoint $ iterate f (x, ident (rows x))
    where fixedPoint (a:b:rest) | pnorm PNorm1 (fst a |-| fst b) < peps   = a
                                | otherwise = fixedPoint (b:rest)
          fixedPoint _ = error "fixedpoint with impossible inputs"
          f (y,z) = (0.5 .* (y |+| inv z),
                     0.5 .* (inv y |+| z))
          (.*) = scale
          (|+|) = add
          (|-|) = sub

------------------------------------------------------------------

signlp r vals = foldl f 1 (zip [0..r-1] vals)
    where f s (a,b) | a /= b    = -s
                    | otherwise =  s

fixPerm r vals = (fromColumns $ A.elems res, sign)
  where
    v = [0..r-1]
    t = toColumns (ident r)
    (res,sign) = foldl swap (A.listArray (0,r-1) t, 1) (zip v vals)
    swap (arr,s) (a,b)
      | a /= b    = (arr A.// [(a, arr A.! b),(b,arr A.! a)],-s)
      | otherwise = (arr,s)

fixPerm' :: [Int] -> Vector I
fixPerm' s = res $ mutable f s0
  where
    s0 = reshape 1 (range (length s))
    res = flatten . fst
    swap m i j = rowOper (SWAP i j AllCols) m
    f :: (Num t, Element t) => (Int, Int) -> STMatrix s t -> ST s () -- needed because of TypeFamilies
    f _ p = sequence_ $ zipWith (swap p) [0..] s

triang r c h v = (r><c) [el s t | s<-[0..r-1], t<-[0..c-1]]
    where el p q = if q-p>=h then v else 1 - v

-- | Compute the explicit LU decomposition from the compact one obtained by 'luPacked'.
luFact :: Numeric t => LU t -> (Matrix t, Matrix t, Matrix t, t)
luFact (LU l_u perm)
    | r <= c    = (l ,u ,p, s)
    | otherwise = (l',u',p, s)
  where
    r = rows l_u
    c = cols l_u
    tu = triang r c 0 1
    tl = triang r c 0 0
    l = takeColumns r (l_u |*| tl) |+| diagRect 0 (konst' 1 r) r r
    u = l_u |*| tu
    (p,s) = fixPerm r perm
    l' = (l_u |*| tl) |+| diagRect 0 (konst' 1 c) r c
    u' = takeRows c (l_u |*| tu)
    (|+|) = add
    (|*|) = mul

---------------------------------------------------------------------------

data NormType = Infinity | PNorm1 | PNorm2 | Frobenius

class (RealFloat (RealOf t)) => Normed c t where
    pnorm :: NormType -> c t -> RealOf t

instance Normed Vector Double where
    pnorm PNorm1    = norm1
    pnorm PNorm2    = norm2
    pnorm Infinity  = normInf
    pnorm Frobenius = norm2

instance Normed Vector (Complex Double) where
    pnorm PNorm1    = norm1
    pnorm PNorm2    = norm2
    pnorm Infinity  = normInf
    pnorm Frobenius = pnorm PNorm2

instance Normed Vector Float where
    pnorm PNorm1    = norm1
    pnorm PNorm2    = norm2
    pnorm Infinity  = normInf
    pnorm Frobenius = pnorm PNorm2

instance Normed Vector (Complex Float) where
    pnorm PNorm1    = norm1
    pnorm PNorm2    = norm2
    pnorm Infinity  = normInf
    pnorm Frobenius = pnorm PNorm2


instance Normed Matrix Double where
    pnorm PNorm1    = maximum . map (pnorm PNorm1) . toColumns
    pnorm PNorm2    = (@>0) . singularValues
    pnorm Infinity  = pnorm PNorm1 . trans
    pnorm Frobenius = pnorm PNorm2 . flatten

instance Normed Matrix (Complex Double) where
    pnorm PNorm1    = maximum . map (pnorm PNorm1) . toColumns
    pnorm PNorm2    = (@>0) . singularValues
    pnorm Infinity  = pnorm PNorm1 . trans
    pnorm Frobenius = pnorm PNorm2 . flatten

instance Normed Matrix Float where
    pnorm PNorm1    = maximum . map (pnorm PNorm1) . toColumns
    pnorm PNorm2    = realToFrac . (@>0) . singularValues . double
    pnorm Infinity  = pnorm PNorm1 . trans
    pnorm Frobenius = pnorm PNorm2 . flatten

instance Normed Matrix (Complex Float) where
    pnorm PNorm1    = maximum . map (pnorm PNorm1) . toColumns
    pnorm PNorm2    = realToFrac . (@>0) . singularValues . double
    pnorm Infinity  = pnorm PNorm1 . trans
    pnorm Frobenius = pnorm PNorm2 . flatten

-- | Approximate number of common digits in the maximum element.
relativeError' :: (Normed c t, Container c t) => c t -> c t -> Int
relativeError' x y = dig (norm (x `sub` y) / norm x)
    where norm = pnorm Infinity
          dig r = round $ -logBase 10 (realToFrac r :: Double)


relativeError :: Num a => (a -> Double) -> a -> a -> Double
relativeError norm a b = r
  where
    na = norm a
    nb = norm b
    nab = norm (a-b)
    mx = max na nb
    mn = min na nb
    r = if mn < peps
        then mx
        else nab/mx


----------------------------------------------------------------------

-- | Generalized symmetric positive definite eigensystem Av = lBv,
-- for A and B symmetric, B positive definite.
geigSH :: Field t
        => Herm t -- ^ A
        -> Herm t -- ^ B
        -> (Vector Double, Matrix t)
geigSH (Herm a) (Herm b) = geigSH' a b

geigSH' :: Field t
        => Matrix t -- ^ A
        -> Matrix t -- ^ B
        -> (Vector Double, Matrix t)
geigSH' a b = (l,v')
  where
    u = cholSH b
    iu = inv u
    c = ctrans iu <> a <> iu
    (l,v) = eigSH' c
    v' = iu <> v
    (<>) = mXm

--------------------------------------------------------------------------------

-- | A matrix that, by construction, it is known to be complex Hermitian or real symmetric.
--
--   It can be created using 'sym', 'mTm', or 'trustSym', and the matrix can be extracted using 'unSym'.
newtype Herm t = Herm (Matrix t) deriving Show

instance (NFData t, Numeric t) => NFData (Herm t)
  where
    rnf (Herm m) = rnf m

-- | Extract the general matrix from a 'Herm' structure, forgetting its symmetric or Hermitian property.
unSym :: Herm t -> Matrix t
unSym (Herm x) = x

-- | Compute the complex Hermitian or real symmetric part of a square matrix (@(x + tr x)/2@).
sym :: Field t => Matrix t -> Herm t
sym x = Herm (scale 0.5 (tr x `add` x))

-- | Compute the contraction @tr x <> x@ of a general matrix.
mTm :: Numeric t => Matrix t -> Herm t
mTm x = Herm (tr x `mXm` x)

instance Field t => Linear t Herm where
    scale  x (Herm m) = Herm (scale x m)

instance Field t => Additive (Herm t) where
    add (Herm a) (Herm b) = Herm (a `add` b)

-- | At your own risk, declare that a matrix is complex Hermitian or real symmetric
--   for usage in 'chol', 'eigSH', etc. Only a triangular part of the matrix will be used.
trustSym :: Matrix t -> Herm t
trustSym x = (Herm x)
