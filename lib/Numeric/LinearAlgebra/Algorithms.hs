{-# OPTIONS_GHC -XFlexibleContexts -XFlexibleInstances #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Algorithms
Copyright   :  (c) Alberto Ruiz 2006-9
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Generic interface for the most common functions. Using it we can write higher level algorithms and testing properties for both real and complex matrices.

Specific functions for particular base types can also be explicitly
imported from "Numeric.LinearAlgebra.LAPACK".

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Algorithms (
-- * Supported types
    Field(),
-- * Products
    multiply, dot,
    outer, kronecker,
-- * Linear Systems
    linearSolve,
    luSolve,
    linearSolveLS,
    linearSolveSVD,
    inv, pinv,
    det, rank, rcond,
-- * Matrix factorizations
-- ** Singular value decomposition
    svd,
    fullSVD,
    thinSVD,
    compactSVD,
    singularValues,
    leftSV, rightSV,
-- ** Eigensystems
    eig, eigSH, eigSH',
    eigenvalues, eigenvaluesSH, eigenvaluesSH',
-- ** QR
    qr, rq,
-- ** Cholesky
    chol, cholSH,
-- ** Hessenberg
    hess,
-- ** Schur
    schur,
-- ** LU
    lu, luPacked,
-- * Matrix functions
    expm,
    sqrtm,
    matFunc,
-- * Nullspace
    nullspacePrec,
    nullVector,
    nullspaceSVD,
-- * Norms
    Normed(..), NormType(..),
-- * Misc
    ctrans,
    eps, i,
-- * Util
    haussholder,
    unpackQR, unpackHess,
    pinvTol,
    ranksv,
    full, economy
) where


import Data.Packed.Internal hiding (fromComplex, toComplex, conj, (//))
import Data.Packed
import Numeric.GSL.Vector
import Numeric.LinearAlgebra.LAPACK as LAPACK
import Numeric.LinearAlgebra.Linear
import Data.List(foldl1')
import Data.Array


-- | Auxiliary typeclass used to define generic computations for both real and complex matrices.
class (Normed (Matrix t), Linear Vector t, Linear Matrix t) => Field t where
    svd'         :: Matrix t -> (Matrix t, Vector Double, Matrix t)
    thinSVD'     :: Matrix t -> (Matrix t, Vector Double, Matrix t)
    sv'          :: Matrix t -> Vector Double
    luPacked'    :: Matrix t -> (Matrix t, [Int])
    luSolve'     :: (Matrix t, [Int]) -> Matrix t -> Matrix t
    linearSolve' :: Matrix t -> Matrix t -> Matrix t
    linearSolveSVD' :: Matrix t -> Matrix t -> Matrix t
    linearSolveLS'  :: Matrix t -> Matrix t -> Matrix t
    eig'         :: Matrix t -> (Vector (Complex Double), Matrix (Complex Double))
    eigSH''      :: Matrix t -> (Vector Double, Matrix t)
    eigOnly      :: Matrix t -> Vector (Complex Double)
    eigOnlySH    :: Matrix t -> Vector Double
    cholSH'      :: Matrix t -> Matrix t
    qr'          :: Matrix t -> (Matrix t, Matrix t)
    hess'        :: Matrix t -> (Matrix t, Matrix t)
    schur'       :: Matrix t -> (Matrix t, Matrix t)
    ctrans'      :: Matrix t -> Matrix t
    multiply'    :: Matrix t -> Matrix t -> Matrix t


instance Field Double where
    svd' = svdRd
    thinSVD' = thinSVDRd
    sv' = svR
    luPacked' = luR
    luSolve' (l_u,perm) = lusR l_u perm
    linearSolve' = linearSolveR                 -- (luSolve . luPacked) ??
    linearSolveLS' = linearSolveLSR
    linearSolveSVD' = linearSolveSVDR Nothing
    ctrans' = trans
    eig' = eigR
    eigSH'' = eigS
    eigOnly = eigOnlyR
    eigOnlySH = eigOnlyS
    cholSH' = cholS
    qr' = unpackQR . qrR
    hess' = unpackHess hessR
    schur' = schurR
    multiply' = multiplyR

instance Field (Complex Double) where
    svd' = svdCd
    thinSVD' = thinSVDCd
    sv' = svC
    luPacked' = luC
    luSolve' (l_u,perm) = lusC l_u perm
    linearSolve' = linearSolveC
    linearSolveLS' = linearSolveLSC
    linearSolveSVD' = linearSolveSVDC Nothing
    ctrans' = conj . trans
    eig' = eigC
    eigOnly = eigOnlyC
    eigSH'' = eigH
    eigOnlySH = eigOnlyH
    cholSH' = cholH
    qr' = unpackQR . qrC
    hess' = unpackHess hessC
    schur' = schurC
    multiply' = multiplyC

--------------------------------------------------------------

square m = rows m == cols m

vertical m = rows m >= cols m

exactHermitian m = m `equal` ctrans m

--------------------------------------------------------------

-- | Full singular value decomposition.
svd :: Field t => Matrix t -> (Matrix t, Vector Double, Matrix t)
svd = svd'

-- | A version of 'svd' which returns only the @min (rows m) (cols m)@ singular vectors of @m@.
--
-- If @(u,s,v) = thinSVD m@ then @m == u \<> diag s \<> trans v@.
thinSVD :: Field t => Matrix t -> (Matrix t, Vector Double, Matrix t)
thinSVD = thinSVD'

-- | Singular values only.
singularValues :: Field t => Matrix t -> Vector Double
singularValues = sv'

-- | A version of 'svd' which returns an appropriate diagonal matrix with the singular values.
--
-- If @(u,d,v) = fullSVD m@ then @m == u \<> d \<> trans v@.
fullSVD :: Field t => Matrix t -> (Matrix t, Matrix Double, Matrix t)
fullSVD m = (u,d,v) where
    (u,s,v) = svd m
    d = diagRect s r c
    r = rows m
    c = cols m

-- | Similar to 'thinSVD', returning only the nonzero singular values and the corresponding singular vectors.
compactSVD :: Field t  => Matrix t -> (Matrix t, Vector Double, Matrix t)
compactSVD m = (u', subVector 0 d s, v') where
    (u,s,v) = thinSVD m
    d = rankSVD (1*eps) m s `max` 1
    u' = takeColumns d u
    v' = takeColumns d v


-- | Singular values and all right singular vectors.
rightSV :: Field t => Matrix t -> (Vector Double, Matrix t)
rightSV m | vertical m = let (_,s,v) = thinSVD m in (s,v)
          | otherwise  = let (_,s,v) = svd m     in (s,v)

-- | Singular values and all right singular vectors.
leftSV :: Field t => Matrix t -> (Matrix t, Vector Double)
leftSV m  | vertical m = let (u,s,_) = svd m     in (u,s)
          | otherwise  = let (u,s,_) = thinSVD m in (u,s)


{-# DEPRECATED full "use fullSVD instead" #-}
full svdFun m = (u, d ,v) where
    (u,s,v) = svdFun m
    d = diagRect s r c
    r = rows m
    c = cols m

{-# DEPRECATED economy "use compactSVD instead" #-}
economy svdFun m = (u', subVector 0 d s, v') where
    (u,s,v) = svdFun m
    d = rankSVD (1*eps) m s `max` 1
    u' = takeColumns d u
    v' = takeColumns d v


--------------------------------------------------------------

-- | Obtains the LU decomposition of a matrix in a compact data structure suitable for 'luSolve'.
luPacked    :: Field t => Matrix t -> (Matrix t, [Int])
luPacked = luPacked'

-- | Solution of a linear system (for several right hand sides) from the precomputed LU factorization obtained by 'luPacked'.
luSolve     :: Field t => (Matrix t, [Int]) -> Matrix t -> Matrix t
luSolve = luSolve'

-- | Solve a linear system (for square coefficient matrix and several right-hand sides) using the LU decomposition. For underconstrained or overconstrained systems use 'linearSolveLS' or 'linearSolveSVD'.
-- It is similar to 'luSolve' . 'luPacked', but @linearSolve@ raises an error if called on a singular system.
linearSolve :: Field t => Matrix t -> Matrix t -> Matrix t
linearSolve = linearSolve'

-- | Minimum norm solution of a general linear least squares problem Ax=B using the SVD. Admits rank-deficient systems but it is slower than 'linearSolveLS'. The effective rank of A is determined by treating as zero those singular valures which are less than 'eps' times the largest singular value.
linearSolveSVD :: Field t => Matrix t -> Matrix t -> Matrix t
linearSolveSVD = linearSolveSVD'


-- | Least squared error solution of an overconstrained linear system, or the minimum norm solution of an underconstrained system. For rank-deficient systems use 'linearSolveSVD'.
linearSolveLS :: Field t => Matrix t -> Matrix t -> Matrix t
linearSolveLS = linearSolveLS'

--------------------------------------------------------------

-- | Eigenvalues and eigenvectors of a general square matrix.
--
-- If @(s,v) = eig m@ then @m \<> v == v \<> diag s@
eig         :: Field t => Matrix t -> (Vector (Complex Double), Matrix (Complex Double))
eig = eig'

-- | Eigenvalues of a general square matrix.
eigenvalues :: Field t => Matrix t -> Vector (Complex Double)
eigenvalues = eigOnly

-- | Similar to 'eigSH' without checking that the input matrix is hermitian or symmetric.
eigSH'      :: Field t => Matrix t -> (Vector Double, Matrix t)
eigSH' = eigSH''

-- | Similar to 'eigenvaluesSH' without checking that the input matrix is hermitian or symmetric.
eigenvaluesSH' :: Field t => Matrix t -> Vector Double
eigenvaluesSH' = eigOnlySH

-- | Eigenvalues and Eigenvectors of a complex hermitian or real symmetric matrix.
--
-- If @(s,v) = eigSH m@ then @m == v \<> diag s \<> ctrans v@
eigSH :: Field t => Matrix t -> (Vector Double, Matrix t)
eigSH m | exactHermitian m = eigSH' m
        | otherwise = error "eigSH requires complex hermitian or real symmetric matrix"

-- | Eigenvalues of a complex hermitian or real symmetric matrix.
eigenvaluesSH :: Field t => Matrix t -> Vector Double
eigenvaluesSH m | exactHermitian m = eigenvaluesSH' m
                | otherwise = error "eigenvaluesSH requires complex hermitian or real symmetric matrix"

--------------------------------------------------------------

-- | QR factorization.
--
-- If @(q,r) = qr m@ then @m == q \<> r@, where q is unitary and r is upper triangular.
qr          :: Field t => Matrix t -> (Matrix t, Matrix t)
qr = qr'

-- | RQ factorization.
--
-- If @(r,q) = rq m@ then @m == r \<> q@, where q is unitary and r is upper triangular.
rq :: Field t => Matrix t -> (Matrix t, Matrix t)
rq m = (r,q) where
    (q',r') = qr $ trans $ rev1 m
    r = rev2 (trans r')
    q = rev2 (trans q')
    rev1 = flipud . fliprl
    rev2 = fliprl . flipud

-- | Hessenberg factorization.
--
-- If @(p,h) = hess m@ then @m == p \<> h \<> ctrans p@, where p is unitary
-- and h is in upper Hessenberg form (it has zero entries below the first subdiagonal).
hess        :: Field t => Matrix t -> (Matrix t, Matrix t)
hess = hess'

-- | Schur factorization.
--
-- If @(u,s) = schur m@ then @m == u \<> s \<> ctrans u@, where u is unitary
-- and s is a Shur matrix. A complex Schur matrix is upper triangular. A real Schur matrix is
-- upper triangular in 2x2 blocks.
--
-- \"Anything that the Jordan decomposition can do, the Schur decomposition
-- can do better!\" (Van Loan)
schur       :: Field t => Matrix t -> (Matrix t, Matrix t)
schur = schur'

-- | Generic conjugate transpose.
ctrans :: Field t => Matrix t -> Matrix t
ctrans = ctrans'

-- | Matrix product.
multiply :: Field t => Matrix t -> Matrix t -> Matrix t
multiply = multiply'


-- | Similar to 'chol' without checking that the input matrix is hermitian or symmetric.
cholSH      :: Field t => Matrix t -> Matrix t
cholSH = cholSH'

-- | Cholesky factorization of a positive definite hermitian or symmetric matrix.
--
-- If @c = chol m@ then @m == ctrans c \<> c@.
chol :: Field t => Matrix t ->  Matrix t
chol m | exactHermitian m = cholSH m
       | otherwise = error "chol requires positive definite complex hermitian or real symmetric matrix"




-- | Determinant of a square matrix.
det :: Field t => Matrix t -> t
det m | square m = s * (product $ toList $ takeDiag $ lup)
      | otherwise = error "det of nonsquare matrix"
    where (lup,perm) = luPacked m
          s = signlp (rows m) perm

-- | Explicit LU factorization of a general matrix.
--
-- If @(l,u,p,s) = lu m@ then @m == p \<> l \<> u@, where l is lower triangular,
-- u is upper triangular, p is a permutation matrix and s is the signature of the permutation.
lu :: Field t => Matrix t -> (Matrix t, Matrix t, Matrix t, t)
lu = luFact . luPacked

-- | Inverse of a square matrix.
inv :: Field t => Matrix t -> Matrix t
inv m | square m = m `linearSolve` ident (rows m)
      | otherwise = error "inv of nonsquare matrix"

-- | Pseudoinverse of a general matrix.
pinv :: Field t => Matrix t -> Matrix t
pinv m = linearSolveSVD m (ident (rows m))

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

-- | The imaginary unit: @i = 0.0 :+ 1.0@
i :: Complex Double
i = 0:+1


-- matrix product
mXm :: (Num t, Field t) => Matrix t -> Matrix t -> Matrix t
mXm = multiply

-- matrix - vector product
mXv :: (Num t, Field t) => Matrix t -> Vector t -> Vector t
mXv m v = flatten $ m `mXm` (asColumn v)

-- vector - matrix product
vXm :: (Num t, Field t) => Vector t -> Matrix t -> Vector t
vXm v m = flatten $ (asRow v) `mXm` m


---------------------------------------------------------------------------

norm2 :: Vector Double -> Double
norm2 = toScalarR Norm2

norm1 :: Vector Double -> Double
norm1 = toScalarR AbsSum

data NormType = Infinity | PNorm1 | PNorm2 -- PNorm Int

pnormRV PNorm2 = norm2
pnormRV PNorm1 = norm1
pnormRV Infinity = vectorMax . vectorMapR Abs
--pnormRV _ = error "pnormRV not yet defined"

pnormCV PNorm2 = norm2 . asReal
pnormCV PNorm1 = norm1 . mapVector magnitude
pnormCV Infinity = vectorMax . mapVector magnitude
--pnormCV _ = error "pnormCV not yet defined"

pnormRM PNorm2 m = singularValues m @> 0
pnormRM PNorm1 m = vectorMax $ constant 1 (rows m) `vXm` liftMatrix (vectorMapR Abs) m
pnormRM Infinity m = vectorMax $ liftMatrix (vectorMapR Abs) m `mXv` constant 1 (cols m)
--pnormRM _ _ = error "p norm not yet defined"

pnormCM PNorm2 m = singularValues m @> 0
pnormCM PNorm1 m = vectorMax $ constant 1 (rows m) `vXm` liftMatrix (mapVector magnitude) m
pnormCM Infinity m = vectorMax $ liftMatrix (mapVector magnitude) m `mXv` constant 1 (cols m)
--pnormCM _ _ = error "p norm not yet defined"

-- | Objects which have a p-norm.
-- Using it you can define convenient shortcuts:
--
-- @norm2 x = pnorm PNorm2 x@
--
-- @frobenius m = norm2 . flatten $ m@
class Normed t where
    pnorm :: NormType -> t -> Double

instance Normed (Vector Double) where
    pnorm = pnormRV

instance Normed (Vector (Complex Double)) where
    pnorm = pnormCV

instance Normed (Matrix Double) where
    pnorm = pnormRM

instance Normed (Matrix (Complex Double)) where
    pnorm = pnormCM

-----------------------------------------------------------------------

-- | The nullspace of a matrix from its SVD decomposition.
nullspaceSVD :: Field t
             => Either Double Int -- ^ Left \"numeric\" zero (eg. 1*'eps'),
                                  --   or Right \"theoretical\" matrix rank.
             -> Matrix t          -- ^ input matrix m
             -> (Vector Double, Matrix t) -- ^ 'rightSV' of m
             -> [Vector t]        -- ^ list of unitary vectors spanning the nullspace
nullspaceSVD hint a (s,v) = vs where
    r = rows a
    c = cols a
    tol = case hint of
        Left t -> t
        _      -> eps
    k = case hint of
        Right t -> t
        _       -> rankSVD tol a s
    vs = drop k $ toRows $ ctrans v


-- | The nullspace of a matrix. See also 'nullspaceSVD'.
nullspacePrec :: Field t
              => Double     -- ^ relative tolerance in 'eps' units (e.g., use 3 to get 3*'eps')
              -> Matrix t   -- ^ input matrix
              -> [Vector t] -- ^ list of unitary vectors spanning the nullspace
nullspacePrec t m = nullspaceSVD (Left (t*eps)) m (rightSV m)

-- | The nullspace of a matrix, assumed to be one-dimensional, with machine precision.
nullVector :: Field t => Matrix t -> Vector t
nullVector = last . nullspacePrec 1

------------------------------------------------------------------------

{-  Pseudoinverse of a real matrix with the desired tolerance, expressed as a
multiplicative factor of the default tolerance used by GNU-Octave (see 'pinv').

@\> let m = 'fromLists' [[1,0,    0]
                    ,[0,1,    0]
                    ,[0,0,1e-10]]
\  --
\> 'pinv' m 
1. 0.           0.
0. 1.           0.
0. 0. 10000000000.
\  --
\> pinvTol 1E8 m
1. 0. 0.
0. 1. 0.
0. 0. 1.@

-}
--pinvTol :: Double -> Matrix Double -> Matrix Double
pinvTol t m = v' `mXm` diag s' `mXm` trans u' where
    (u,s,v) = thinSVDRd m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x < g*tol then 1 else 1/x
    tol = (fromIntegral (max r c) * g * t * eps)
    r = rows m
    c = cols m
    d = dim s
    u' = takeColumns d u
    v' = takeColumns d v

---------------------------------------------------------------------

-- many thanks, quickcheck!

haussholder :: (Field a) => a -> Vector a -> Matrix a
haussholder tau v = ident (dim v) `sub` (tau `scale` (w `mXm` ctrans w))
    where w = asColumn v


zh k v = fromList $ replicate (k-1) 0 ++ (1:drop k xs)
              where xs = toList v

zt 0 v = v
zt k v = join [subVector 0 (dim v - k) v, constant 0 k]


unpackQR :: (Field t) => (Matrix t, Vector t) -> (Matrix t, Matrix t)
unpackQR (pq, tau) = (q,r)
    where cs = toColumns pq
          m = rows pq
          n = cols pq
          mn = min m n
          r = fromColumns $ zipWith zt ([m-1, m-2 .. 1] ++ repeat 0) cs
          vs = zipWith zh [1..mn] cs
          hs = zipWith haussholder (toList tau) vs
          q = foldl1' mXm hs

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

-- | Number of linearly independent rows or columns.
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
                    then let (l',v') = eigSH m in (real l', v')
                    else eig m

-- | Generic matrix functions for diagonalizable matrices. For instance:
--
-- @logm = matFunc log@
--
matFunc :: Field t => (Complex Double -> Complex Double) -> Matrix t -> Matrix (Complex Double)
matFunc f m = case diagonalize (complex m) of
    Just (l,v) -> v `mXm` diag (mapVector f l) `mXm` inv v
    Nothing -> error "Sorry, matFunc requires a diagonalizable matrix" 

--------------------------------------------------------------

golubeps :: Integer -> Integer -> Double
golubeps p q = a * fromIntegral b / fromIntegral c where
    a = 2^^(3-p-q)
    b = fact p * fact q
    c = fact (p+q) * fact (p+q+1)
    fact n = product [1..n]

epslist = [ (fromIntegral k, golubeps k k) | k <- [1..]]

geps delta = head [ k | (k,g) <- epslist, g<delta]

expGolub m = iterate msq f !! j
    where j = max 0 $ floor $ log2 $ pnorm Infinity m
          log2 x = log x / log 2
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

{- | Matrix exponential. It uses a direct translation of Algorithm 11.3.1 in Golub & Van Loan,
     based on a scaled Pade approximation.
-}
expm :: Field t => Matrix t -> Matrix t
expm = expGolub

--------------------------------------------------------------

{- | Matrix square root. Currently it uses a simple iterative algorithm described in Wikipedia.
It only works with invertible matrices that have a real solution. For diagonalizable matrices you can try @matFunc sqrt@.

@m = (2><2) [4,9
           ,0,4] :: Matrix Double@

@\>sqrtm m
(2><2)
 [ 2.0, 2.25
 , 0.0,  2.0 ]@
-}
sqrtm :: Field t => Matrix t -> Matrix t
sqrtm = sqrtmInv

sqrtmInv x = fst $ fixedPoint $ iterate f (x, ident (rows x))
    where fixedPoint (a:b:rest) | pnorm PNorm1 (fst a |-| fst b) < eps   = a
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

swap (arr,s) (a,b) | a /= b    = (arr // [(a, arr!b),(b,arr!a)],-s)
                   | otherwise = (arr,s)

fixPerm r vals = (fromColumns $ elems res, sign)
    where v = [0..r-1]
          s = toColumns (ident r)
          (res,sign) = foldl swap (listArray (0,r-1) s, 1) (zip v vals)

triang r c h v = (r><c) [el s t | s<-[0..r-1], t<-[0..c-1]]
    where el p q = if q-p>=h then v else 1 - v

luFact (l_u,perm) | r <= c    = (l ,u ,p, s)
                  | otherwise = (l',u',p, s)
  where
    r = rows l_u
    c = cols l_u
    tu = triang r c 0 1
    tl = triang r c 0 0
    l = takeColumns r (l_u |*| tl) |+| diagRect (constant 1 r) r r
    u = l_u |*| tu
    (p,s) = fixPerm r perm
    l' = (l_u |*| tl) |+| diagRect (constant 1 c) r c
    u' = takeRows c (l_u |*| tu)
    (|+|) = add
    (|*|) = mul

--------------------------------------------------

-- | Euclidean inner product.
dot :: (Field t) => Vector t -> Vector t -> t
dot u v = multiply r c  @@> (0,0)
    where r = asRow u
          c = asColumn v


{- | Outer product of two vectors.

@\> 'fromList' [1,2,3] \`outer\` 'fromList' [5,2,3]
(3><3)
 [  5.0, 2.0, 3.0
 , 10.0, 4.0, 6.0
 , 15.0, 6.0, 9.0 ]@
-}
outer :: (Field t) => Vector t -> Vector t -> Matrix t
outer u v = asColumn u `multiply` asRow v

{- | Kronecker product of two matrices.

@m1=(2><3)
 [ 1.0,  2.0, 0.0
 , 0.0, -1.0, 3.0 ]
m2=(4><3)
 [  1.0,  2.0,  3.0
 ,  4.0,  5.0,  6.0
 ,  7.0,  8.0,  9.0
 , 10.0, 11.0, 12.0 ]@

@\> kronecker m1 m2
(8><9)
 [  1.0,  2.0,  3.0,   2.0,   4.0,   6.0,  0.0,  0.0,  0.0
 ,  4.0,  5.0,  6.0,   8.0,  10.0,  12.0,  0.0,  0.0,  0.0
 ,  7.0,  8.0,  9.0,  14.0,  16.0,  18.0,  0.0,  0.0,  0.0
 , 10.0, 11.0, 12.0,  20.0,  22.0,  24.0,  0.0,  0.0,  0.0
 ,  0.0,  0.0,  0.0,  -1.0,  -2.0,  -3.0,  3.0,  6.0,  9.0
 ,  0.0,  0.0,  0.0,  -4.0,  -5.0,  -6.0, 12.0, 15.0, 18.0
 ,  0.0,  0.0,  0.0,  -7.0,  -8.0,  -9.0, 21.0, 24.0, 27.0
 ,  0.0,  0.0,  0.0, -10.0, -11.0, -12.0, 30.0, 33.0, 36.0 ]@
-}
kronecker :: (Field t) => Matrix t -> Matrix t -> Matrix t
kronecker a b = fromBlocks
              . splitEvery (cols a)
              . map (reshape (cols b))
              . toRows
              $ flatten a `outer` flatten b
