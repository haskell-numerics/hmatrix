{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  LinearAlgebra.Algorithms
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi


-}
-----------------------------------------------------------------------------

module LinearAlgebra.Algorithms (
    GMatrix(..),
    Normed(..), NormType(..),
    det,inv,pinv,full,economy,
    pinvTol,
--    pinvTolg,
    nullspacePrec,
    nullVector,
    eps, i
) where


import Data.Packed.Internal
import Data.Packed.Matrix
import GSL.Matrix
import GSL.Vector
import LAPACK
import Complex
import LinearAlgebra.Linear

class (Linear Matrix t) => GMatrix t where
    svd         :: Matrix t -> (Matrix t, Vector Double, Matrix t)
    lu          :: Matrix t -> (Matrix t, Matrix t, [Int], t)
    linearSolve :: Matrix t -> Matrix t -> Matrix t
    linearSolveSVD :: Matrix t -> Matrix t -> Matrix t
    ctrans :: Matrix t -> Matrix t
    eig         :: Matrix t -> (Vector (Complex Double), Matrix (Complex Double))
    eigSH       :: Matrix t -> (Vector Double, Matrix t)

instance GMatrix Double where
    svd = svdR
    lu  = luR
    linearSolve = linearSolveR
    linearSolveSVD = linearSolveSVDR Nothing
    ctrans = trans
    eig = eigR
    eigSH = eigS

instance GMatrix (Complex Double) where
    svd = svdC
    lu  = luC
    linearSolve = linearSolveC
    linearSolveSVD = linearSolveSVDC Nothing
    ctrans = conjTrans
    eig = eigC
    eigSH = eigH

square m = rows m == cols m

det :: GMatrix t => Matrix t -> t
det m | square m = s * (product $ toList $ takeDiag $ u)
      | otherwise = error "det of nonsquare matrix"
    where (_,u,_,s) = lu m

inv :: GMatrix t => Matrix t -> Matrix t
inv m | square m = m `linearSolve` ident (rows m)
      | otherwise = error "inv of nonsquare matrix"

pinv :: GMatrix t => Matrix t -> Matrix t
pinv m = linearSolveSVD m (ident (rows m))


full svd m = (u, d ,v) where
    (u,s,v) = svd m
    d = diagRect s r c
    r = rows m
    c = cols m

economy svd m = (u', subVector 0 d s, v') where
    (u,s,v) = svd m
    sl@(g:_) = toList (complex s)
    s' = fromList . filter rec $ sl
    rec x = magnitude x > magnitude g*tol
    t = 1
    tol = (fromIntegral (max (rows m) (cols m)) * magnitude g * t * eps)
    r = rows m
    c = cols m
    d = dim s'
    u' = takeColumns d u
    v' = takeColumns d v


{- | Machine precision of a Double.

>> eps
> 2.22044604925031e-16

(The value used by GNU-Octave)

-}
eps :: Double
eps =  2.22044604925031e-16

{- | The imaginary unit

@> 'ident' 3 \<\> i
1.i   0.   0.
 0.  1.i   0.
 0.   0.  1.i@

-}
i :: Complex Double
i = 0:+1


-- | matrix product
mXm :: (Num t, Field t) => Matrix t -> Matrix t -> Matrix t
mXm = multiply

-- | matrix - vector product
mXv :: (Num t, Field t) => Matrix t -> Vector t -> Vector t
mXv m v = flatten $ m `mXm` (asColumn v)

-- | vector - matrix product
vXm :: (Num t, Field t) => Vector t -> Matrix t -> Vector t
vXm v m = flatten $ (asRow v) `mXm` m


---------------------------------------------------------------------------

norm2 :: Vector Double -> Double
norm2 = toScalarR Norm2

norm1 :: Vector Double -> Double
norm1 = toScalarR AbsSum

vectorMax :: Vector Double -> Double
vectorMax = toScalarR Max
vectorMin :: Vector Double -> Double
vectorMin = toScalarR Min
vectorMaxIndex :: Vector Double -> Int
vectorMaxIndex = round . toScalarR MaxIdx
vectorMinIndex :: Vector Double -> Int
vectorMinIndex = round . toScalarR MinIdx

data NormType = Infinity | PNorm1 | PNorm2 -- PNorm Int

pnormRV PNorm2 = norm2
pnormRV PNorm1 = norm1
pnormRV Infinity = vectorMax . vectorMapR Abs
--pnormRV _ = error "pnormRV not yet defined"

pnormCV PNorm2 = norm2 . asReal
pnormCV PNorm1 = norm1 . liftVector magnitude
pnormCV Infinity = vectorMax . liftVector magnitude
--pnormCV _ = error "pnormCV not yet defined"

pnormRM PNorm2 m = head (toList s) where (_,s,_) = svdR m
pnormRM PNorm1 m = vectorMax $ constant 1 (rows m) `vXm` liftMatrix (vectorMapR Abs) m
pnormRM Infinity m = vectorMax $ liftMatrix (vectorMapR Abs) m `mXv` constant 1 (cols m)
--pnormRM _ _ = error "p norm not yet defined"

pnormCM PNorm2 m = head (toList s) where (_,s,_) = svdC m
pnormCM PNorm1 m = vectorMax $ constant 1 (rows m) `vXm` liftMatrix (liftVector magnitude) m
pnormCM Infinity m = vectorMax $ liftMatrix (liftVector magnitude) m `mXv` constant 1 (cols m)
--pnormCM _ _ = error "p norm not yet defined"

-- -- | computes the p-norm of a matrix or vector (with the same definitions as GNU-octave). pnorm 0 denotes \\inf-norm. See also 'norm'.
--pnorm :: (Container t, Field a) => Int -> t a -> Double
--pnorm = pnormG

class Normed t where
    pnorm :: NormType -> t -> Double
    norm :: t -> Double
    norm = pnorm PNorm2

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
nullspacePrec :: GMatrix t
              => Double          -- ^ relative tolerance in 'eps' units
              -> Matrix t   -- ^ input matrix
              -> [Vector t] -- ^ list of unitary vectors spanning the nullspace
nullspacePrec t m = ns where
    (_,s,v) = svd m
    sl@(g:_) = toList s
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)
    rank = length (filter (> g*tol) sl)
--    ns = drop rank (toColumns v)
    ns = drop rank $ toRows $ ctrans v

-- | The nullspace of a matrix, assumed to be one-dimensional, with default tolerance (shortcut for @last . nullspacePrec 1@).
nullVector :: GMatrix t => Matrix t -> Vector t
nullVector = last . nullspacePrec 1

------------------------------------------------------------------------

{- | Pseudoinverse of a real matrix with the desired tolerance, expressed as a
multiplicative factor of the default tolerance used by GNU-Octave (see 'pinv').

@\> let m = 'fromLists' [[1,0,    0]
                    ,[0,1,    0]
                    ,[0,0,1e-10]]
\ 
\> 'pinv' m 
1. 0.           0.
0. 1.           0.
0. 0. 10000000000.
\ 
\> pinvTol 1E8 m
1. 0. 0.
0. 1. 0.
0. 0. 1.@

-}
pinvTol :: Double -> Matrix Double -> Matrix Double
pinvTol t m = v' `mXm` diag s' `mXm` trans u' where
    (u,s,v) = svdR m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x < g*tol then 1 else 1/x
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)
    r = rows m
    c = cols m
    d = dim s
    u' = takeColumns d u
    v' = takeColumns d v
