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
    mXm, mXv, vXm,
    inv,
    pinv,
    pinvTol,
    pinvTolg,
    Normed(..), NormType(..),
    det,
    eps, i
) where


import Data.Packed.Internal
import Data.Packed.Matrix
import GSL.Matrix
import GSL.Vector
import LAPACK
import Complex

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
mXm = multiply RowMajor

-- | matrix - vector product
mXv :: (Num t, Field t) => Matrix t -> Vector t -> Vector t
mXv m v = flatten $ m `mXm` (asColumn v)

-- | vector - matrix product
vXm :: (Num t, Field t) => Vector t -> Matrix t -> Vector t
vXm v m = flatten $ (asRow v) `mXm` m



-- | Pseudoinverse of a real matrix
--
-- @dispR 3 $ pinv (fromLists [[1,2],
--                           [3,4],
--                           [5,6]])
--matrix (2x3)
-- -1.333 | -0.333 |  0.667
--  1.083 |  0.333 | -0.417@
--

pinv :: Matrix Double -> Matrix Double
pinv m = pinvTol 1 m
--pinv m = linearSolveSVDR Nothing m (ident (rows m))

{- -| Pseudoinverse of a real matrix with the default tolerance used by GNU-Octave: the singular values less than max (rows, colums) * greatest singular value * 'eps' are ignored. See 'pinvTol'.

@\> let m = 'fromLists' [[ 1, 2]
                    ,[ 5, 8]
                    ,[10,-5]]
\> pinv m
9.353e-3 4.539e-2  7.637e-2
2.231e-2 8.993e-2 -4.719e-2
\ 
\> m \<\> pinv m \<\> m
 1.  2.
 5.  8.
10. -5.@

-}
--pinvg :: Matrix Double -> Matrix Double
pinvg m = pinvTolg 1 m

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
    (u,s,v) = svdR' m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x < g*tol then 1 else 1/x
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)
    r = rows m
    c = cols m
    d = dim s
    u' = takeColumns d u
    v' = takeColumns d v


pinvTolg :: Double -> Matrix Double -> Matrix Double
pinvTolg t m = v `mXm` diag s' `mXm` trans u where
    (u,s,v) = svdg m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x < g*tol then 1 else 1/x
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)



{- | Inverse of a square matrix.

inv m = 'linearSolveR' m ('ident' ('rows' m))

@\>inv ('fromLists' [[1,4]
                ,[0,2]])
1.   -2.
0. 0.500@
-}
inv :: Matrix Double -> Matrix Double
inv m = if rows m == cols m
    then m `linearSolveR` ident (rows m)
    else error "inv of nonsquare matrix"


{- - | Shortcut for the 2-norm ('pnorm' 2)

@ > norm $ 'hilb' 5
1.5670506910982311
@

@\> norm $ 'fromList' [1,-1,'i',-'i']
2.0@

-}



{- | Determinant of a square matrix, computed from the LU decomposition.

@\> det ('fromLists' [[7,2],[3,8]])
50.0@

-}
det :: Matrix Double -> Double
det m = s * (product $ toList $ takeDiag $ u)
    where (_,u,_,s) = luR m

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

pnormRM PNorm2 m = head (toList s) where (_,s,_) = svdR' m
pnormRM PNorm1 m = vectorMax $ constant 1 (rows m) `vXm` liftMatrix (vectorMapR Abs) m
pnormRM Infinity m = vectorMax $ liftMatrix (vectorMapR Abs) m `mXv` constant 1 (cols m)
--pnormRM _ _ = error "p norm not yet defined"

pnormCM PNorm2 m = head (toList s) where (_,s,_) = svdC' m
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
