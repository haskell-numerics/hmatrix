{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Internal.Container
-- Copyright   :  (c) Alberto Ruiz 2010-14
-- License     :  BSD3
-- Maintainer  :  Alberto Ruiz
-- Stability   :  provisional
--
-- Basic numeric operations on 'Vector' and 'Matrix', including conversion routines.
--
-- The 'Container' class is used to define optimized generic functions which work
-- on 'Vector' and 'Matrix' with real or complex elements.
--
-- Some of these functions are also available in the instances of the standard
-- numeric Haskell classes provided by "Numeric.LinearAlgebra".
--
-----------------------------------------------------------------------------

module Internal.Container where

import Internal.Vector
import Internal.Matrix
import Internal.Element
import Internal.Numeric
import Internal.Algorithms(Field,linearSolveSVD,Herm,mTm)
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif
------------------------------------------------------------------

{- | Creates a real vector containing a range of values:

>>> linspace 5 (-3,7::Double)
[-3.0,-0.5,2.0,4.5,7.0]
it :: Vector Double

>>> linspace 5 (8,3:+2) :: Vector (Complex Double)
[8.0 :+ 0.0,6.75 :+ 0.5,5.5 :+ 1.0,4.25 :+ 1.5,3.0 :+ 2.0]
it :: Vector (Complex Double)

Logarithmic spacing can be defined as follows:

@logspace n (a,b) = 10 ** linspace n (a,b)@
-}
linspace :: (Fractional e, Container Vector e) => Int -> (e, e) -> Vector e
linspace 0 _     = fromList[]
linspace 1 (a,b) = fromList[(a+b)/2]
linspace n (a,b) = addConstant a $ scale s $ fromList $ map fromIntegral [0 .. n-1]
    where s = (b-a)/fromIntegral (n-1)

--------------------------------------------------------------------------------

infixr 8 <.>
{- | An infix synonym for 'dot'

>>> vector [1,2,3,4] <.> vector [-2,0,1,1]
5.0

>>> let 𝑖 = 0:+1 :: C
>>> fromList [1+𝑖,1] <.> fromList [1,1+𝑖]
2.0 :+ 0.0

-}

(<.>) :: Numeric t => Vector t -> Vector t -> t
(<.>) = dot





{- | dense matrix-vector product

>>> let m = (2><3) [1..]
>>> m
(2><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0 ]

>>> let v = vector [10,20,30]

>>> m #> v
[140.0,320.0]
it :: Vector Numeric.LinearAlgebra.Data.R

-}
infixr 8 #>
(#>) :: Numeric t => Matrix t -> Vector t -> Vector t
(#>) = mXv

-- | dense matrix-vector product
app :: Numeric t => Matrix t -> Vector t -> Vector t
app = (#>)

infixl 8 <#
-- | dense vector-matrix product
(<#) :: Numeric t => Vector t -> Matrix t -> Vector t
(<#) = vXm

--------------------------------------------------------------------------------

class Mul a b c | a b -> c where
 infixl 7 <>
 -- | Matrix-matrix, matrix-vector, and vector-matrix products.
 (<>)  :: Product t => a t -> b t -> c t

instance Mul Matrix Matrix Matrix where
    (<>) = mXm

instance Mul Matrix Vector Vector where
    (<>) m v = flatten $ m <> asColumn v

instance Mul Vector Matrix Vector where
    (<>) v m = flatten $ asRow v <> m

--------------------------------------------------------------------------------

{- | Least squares solution of a linear system, similar to the \\ operator of Matlab\/Octave (based on linearSolveSVD)

@
a = (3><2)
 [ 1.0,  2.0
 , 2.0,  4.0
 , 2.0, -1.0 ]
@

@
v = vector [13.0,27.0,1.0]
@

>>> let x = a <\> v
>>> x
[3.0799999999999996,5.159999999999999]
it :: Vector Numeric.LinearAlgebra.Data.R

>>> a #> x
[13.399999999999999,26.799999999999997,0.9999999999999991]
it :: Vector Numeric.LinearAlgebra.Data.R

It also admits multiple right-hand sides stored as columns in a matrix.

-}
infixl 7 <\>
(<\>) :: (LSDiv c, Field t) => Matrix t -> c t -> c t
(<\>) = linSolve

class LSDiv c
  where
    linSolve :: Field t => Matrix t -> c t -> c t

instance LSDiv Vector
  where
    linSolve m v = flatten (linearSolveSVD m (reshape 1 v))

instance LSDiv Matrix
  where
    linSolve = linearSolveSVD

--------------------------------------------------------------------------------


class Build d f c e | d -> c, c -> d, f -> e, f -> d, f -> c, c e -> f, d e -> f
  where
    -- |
    -- >>> build 5 (**2) :: Vector Double
    -- [0.0,1.0,4.0,9.0,16.0]
    -- it :: Vector Double
    --
    -- Hilbert matrix of order N:
    --
    -- >>> let hilb n = build (n,n) (\i j -> 1/(i+j+1)) :: Matrix Double
    -- >>> putStr . dispf 2 $ hilb 3
    -- 3x3
    -- 1.00  0.50  0.33
    -- 0.50  0.33  0.25
    -- 0.33  0.25  0.20
    --
    build :: d -> f -> c e

instance Container Vector e => Build Int (e -> e) Vector e
  where
    build = build'

instance (Num e, Container Vector e) => Build (Int,Int) (e -> e -> e) Matrix e
  where
    build = build'

--------------------------------------------------------------------------------

-- @dot u v = 'udot' ('conj' u) v@
dot :: (Numeric t) => Vector t -> Vector t -> t
dot u v = udot (conj u) v

--------------------------------------------------------------------------------

optimiseMult :: Monoid (Matrix t) => [Matrix t] -> Matrix t
optimiseMult = mconcat

--------------------------------------------------------------------------------


{- | Compute mean vector and covariance matrix of the rows of a matrix.

>>> meanCov $ gaussianSample 666 1000 (fromList[4,5]) (trustSym $ diagl [2,3])
([3.9933155655086696,5.061409102770331],Herm (2><2)
 [    1.9963242906624408, -4.227815571404954e-2
 , -4.227815571404954e-2,    3.2003833097832857 ])
it :: (Vector Double, Herm Double)
-}
meanCov :: Matrix Double -> (Vector Double, Herm Double)
meanCov x = (med,cov) where
    r    = rows x
    k    = 1 / fromIntegral r
    med  = konst k r `vXm` x
    meds = konst 1 r `outer` med
    xc   = x `sub` meds
    cov  = scale (recip (fromIntegral (r-1))) (mTm xc)

--------------------------------------------------------------------------------

sortVector :: (Ord t, Element t) => Vector t -> Vector t
sortVector = sortV

{- |

>>> m <- randn 4 10
>>> disp 2 m
4x10
-0.31   0.41   0.43  -0.19  -0.17  -0.23  -0.17  -1.04  -0.07  -1.24
 0.26   0.19   0.14   0.83  -1.54  -0.09   0.37  -0.63   0.71  -0.50
-0.11  -0.10  -1.29  -1.40  -1.04  -0.89  -0.68   0.35  -1.46   1.86
 1.04  -0.29   0.19  -0.75  -2.20  -0.01   1.06   0.11  -2.09  -1.58

>>> disp 2 $ m ?? (All, Pos $ sortIndex (m!1))
4x10
-0.17  -1.04  -1.24  -0.23   0.43   0.41  -0.31  -0.17  -0.07  -0.19
-1.54  -0.63  -0.50  -0.09   0.14   0.19   0.26   0.37   0.71   0.83
-1.04   0.35   1.86  -0.89  -1.29  -0.10  -0.11  -0.68  -1.46  -1.40
-2.20   0.11  -1.58  -0.01   0.19  -0.29   1.04   1.06  -2.09  -0.75

-}
sortIndex :: (Ord t, Element t) => Vector t -> Vector I
sortIndex = sortI

ccompare :: (Ord t, Container c t) => c t -> c t -> c I
ccompare = ccompare'

cselect :: (Container c t) => c I -> c t -> c t -> c t -> c t
cselect = cselect'

{- | Extract elements from positions given in matrices of rows and columns.

>>> r
(3><3)
 [ 1, 1, 1
 , 1, 2, 2
 , 1, 2, 3 ]
>>> c
(3><3)
 [ 0, 1, 5
 , 2, 2, 1
 , 4, 4, 1 ]
>>> m
(4><6)
 [  0,  1,  2,  3,  4,  5
 ,  6,  7,  8,  9, 10, 11
 , 12, 13, 14, 15, 16, 17
 , 18, 19, 20, 21, 22, 23 ]

>>> remap r c m
(3><3)
 [  6,  7, 11
 ,  8, 14, 13
 , 10, 16, 19 ]

The indexes are autoconformable.

>>> c'
(3><1)
 [ 1
 , 2
 , 4 ]
>>> remap r c' m
(3><3)
 [  7,  7,  7
 ,  8, 14, 14
 , 10, 16, 22 ]

-}
remap :: Element t => Matrix I -> Matrix I -> Matrix t -> Matrix t
remap i j m
    | minElement i >= 0 && maxElement i < fromIntegral (rows m) &&
      minElement j >= 0 && maxElement j < fromIntegral (cols m) = remapM i' j' m
    | otherwise = error $ "out of range index in remap"
  where
    [i',j'] = conformMs [i,j]
