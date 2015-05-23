{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Numeric
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
{-# OPTIONS_HADDOCK hide #-}

module Data.Packed.Numeric (
    -- * Basic functions
    module Data.Packed,
    Konst(..), Build(..),
    linspace,
    diag, ident,
    ctrans,
    -- * Generic operations
    Container(..), Numeric,
    -- add, mul, sub, divide, equal, scaleRecip, addConstant,
    scalar, conj, scale, arctan2, cmap,
    atIndex, minIndex, maxIndex, minElement, maxElement,
    sumElements, prodElements,
    step, cond, find, assoc, accum,
    Transposable(..), Linear(..),
    -- * Matrix product
    Product(..), udot, dot, (<·>), (#>), (<#), app,
    Mul(..),
    (<.>),
    optimiseMult,
    mXm,mXv,vXm,LSDiv,(<\>),
    outer, kronecker,
    -- * Random numbers
    RandDist(..),
    randomVector,
    gaussianSample,
    uniformSample,
    meanCov,
    -- * sorting
    sortVector,
    -- * Element conversion
    Convert(..),
    Complexable(),
    RealElement(),
    RealOf, ComplexOf, SingleOf, DoubleOf,
    roundVector,
    IndexOf,
    module Data.Complex,
    -- * IO
    module Data.Packed.IO,
    -- * Misc
    Testable(..)
) where

import Data.Packed
import Data.Packed.Internal.Numeric
import Data.Complex
import Numeric.LinearAlgebra.Algorithms(Field,linearSolveSVD)
import Data.Packed.IO
import Numeric.LinearAlgebra.Random

------------------------------------------------------------------

{- | Creates a real vector containing a range of values:

>>> linspace 5 (-3,7::Double)
fromList [-3.0,-0.5,2.0,4.5,7.0]@

>>> linspace 5 (8,2+i) :: Vector (Complex Double)
fromList [8.0 :+ 0.0,6.5 :+ 0.25,5.0 :+ 0.5,3.5 :+ 0.75,2.0 :+ 1.0]

Logarithmic spacing can be defined as follows:

@logspace n (a,b) = 10 ** linspace n (a,b)@
-}
linspace :: (Fractional e, Container Vector e) => Int -> (e, e) -> Vector e
linspace 0 _     = fromList[]
linspace 1 (a,b) = fromList[(a+b)/2]
linspace n (a,b) = addConstant a $ scale s $ fromList $ map fromIntegral [0 .. n-1]
    where s = (b-a)/fromIntegral (n-1)

--------------------------------------------------------------------------------

infixl 7 <.>
-- | An infix synonym for 'dot'
(<.>) :: Numeric t => Vector t -> Vector t -> t
(<.>) = dot


infixr 8 <·>, #>

{- | infix synonym for 'dot'

>>> vector [1,2,3,4] <·> vector [-2,0,1,1]
5.0

>>> let 𝑖 = 0:+1 :: ℂ
>>> fromList [1+𝑖,1] <·> fromList [1,1+𝑖]
2.0 :+ 0.0

(the dot symbol "·" is obtained by Alt-Gr .)

-}
(<·>) :: Numeric t => Vector t -> Vector t -> t
(<·>) = dot


{- | infix synonym for 'app'

>>> let m = (2><3) [1..]
>>> m
(2><3)
 [ 1.0, 2.0, 3.0
 , 4.0, 5.0, 6.0 ]

>>> let v = vector [10,20,30]

>>> m #> v
fromList [140.0,320.0]

-}
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
fromList [3.0799999999999996,5.159999999999999]

>>> a #> x
fromList [13.399999999999999,26.799999999999997,1.0]

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

class Konst e d c | d -> c, c -> d
  where
    -- |
    -- >>> konst 7 3 :: Vector Float
    -- fromList [7.0,7.0,7.0]
    --
    -- >>> konst i (3::Int,4::Int)
    -- (3><4)
    --  [ 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0
    --  , 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0
    --  , 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0, 0.0 :+ 1.0 ]
    --
    konst :: e -> d -> c e

instance Container Vector e => Konst e Int Vector
  where
    konst = konst'

instance (Num e, Container Vector e) => Konst e (Int,Int) Matrix
  where
    konst = konst'

--------------------------------------------------------------------------------

class Build d f c e | d -> c, c -> d, f -> e, f -> d, f -> c, c e -> f, d e -> f
  where
    -- |
    -- >>> build 5 (**2) :: Vector Double
    -- fromList [0.0,1.0,4.0,9.0,16.0]
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

instance Container Matrix e => Build (Int,Int) (e -> e -> e) Matrix e
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

>>> meanCov $ gaussianSample 666 1000 (fromList[4,5]) (diagl[2,3])
(fromList [4.010341078059521,5.0197204699640405],
(2><2)
 [     1.9862461923890056, -1.0127225830525157e-2
 , -1.0127225830525157e-2,     3.0373954915729318 ])

-}
meanCov :: Matrix Double -> (Vector Double, Matrix Double)
meanCov x = (med,cov) where
    r    = rows x
    k    = 1 / fromIntegral r
    med  = konst k r `vXm` x
    meds = konst 1 r `outer` med
    xc   = x `sub` meds
    cov  = scale (recip (fromIntegral (r-1))) (trans xc `mXm` xc)

--------------------------------------------------------------------------------

class ( Container Vector t
      , Container Matrix t
      , Konst t Int Vector
      , Konst t (Int,Int) Matrix
      , Product t
      ) => Numeric t

instance Numeric Double
instance Numeric (Complex Double)
instance Numeric Float
instance Numeric (Complex Float)
instance Numeric CInt


