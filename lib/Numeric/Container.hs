{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Container
-- Copyright   :  (c) Alberto Ruiz 2010-14
-- License     :  GPL
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
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

module Numeric.Container (
    -- * Basic functions
    module Data.Packed,
    konst, build,
    constant, linspace,
    diag, ident,
    ctrans,
    -- * Generic operations
    Container(..),
    -- * Matrix product
    Product(..), udot,
    Mul(..),
    Contraction(..), mmul,
    optimiseMult,
    mXm,mXv,vXm,LSDiv(..), cdot, (·), dot, (<.>),
    outer, kronecker,
    -- * Random numbers
    RandDist(..),
    randomVector,
    gaussianSample,
    uniformSample,
    meanCov,
    -- * Element conversion
    Convert(..),
    Complexable(),
    RealElement(),

    RealOf, ComplexOf, SingleOf, DoubleOf,

    IndexOf,
    module Data.Complex,
    -- * IO
    dispf, disps, dispcf, vecdisp, latexFormat, format,
    loadMatrix, saveMatrix, fromFile, fileDimensions,
    readMatrix,
    fscanfVector, fprintfVector, freadVector, fwriteVector,
) where

import Data.Packed
import Data.Packed.Internal(constantD)
import Numeric.ContainerBoot
import Numeric.Chain
import Numeric.IO
import Data.Complex
import Numeric.LinearAlgebra.Algorithms(Field,linearSolveSVD)
import Data.Packed.Random

------------------------------------------------------------------

{- | creates a vector with a given number of equal components:

@> constant 2 7
7 |> [2.0,2.0,2.0,2.0,2.0,2.0,2.0]@
-}
constant :: Element a => a -> Int -> Vector a
-- constant x n = runSTVector (newVector x n)
constant = constantD-- about 2x faster

{- | Creates a real vector containing a range of values:

>>> linspace 5 (-3,7::Double)
fromList [-3.0,-0.5,2.0,4.5,7.0]@

>>> linspace 5 (8,2+i) :: Vector (Complex Double)
fromList [8.0 :+ 0.0,6.5 :+ 0.25,5.0 :+ 0.5,3.5 :+ 0.75,2.0 :+ 1.0]

Logarithmic spacing can be defined as follows:

@logspace n (a,b) = 10 ** linspace n (a,b)@
-}
linspace :: (Container Vector e) => Int -> (e, e) -> Vector e
linspace 0 (a,b) = fromList[(a+b)/2]
linspace n (a,b) = addConstant a $ scale s $ fromList $ map fromIntegral [0 .. n-1]
    where s = (b-a)/fromIntegral (n-1)

-- | dot product: @cdot u v = 'udot' ('conj' u) v@
cdot :: (Container Vector t, Product t) => Vector t -> Vector t -> t
cdot u v = udot (conj u) v

--------------------------------------------------------

class Contraction a b c | a b -> c, c -> a b
  where
    infixr 7 ×
    {- | Matrix-matrix product, matrix-vector product, and unconjugated dot product

(unicode 0x00d7, multiplication sign)

Examples:

>>> let a = (3><4)   [1..]      :: Matrix Double
>>> let v = fromList [1,0,2,-1] :: Vector Double
>>> let u = fromList [1,2,3]    :: Vector Double

>>> a
(3><4)
 [ 1.0,  2.0,  3.0,  4.0
 , 5.0,  6.0,  7.0,  8.0
 , 9.0, 10.0, 11.0, 12.0 ]

matrix × matrix:

>>> disp 2 (a × trans a)
3x3
 30   70  110
 70  174  278
110  278  446

matrix × vector:

>>> a × v
fromList [3.0,11.0,19.0]

unconjugated dot product:

>>> fromList [1,i] × fromList[2*i+1,3]
1.0 :+ 5.0

(×) is right associative, so we can write:

>>> u × a × v
82.0 :: Double

-}
    (×) :: a -> b -> c

instance Product t => Contraction (Matrix t) (Vector t) (Vector t) where
    (×) = mXv

instance Product t => Contraction (Matrix t) (Matrix t) (Matrix t) where
    (×) = mXm

instance Contraction (Vector Double) (Vector Double) Double where
    (×) = udot

instance Contraction (Vector Float) (Vector Float) Float where
    (×) = udot

instance Contraction (Vector (Complex Double)) (Vector (Complex Double)) (Complex Double) where
    (×) = udot

instance Contraction (Vector (Complex Float)) (Vector (Complex Float)) (Complex Float) where
    (×) = udot


-- | alternative function for the matrix product (×)
mmul :: Contraction a b c => a -> b -> c
mmul = (×)

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

class LSDiv c where
 infixl 7 <\>
 -- | least squares solution of a linear system, similar to the \\ operator of Matlab\/Octave (based on linearSolveSVD)
 (<\>)  :: Field t => Matrix t -> c t -> c t

instance LSDiv Vector where
    m <\> v = flatten (linearSolveSVD m (reshape 1 v))

instance LSDiv Matrix where
    (<\>) = linearSolveSVD

--------------------------------------------------------

{- | Dot product : @u · v = 'cdot' u v@

 (unicode 0x00b7, middle dot, Alt-Gr .)

>>> fromList [1,i] · fromList[2*i+1,3]
1.0 :+ (-1.0)

-}
(·) :: (Container Vector t, Product t) => Vector t -> Vector t -> t
infixl 7 ·
u · v = cdot u v

--------------------------------------------------------------------------------

-- bidirectional type inference
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

instance Container Vector e => Konst e (Int,Int) Matrix
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

{-# DEPRECATED dot "use udot" #-}
dot :: Product e => Vector e -> Vector e -> e
dot = udot

{-# DEPRECATED (<.>) "use udot or (×)" #-}
infixl 7 <.>
(<.>) :: Product e => Vector e -> Vector e -> e
(<.>) = udot

