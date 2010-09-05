{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Linear
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic optimized operations on vectors and matrices.

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Linear (
    -- * Linear Algebra Typeclasses
    Vectors(..),
    Linear(..),
    -- * Products
    Product(..),
    mXm,mXv,vXm,
    outer, kronecker,
    -- * Creation of numeric vectors
    constant, linspace
) where

import Data.Packed.Internal
import Data.Packed.Matrix
import Data.Complex
import Numeric.GSL.Vector
import Numeric.LinearAlgebra.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ)

import Control.Monad(ap)

-- | basic Vector functions
class Num e => Vectors a e where
    -- the C functions sumX are twice as fast as using foldVector
    vectorSum  :: a e -> e
    vectorProd :: a e -> e
    absSum     :: a e -> e
    vectorMin  :: a e -> e
    vectorMax  :: a e -> e
    minIdx     :: a e -> Int
    maxIdx     :: a e -> Int
    dot        :: a e -> a e -> e
    norm1      :: a e -> e
    norm2      :: a e -> e
    normInf    :: a e -> e


instance Vectors Vector Float where
    vectorSum  = sumF
    vectorProd = prodF
    norm2      = toScalarF Norm2
    absSum     = toScalarF AbsSum
    vectorMin  = toScalarF Min
    vectorMax  = toScalarF Max
    minIdx     = round . toScalarF MinIdx
    maxIdx     = round . toScalarF MaxIdx
    dot        = dotF
    norm1      = toScalarF AbsSum
    normInf    = vectorMax . vectorMapF Abs

instance Vectors Vector Double where
    vectorSum  = sumR
    vectorProd = prodR
    norm2      = toScalarR Norm2
    absSum     = toScalarR AbsSum
    vectorMin  = toScalarR Min
    vectorMax  = toScalarR Max
    minIdx     = round . toScalarR MinIdx
    maxIdx     = round . toScalarR MaxIdx
    dot        = dotR
    norm1      = toScalarR AbsSum
    normInf    = vectorMax . vectorMapR Abs

instance Vectors Vector (Complex Float) where
    vectorSum  = sumQ
    vectorProd = prodQ
    norm2      = (:+ 0) . toScalarQ Norm2
    absSum     = (:+ 0) . toScalarQ AbsSum
    vectorMin  = ap (@>) minIdx
    vectorMax  = ap (@>) maxIdx
    minIdx     = minIdx . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    maxIdx     = maxIdx . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    dot        = dotQ
    norm1      = (:+ 0) . vectorSum . fst . fromComplex . vectorMapQ Abs
    normInf    = (:+ 0) . vectorMax . fst . fromComplex . vectorMapQ Abs

instance Vectors Vector (Complex Double) where
    vectorSum  = sumC
    vectorProd = prodC
    norm2      = (:+ 0) . toScalarC Norm2
    absSum     = (:+ 0) . toScalarC AbsSum
    vectorMin  = ap (@>) minIdx
    vectorMax  = ap (@>) maxIdx
    minIdx     = minIdx . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    maxIdx     = maxIdx . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    dot        = dotC
    norm1      = (:+ 0) . vectorSum . fst . fromComplex . vectorMapC Abs
    normInf    = (:+ 0) . vectorMax . fst . fromComplex . vectorMapC Abs

----------------------------------------------------

-- | Basic element-by-element functions.
class (Element e, Container c) => Linear c e where
    -- | create a structure with a single element
    scalar      :: e -> c e
    scale       :: e -> c e -> c e
    -- | scale the element by element reciprocal of the object:
    --
    -- @scaleRecip 2 (fromList [5,i]) == 2 |> [0.4 :+ 0.0,0.0 :+ (-2.0)]@
    scaleRecip  :: e -> c e -> c e
    addConstant :: e -> c e -> c e
    add         :: c e -> c e -> c e
    sub         :: c e -> c e -> c e
    -- | element by element multiplication
    mul         :: c e -> c e -> c e
    -- | element by element division
    divide      :: c e -> c e -> c e
    equal       :: c e -> c e -> Bool


instance Linear Vector Float where
    scale = vectorMapValF Scale
    scaleRecip = vectorMapValF Recip
    addConstant = vectorMapValF AddConstant
    add = vectorZipF Add
    sub = vectorZipF Sub
    mul = vectorZipF Mul
    divide = vectorZipF Div
    equal u v = dim u == dim v && vectorMax (vectorMapF Abs (sub u v)) == 0.0
    scalar x = fromList [x]

instance Linear Vector Double where
    scale = vectorMapValR Scale
    scaleRecip = vectorMapValR Recip
    addConstant = vectorMapValR AddConstant
    add = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul
    divide = vectorZipR Div
    equal u v = dim u == dim v && vectorMax (vectorMapR Abs (sub u v)) == 0.0
    scalar x = fromList [x]

instance Linear Vector (Complex Double) where
    scale = vectorMapValC Scale
    scaleRecip = vectorMapValC Recip
    addConstant = vectorMapValC AddConstant
    add = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul
    divide = vectorZipC Div
    equal u v = dim u == dim v && vectorMax (mapVector magnitude (sub u v)) == 0.0
    scalar x = fromList [x]

instance Linear Vector (Complex Float) where
    scale = vectorMapValQ Scale
    scaleRecip = vectorMapValQ Recip
    addConstant = vectorMapValQ AddConstant
    add = vectorZipQ Add
    sub = vectorZipQ Sub
    mul = vectorZipQ Mul
    divide = vectorZipQ Div
    equal u v = dim u == dim v && vectorMax (mapVector magnitude (sub u v)) == 0.0
    scalar x = fromList [x]

instance (Linear Vector a, Container Matrix) => (Linear Matrix a) where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    equal a b = cols a == cols b && flatten a `equal` flatten b
    scalar x = (1><1) [x]


----------------------------------------------------

{- | creates a vector with a given number of equal components:

@> constant 2 7
7 |> [2.0,2.0,2.0,2.0,2.0,2.0,2.0]@
-}
constant :: Element a => a -> Int -> Vector a
-- constant x n = runSTVector (newVector x n)
constant = constantD -- about 2x faster

{- | Creates a real vector containing a range of values:

@\> linspace 5 (-3,7)
5 |> [-3.0,-0.5,2.0,4.5,7.0]@

Logarithmic spacing can be defined as follows:

@logspace n (a,b) = 10 ** linspace n (a,b)@
-}
linspace :: (Enum e, Linear Vector e) => Int -> (e, e) -> Vector e
linspace n (a,b) = addConstant a $ scale s $ fromList [0 .. fromIntegral n-1]
    where s = (b-a)/fromIntegral (n-1)

----------------------------------------------------

class Element t => Product t where
    multiply :: Matrix t -> Matrix t -> Matrix t
    ctrans :: Matrix t -> Matrix t

instance Product Double where
    multiply = multiplyR
    ctrans = trans

instance Product (Complex Double) where
    multiply = multiplyC
    ctrans = conj . trans

instance Product Float where
    multiply = multiplyF
    ctrans = trans

instance Product (Complex Float) where
    multiply = multiplyQ
    ctrans = conj . trans

----------------------------------------------------------

-- synonym for matrix product
mXm :: Product t => Matrix t -> Matrix t -> Matrix t
mXm = multiply

-- matrix - vector product
mXv :: Product t => Matrix t -> Vector t -> Vector t
mXv m v = flatten $ m `mXm` (asColumn v)

-- vector - matrix product
vXm :: Product t => Vector t -> Matrix t -> Vector t
vXm v m = flatten $ (asRow v) `mXm` m

{- | Outer product of two vectors.

@\> 'fromList' [1,2,3] \`outer\` 'fromList' [5,2,3]
(3><3)
 [  5.0, 2.0, 3.0
 , 10.0, 4.0, 6.0
 , 15.0, 6.0, 9.0 ]@
-}
outer :: (Product t) => Vector t -> Vector t -> Matrix t
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
kronecker :: (Product t) => Matrix t -> Matrix t -> Matrix t
kronecker a b = fromBlocks
              . splitEvery (cols a)
              . map (reshape (cols b))
              . toRows
              $ flatten a `outer` flatten b

--------------------------------------------------
