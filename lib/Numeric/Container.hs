{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Container
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Numeric classes for containers of numbers, including conversion routines
--
-----------------------------------------------------------------------------

module Numeric.Container (
    --Linear(..),
    Container(..),
    Vectors(..),
    Product(..),
    mXm,mXv,vXm,
    outer, kronecker,

    RealElement, --Precision,
    ComplexContainer(toComplex,fromComplex,comp,conj),
    Convert(..), --AutoReal(..),
    RealOf, ComplexOf, SingleOf, DoubleOf,

    IndexOf,
    module Data.Complex
) where

import Data.Packed
import Numeric.Conversion
import Data.Packed.Internal
import Numeric.GSL.Vector

import Data.Complex
import Control.Monad(ap)
--import Control.Arrow((***))

import Numeric.LinearAlgebra.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ)

-------------------------------------------------------------------

type family IndexOf c

type instance IndexOf Vector = Int
type instance IndexOf Matrix = (Int,Int)

-------------------------------------------------------------------

-- | Basic element-by-element functions for numeric containers
class (Element e) => Container c e where

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

    -- | cannot implement instance Functor because of Element class constraint
    cmap        :: (Element a, Element b) => (a -> b) -> c a -> c b
    --
    -- | indexing function
    atIndex     :: c e -> IndexOf c -> e
    -- | index of min/max element
    minIndex    :: c e -> IndexOf c
    maxIndex    :: c e -> IndexOf c
    -- | value of min/max element
    minElement  :: c e -> e
    maxElement  :: c e -> e
    -- the C functions sumX/prodX are twice as fast as using foldVector
    -- | the sum/product of elements (faster than using @fold@
    sumElements :: c e -> e
    prodElements :: c e -> e

-- -- | Basic element-by-element functions.
-- class (Element e, Container c e) => Linear c e where


--------------------------------------------------------------------------

instance Container Vector Float where
    scale = vectorMapValF Scale
    scaleRecip = vectorMapValF Recip
    addConstant = vectorMapValF AddConstant
    add = vectorZipF Add
    sub = vectorZipF Sub
    mul = vectorZipF Mul
    divide = vectorZipF Div
    equal u v = dim u == dim v && maxElement (vectorMapF Abs (sub u v)) == 0.0
    scalar x = fromList [x]
    --
--instance Container Vector Float where
    cmap = mapVector
    atIndex = (@>)
    minIndex     = round . toScalarF MinIdx
    maxIndex     = round . toScalarF MaxIdx
    minElement  = toScalarF Min
    maxElement  = toScalarF Max
    sumElements  = sumF
    prodElements = prodF

instance Container Vector Double where
    scale = vectorMapValR Scale
    scaleRecip = vectorMapValR Recip
    addConstant = vectorMapValR AddConstant
    add = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul
    divide = vectorZipR Div
    equal u v = dim u == dim v && maxElement (vectorMapR Abs (sub u v)) == 0.0
    scalar x = fromList [x]
    --
--instance Container Vector Double where
    cmap = mapVector
    atIndex = (@>)
    minIndex     = round . toScalarR MinIdx
    maxIndex     = round . toScalarR MaxIdx
    minElement  = toScalarR Min
    maxElement  = toScalarR Max
    sumElements  = sumR
    prodElements = prodR

instance Container Vector (Complex Double) where
    scale = vectorMapValC Scale
    scaleRecip = vectorMapValC Recip
    addConstant = vectorMapValC AddConstant
    add = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul
    divide = vectorZipC Div
    equal u v = dim u == dim v && maxElement (mapVector magnitude (sub u v)) == 0.0
    scalar x = fromList [x]
    --
--instance Container Vector (Complex Double) where
    cmap = mapVector
    atIndex = (@>)
    minIndex     = minIndex . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    maxIndex     = maxIndex . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    minElement  = ap (@>) minIndex
    maxElement  = ap (@>) maxIndex
    sumElements  = sumC
    prodElements = prodC

instance Container Vector (Complex Float) where
    scale = vectorMapValQ Scale
    scaleRecip = vectorMapValQ Recip
    addConstant = vectorMapValQ AddConstant
    add = vectorZipQ Add
    sub = vectorZipQ Sub
    mul = vectorZipQ Mul
    divide = vectorZipQ Div
    equal u v = dim u == dim v && maxElement (mapVector magnitude (sub u v)) == 0.0
    scalar x = fromList [x]
    --
--instance Container Vector (Complex Float) where
    cmap = mapVector
    atIndex = (@>)
    minIndex     = minIndex . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    maxIndex     = maxIndex . fst . fromComplex . (zipVectorWith (*) `ap` mapVector conjugate)
    minElement  = ap (@>) minIndex
    maxElement  = ap (@>) maxIndex
    sumElements  = sumQ
    prodElements = prodQ

---------------------------------------------------------------

instance (Container Vector a) => Container Matrix a where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    equal a b = cols a == cols b && flatten a `equal` flatten b
    scalar x = (1><1) [x]
    --
--instance (Container Vector a) => Container Matrix a where
    cmap f = liftMatrix (mapVector f)
    atIndex = (@@>)
    minIndex m = let (r,c) = (rows m,cols m)
                     i = (minIndex $ flatten m)
                 in (i `div` c,(i `mod` c) + 1)
    maxIndex m = let (r,c) = (rows m,cols m)
                     i = (maxIndex $ flatten m)
                 in (i `div` c,(i `mod` c) + 1)
    minElement = ap (@@>) minIndex
    maxElement = ap (@@>) maxIndex
    sumElements = sumElements . flatten
    prodElements = prodElements . flatten

----------------------------------------------------


-- | Linear algebraic properties of objects
class Num e => Vectors a e where
    -- | dot (inner) product
    dot        :: a e -> a e -> e
    -- | sum of absolute value of elements (differs in complex case from @norm1@
    absSum     :: a e -> e
    -- | sum of absolute value of elements
    norm1      :: a e -> e
    -- | euclidean norm
    norm2      :: a e -> e
    -- | element of maximum magnitude
    normInf    :: a e -> e

instance Vectors Vector Float where
    norm2      = toScalarF Norm2
    absSum     = toScalarF AbsSum
    dot        = dotF
    norm1      = toScalarF AbsSum
    normInf    = maxElement . vectorMapF Abs

instance Vectors Vector Double where
    norm2      = toScalarR Norm2
    absSum     = toScalarR AbsSum
    dot        = dotR
    norm1      = toScalarR AbsSum
    normInf    = maxElement . vectorMapR Abs

instance Vectors Vector (Complex Float) where
    norm2      = (:+ 0) . toScalarQ Norm2
    absSum     = (:+ 0) . toScalarQ AbsSum
    dot        = dotQ
    norm1      = (:+ 0) . sumElements . fst . fromComplex . vectorMapQ Abs
    normInf    = (:+ 0) . maxElement . fst . fromComplex . vectorMapQ Abs

instance Vectors Vector (Complex Double) where
    norm2      = (:+ 0) . toScalarC Norm2
    absSum     = (:+ 0) . toScalarC AbsSum
    dot        = dotC
    norm1      = (:+ 0) . sumElements . fst . fromComplex . vectorMapC Abs
    normInf    = (:+ 0) . maxElement . fst . fromComplex . vectorMapC Abs

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
