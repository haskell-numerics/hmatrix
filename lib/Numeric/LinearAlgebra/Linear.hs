{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
    Vectors(..),                                    
    Linear(..)
) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Complex
import Numeric.GSL.Vector

import Control.Monad(ap)

-- | basic Vector functions
class Num e => Vectors a e where
    vectorSum :: a e -> e
    euclidean :: a e -> e
    absSum    :: a e -> e
    vectorMin :: a e -> e
    vectorMax :: a e -> e
    minIdx    :: a e -> Int
    maxIdx    :: a e -> Int
    dot       :: a e -> a e -> e

instance Vectors Vector Float where
    vectorSum = sumF
    euclidean = toScalarF Norm2
    absSum    = toScalarF AbsSum
    vectorMin = toScalarF Min
    vectorMax = toScalarF Max
    minIdx    = round . toScalarF MinIdx
    maxIdx    = round . toScalarF MaxIdx
    dot       = dotF

instance Vectors Vector Double where
    vectorSum = sumR
    euclidean = toScalarR Norm2
    absSum    = toScalarR AbsSum
    vectorMin = toScalarR Min
    vectorMax = toScalarR Max
    minIdx    = round . toScalarR MinIdx
    maxIdx    = round . toScalarR MaxIdx
    dot       = dotR

instance Vectors Vector (Complex Float) where
    vectorSum = sumQ
    euclidean = (:+ 0) . toScalarQ Norm2
    absSum    = (:+ 0) . toScalarQ AbsSum
    vectorMin = ap (@>) minIdx
    vectorMax = ap (@>) maxIdx
    minIdx    = minIdx . fst . fromComplex . (zipVector (*) `ap` mapVector conjugate)
    maxIdx    = maxIdx . fst . fromComplex . (zipVector (*) `ap` mapVector conjugate)
    dot       = dotQ

instance Vectors Vector (Complex Double) where
    vectorSum  = sumC
    euclidean = (:+ 0) . toScalarC Norm2
    absSum    = (:+ 0) . toScalarC AbsSum
    vectorMin = ap (@>) minIdx
    vectorMax = ap (@>) maxIdx
    minIdx    = minIdx . fst . fromComplex . (zipVector (*) `ap` mapVector conjugate)
    maxIdx    = maxIdx . fst . fromComplex . (zipVector (*) `ap` mapVector conjugate)
    dot       = dotC

----------------------------------------------------

-- | Basic element-by-element functions.
class (Container c e) => Linear c e where
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

instance (Linear Vector a, Container Matrix a) => (Linear Matrix a) where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    equal a b = cols a == cols b && flatten a `equal` flatten b
    scalar x = (1><1) [x]
