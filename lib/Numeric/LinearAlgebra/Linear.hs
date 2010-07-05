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
    Vectors(..), normalise,                                    
    Linear(..)
) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Complex
import Numeric.GSL.Vector

-- | normalise a vector to unit length
normalise :: (Floating a, Vectors Vector a, 
              Linear Vector a, Fractional (Vector a)) => Vector a -> Vector a
normalise v = scaleRecip (vectorSum v) v 

-- | basic Vector functions
class (Num b) => Vectors a b where
    vectorSum :: a b -> b
    euclidean :: a b -> b
    absSum    :: a b -> b
    vectorMin :: a b -> b
    vectorMax :: a b -> b
    minIdx    :: a b -> Int
    maxIdx    :: a b -> Int
    dot       :: a b -> a b -> b

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
    euclidean = undefined
    absSum    = undefined
    vectorMin = undefined
    vectorMax = undefined
    minIdx    = undefined
    maxIdx    = undefined
    dot       = dotQ

instance Vectors Vector (Complex Double) where
    vectorSum  = sumC
    euclidean = undefined
    absSum    = undefined
    vectorMin = undefined
    vectorMax = undefined
    minIdx    = undefined
    maxIdx    = undefined
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
