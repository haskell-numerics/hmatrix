{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
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
    Linear(..)
) where


import Data.Packed.Internal(partit)
import Data.Packed
import Numeric.GSL.Vector
import Complex

-- | A generic interface for vectors and matrices to a few element-by-element functions in Numeric.GSL.Vector.
class (Container c e) => Linear c e where
    scale       :: e -> c e -> c e
    addConstant :: e -> c e -> c e
    add         :: c e -> c e -> c e
    sub         :: c e -> c e -> c e
    -- | element by element multiplication
    mul         :: c e -> c e -> c e
    -- | element by element division
    divide      :: c e -> c e -> c e
    -- | scale the element by element reciprocal of the object: @scaleRecip 2 (fromList [5,i]) == 2 |> [0.4 :+ 0.0,0.0 :+ (-2.0)]@
    scaleRecip  :: e -> c e -> c e
    equal       :: c e -> c e -> Bool
--  numequal    :: Double -> c e -> c e -> Bool

instance Linear Vector Double where
    scale = vectorMapValR Scale
    scaleRecip = vectorMapValR Recip
    addConstant = vectorMapValR AddConstant
    add = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul
    divide = vectorZipR Div
    equal u v = dim u == dim v && vectorMax (vectorMapR Abs (sub u v)) == 0.0

instance Linear Vector (Complex Double) where
    scale = vectorMapValC Scale
    scaleRecip = vectorMapValC Recip
    addConstant = vectorMapValC AddConstant
    add = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul
    divide = vectorZipC Div
    equal u v = dim u == dim v && vectorMax (liftVector magnitude (sub u v)) == 0.0

instance (Linear Vector a, Container Matrix a) => (Linear Matrix a) where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    equal a b = cols a == cols b && flatten a `equal` flatten b
