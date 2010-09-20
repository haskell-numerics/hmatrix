{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix
-- Copyright   :  (c) Alberto Ruiz 2010
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Numeric instances and functions for 'Matrix'.
-- In the context of the standard numeric operators, one-component
-- vectors and matrices automatically expand to match the dimensions of the other operand.
--
-- Provides instances of standard classes 'Show', 'Read', 'Eq',
-- 'Num', 'Fractional', and 'Floating' for 'Matrix'.
-- 
-----------------------------------------------------------------------------

module Numeric.Matrix (
    -- * Basic functions
    module Data.Packed.Matrix,
    module Numeric.Vector,
    optimiseMult,
    -- * Operators
    (<>), (<\>)
                      ) where

-------------------------------------------------------------------

import Data.Packed.Matrix
import Numeric.Vector
import Numeric.Chain
import Numeric.LinearAlgebra.Algorithms

-------------------------------------------------------------------

instance Container Matrix a => Eq (Matrix a) where
    (==) = equal

instance (Container Matrix a, Num (Vector a)) => Num (Matrix a) where
    (+) = liftMatrix2Auto (+)
    (-) = liftMatrix2Auto (-)
    negate = liftMatrix negate
    (*) = liftMatrix2Auto (*)
    signum = liftMatrix signum
    abs = liftMatrix abs
    fromInteger = (1><1) . return . fromInteger

---------------------------------------------------

instance (Container Vector a, Fractional (Vector a), Num (Matrix a)) => Fractional (Matrix a) where
    fromRational n = (1><1) [fromRational n]
    (/) = liftMatrix2Auto (/)

---------------------------------------------------------

instance (Floating a, Container Vector a, Floating (Vector a), Fractional (Matrix a)) => Floating (Matrix a) where
    sin   = liftMatrix sin
    cos   = liftMatrix cos
    tan   = liftMatrix tan
    asin  = liftMatrix asin
    acos  = liftMatrix acos
    atan  = liftMatrix atan
    sinh  = liftMatrix sinh
    cosh  = liftMatrix cosh
    tanh  = liftMatrix tanh
    asinh = liftMatrix asinh
    acosh = liftMatrix acosh
    atanh = liftMatrix atanh
    exp   = liftMatrix exp
    log   = liftMatrix log
    (**)  = liftMatrix2Auto (**)
    sqrt  = liftMatrix sqrt
    pi    = (1><1) [pi]

--------------------------------------------------------

class Mul a b c | a b -> c where
 infixl 7 <>
 -- | Matrix-matrix, matrix-vector, and vector-matrix products.
 (<>)  :: Product t => a t -> b t -> c t

instance Mul Matrix Matrix Matrix where
    (<>) = mXm

instance Mul Matrix Vector Vector where
    (<>) m v = flatten $ m <> (asColumn v)

instance Mul Vector Matrix Vector where
    (<>) v m = flatten $ (asRow v) <> m

--------------------------------------------------------

-- | least squares solution of a linear system, similar to the \\ operator of Matlab\/Octave (based on linearSolveSVD).
(<\>) :: (Field a) => Matrix a -> Vector a -> Vector a
infixl 7 <\>
m <\> v = flatten (linearSolveSVD m (reshape 1 v))
