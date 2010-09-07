{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Matrix
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Numeric instances and functions for 'Data.Packed.Matrix's
--
-----------------------------------------------------------------------------

module Numeric.Matrix (
                       module Data.Packed.Matrix,
                      ) where

-------------------------------------------------------------------

import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.Container
--import Numeric.LinearAlgebra.Linear
import Numeric.Vector()

import Control.Monad(ap)

import Control.Arrow((***))

-------------------------------------------------------------------

instance Linear Matrix a => Eq (Matrix a) where
    (==) = equal

instance (Linear Matrix a, Num (Vector a)) => Num (Matrix a) where
    (+) = liftMatrix2Auto (+)
    (-) = liftMatrix2Auto (-)
    negate = liftMatrix negate
    (*) = liftMatrix2Auto (*)
    signum = liftMatrix signum
    abs = liftMatrix abs
    fromInteger = (1><1) . return . fromInteger

---------------------------------------------------

instance (Linear Vector a, Fractional (Vector a), Num (Matrix a)) => Fractional (Matrix a) where
    fromRational n = (1><1) [fromRational n]
    (/) = liftMatrix2Auto (/)

---------------------------------------------------------

instance (Linear Vector a, Floating (Vector a), Fractional (Matrix a)) => Floating (Matrix a) where
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

---------------------------------------------------------------

instance NumericContainer Matrix where
    toComplex = uncurry $ liftMatrix2 $ curry toComplex
    fromComplex z = (reshape c *** reshape c) . fromComplex . flatten $ z
        where c = cols z
    complex' = liftMatrix complex'
    conj = liftMatrix conj
--    cmap f = liftMatrix (cmap f)
    single' = liftMatrix single'
    double' = liftMatrix double'

---------------------------------------------------------------

instance (Linear Vector a, Container Matrix a) => Linear Matrix a where
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
instance (Container Vector a) => Container Matrix a where
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
