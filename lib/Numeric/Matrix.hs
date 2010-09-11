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
-----------------------------------------------------------------------------

module Numeric.Matrix (
    -- * Basic functions
    module Data.Packed.Matrix,
    module Numeric.Vector,
    --module Numeric.Container,
    chain,
    -- * Operators
    (<>), (<\>),
    -- * Deprecated
    (.*),(*/),(<|>),(<->),
    vectorMax,vectorMin
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

instance (Container Vector a, Floating (Vector a), Fractional (Matrix a)) => Floating (Matrix a) where
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

----------------------------------------------------

{-# DEPRECATED (.*) "use scale a x or scalar a * x" #-}

-- -- | @x .* a = scale x a@
-- (.*) :: (Linear c a) => a -> c a -> c a
infixl 7 .*
a .* x = scale a x

----------------------------------------------------

{-# DEPRECATED (*/) "use scale (recip a) x or x / scalar a" #-}

-- -- | @a *\/ x = scale (recip x) a@
-- (*/) :: (Linear c a) => c a -> a -> c a
infixl 7 */
v */ x = scale (recip x) v

-- | least squares solution of a linear system, similar to the \\ operator of Matlab\/Octave (based on linearSolveSVD).
(<\>) :: (Field a) => Matrix a -> Vector a -> Vector a
infixl 7 <\>
m <\> v = flatten (linearSolveSVD m (reshape 1 v))

------------------------------------------------

{-# DEPRECATED (<|>) "define operator a & b = fromBlocks[[a,b]] and use asRow/asColumn to join vectors" #-}
{-# DEPRECATED (<->) "define operator a // b = fromBlocks[[a],[b]] and use asRow/asColumn to join vectors" #-}

class Joinable a b where
    joinH :: Element t => a t -> b t -> Matrix t
    joinV :: Element t => a t -> b t -> Matrix t

instance Joinable Matrix Matrix where
    joinH m1 m2 = fromBlocks [[m1,m2]]
    joinV m1 m2 = fromBlocks [[m1],[m2]]

instance Joinable Matrix Vector where
    joinH m v = joinH m (asColumn v)
    joinV m v = joinV m (asRow v)

instance Joinable Vector Matrix where
    joinH v m = joinH (asColumn v) m
    joinV v m = joinV (asRow v) m

infixl 4 <|>
infixl 3 <->

{-- - | Horizontal concatenation of matrices and vectors:

@> (ident 3 \<-\> 3 * ident 3) \<|\> fromList [1..6.0]
(6><4)
 [ 1.0, 0.0, 0.0, 1.0
 , 0.0, 1.0, 0.0, 2.0
 , 0.0, 0.0, 1.0, 3.0
 , 3.0, 0.0, 0.0, 4.0
 , 0.0, 3.0, 0.0, 5.0
 , 0.0, 0.0, 3.0, 6.0 ]@
-}
-- (<|>) :: (Element t, Joinable a b) => a t -> b t -> Matrix t
a <|> b = joinH a b

-- -- | Vertical concatenation of matrices and vectors.
-- (<->) :: (Element t, Joinable a b) => a t -> b t -> Matrix t
a <-> b = joinV a b

-------------------------------------------------------------------

{-# DEPRECATED vectorMin "use minElement" #-}
vectorMin :: (Container Vector t, Element t) => Vector t -> t
vectorMin = minElement

{-# DEPRECATED vectorMax "use maxElement" #-}
vectorMax :: (Container Vector t, Element t) => Vector t -> t
vectorMax = maxElement
