{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Interface
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Some useful operators, and Show, Read, Eq, Num, Fractional, and Floating instances for Vector and Matrix.

In the context of the standard numeric operators, one-component vectors and matrices automatically expand to match the dimensions of the other operand.


-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Interface(
    (<>),(<.>),mulG, Adapt, adaptElements,
    (<\>),
    (.*),(*/),
    (<|>),(<->),
) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Linear
import Data.Complex
import Control.Arrow((***))

--import Numeric.GSL.Vector

class Mul a b c | a b -> c where
 infixl 7 <>
 -- | Matrix-matrix, matrix-vector, and vector-matrix products.
 (<>)  :: Product t => a t -> b t -> c t
 mulG :: (Element r, Element s, Adapt r s t t, Product t) => a r -> b s -> c t

instance Mul Matrix Matrix Matrix where
    (<>) = mXm
    mulG a b = uncurry mXm (curry adapt a b)

instance Mul Matrix Vector Vector where
    (<>) m v = flatten $ m <> (asColumn v)
    mulG m v = flatten $ m `mulG` (asColumn v)

instance Mul Vector Matrix Vector where
    (<>) v m = flatten $ (asRow v) <> m
    mulG v m = flatten $ (asRow v) `mulG` m

---------------------------------------------------

-- | Dot product: @u \<.\> v = dot u v@
--(<.>) :: (Field t) => Vector t -> Vector t -> t
(<.>) :: Vectors Vector t => Vector t -> Vector t -> t
infixl 7 <.>
(<.>) = dot

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

----------------------------------------------------

class Adapt a b c d | a b -> c, a b -> d where
    adapt :: Container k => (k a, k b) -> (k c, k d) 

--instance Adapt a a a a where
--    adapt = id *** id

instance Adapt Float Float Float Float where
    adapt = id *** id

instance Adapt Double Double Double Double where
    adapt = id *** id

instance Adapt (Complex Float) (Complex Float) (Complex Float) (Complex Float) where
    adapt = id *** id

instance Adapt Float Double Double Double where
    adapt = double *** id

instance Adapt Double Float Double Double where
    adapt = id *** double

instance Adapt Float (Complex Float) (Complex Float) (Complex Float) where
    adapt = complex *** id

instance Adapt (Complex Float) Float (Complex Float) (Complex Float) where
    adapt = id *** complex

instance (Convert a, Convert (DoubleOf a), ComplexOf (DoubleOf a) ~ Complex Double) => Adapt a (Complex Double) (Complex Double) (Complex Double) where
    adapt = complex.double *** id

instance (Convert a, Convert (DoubleOf a), ComplexOf (DoubleOf a) ~ Complex Double) => Adapt (Complex Double) a (Complex Double) (Complex Double) where
    adapt = id *** complex.double

instance Adapt Double (Complex Float) (Complex Double) (Complex Double) where
    adapt = complex *** double

instance Adapt (Complex Float) Double (Complex Double) (Complex Double) where
    adapt = double *** complex

adaptElements:: (Adapt a b c d, Container k) => (k a, k b) -> (k c, k d)
adaptElements p = adapt p

