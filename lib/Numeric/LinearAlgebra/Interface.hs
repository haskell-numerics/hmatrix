{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Interface
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

(Very provisional) operators for frequent operations.

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.Interface(
    (<>),(<.>),
    (<\>),
    (.*),(*/),
    (<|>),(<->),
) where

import Numeric.LinearAlgebra.Linear
import Data.Packed.Vector
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms

class Mul a b c | a b -> c where
 infixl 7 <>
 -- | matrix product
 (<>) :: Field t => a t -> b t -> c t

instance Mul Matrix Matrix Matrix where
    (<>) = multiply

instance Mul Matrix Vector Vector where
    (<>) m v = flatten $ m <> (asColumn v)

instance Mul Vector Matrix Vector where
    (<>) v m = flatten $ (asRow v) <> m

---------------------------------------------------

-- | @u \<.\> v = dot u v@
(<.>) :: (Field t) => Vector t -> Vector t -> t
infixl 7 <.>
(<.>) = dot

----------------------------------------------------

-- | @x .* a = scale x a@
(.*) :: (Linear c a) => a -> c a -> c a
infixl 7 .*
a .* x = scale a x

----------------------------------------------------

-- | @a *\/ x = scale (recip x) a@
(*/) :: (Linear c a) => c a -> a -> c a
infixl 7 */
v */ x = scale (recip x) v

-- | least squares solution of a linear system, similar to the \\ operator of Matlab\/Octave (based on linearSolveSVD).
(<\>) :: (GenMat a) => Matrix a -> Vector a -> Vector a
infixl 7 <\>
m <\> v = flatten (linearSolveSVD m (reshape 1 v))

------------------------------------------------

class Joinable a b where
    joinH :: Field t => a t -> b t -> Matrix t
    joinV :: Field t => a t -> b t -> Matrix t

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

{- | Horizontal concatenation of matrices and vectors:

@> (ident 3 \<-\> 3 * ident 3) \<|\> fromList [1..6.0]
(6><4)
 [ 1.0, 0.0, 0.0, 1.0
 , 0.0, 1.0, 0.0, 2.0
 , 0.0, 0.0, 1.0, 3.0
 , 3.0, 0.0, 0.0, 4.0
 , 0.0, 3.0, 0.0, 5.0
 , 0.0, 0.0, 3.0, 6.0 ]@
-}
(<|>) :: (Field t, Joinable a b) => a t -> b t -> Matrix t
a <|> b = joinH a b

-- | Vertical concatenation of matrices and vectors.
(<->) :: (Field t, Joinable a b) => a t -> b t -> Matrix t
a <-> b = joinV a b
