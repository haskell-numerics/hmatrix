{-# OPTIONS_GHC -fglasgow-exts #-}
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
    Linear(..),
    multiply, dot, outer, kronecker
) where


import Data.Packed.Internal(multiply,at,partit)
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

instance Linear Matrix Double where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    equal a b = cols a == cols b && flatten a `equal` flatten b


instance Linear Matrix (Complex Double) where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    equal a b = cols a == cols b && flatten a `equal` flatten b

--------------------------------------------------

-- | euclidean inner product
dot :: (Element t) => Vector t -> Vector t -> t
dot u v = multiply r c  @@> (0,0)
    where r = asRow u
          c = asColumn v


{- | Outer product of two vectors.

@\> 'fromList' [1,2,3] \`outer\` 'fromList' [5,2,3]
(3><3)
 [  5.0, 2.0, 3.0
 , 10.0, 4.0, 6.0
 , 15.0, 6.0, 9.0 ]@
-}
outer :: (Element t) => Vector t -> Vector t -> Matrix t
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
kronecker :: (Element t) => Matrix t -> Matrix t -> Matrix t
kronecker a b = fromBlocks
              . partit (cols a)
              . map (reshape (cols b))
              . toRows
              $ flatten a `outer` flatten b
