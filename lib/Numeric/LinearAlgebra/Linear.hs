{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
    -- * Linear Algebra Typeclasses
    Vectors(..), Linear(..),
    -- * Products
    Product(..),
    mXm,mXv,vXm,
    outer, kronecker,
    -- * Modules
    --module Numeric.Vector,
    --module Numeric.Matrix,
    module Numeric.Container
) where

import Data.Packed.Internal.Common
import Data.Packed.Matrix
import Data.Packed.Vector
import Data.Complex
import Numeric.Container
--import Numeric.Vector
--import Numeric.Matrix
--import Numeric.GSL.Vector
import Numeric.LinearAlgebra.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ)

-- | basic Vector functions
class Num e => Vectors a e where
    -- the C functions sumX are twice as fast as using foldVector
    vectorSum  :: a e -> e
    vectorProd :: a e -> e
    absSum     :: a e -> e
    dot        :: a e -> a e -> e
    norm1      :: a e -> e
    norm2      :: a e -> e
    normInf    :: a e -> e

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


-------------------------------------------------------------------


-- | Basic element-by-element functions.
class (Element e, Container c e) => Linear c e where
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
