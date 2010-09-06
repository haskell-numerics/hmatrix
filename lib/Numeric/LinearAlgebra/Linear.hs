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
    Vectors(..),
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
import Numeric.Vector()
import Numeric.Matrix()
import Numeric.GSL.Vector
import Numeric.LinearAlgebra.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ)

-- | Linear algebraic properties of objects
class Num e => Vectors a e where
    -- | dot (inner) product
    dot        :: a e -> a e -> e
    -- | sum of absolute value of elements (differs in complex case from @norm1@
    absSum     :: a e -> e
    -- | sum of absolute value of elements
    norm1      :: a e -> e
    -- | euclidean norm
    norm2      :: a e -> e
    -- | element of maximum magnitude
    normInf    :: a e -> e

instance Vectors Vector Float where
    norm2      = toScalarF Norm2
    absSum     = toScalarF AbsSum
    dot        = dotF
    norm1      = toScalarF AbsSum
    normInf    = maxElement . vectorMapF Abs

instance Vectors Vector Double where
    norm2      = toScalarR Norm2
    absSum     = toScalarR AbsSum
    dot        = dotR
    norm1      = toScalarR AbsSum
    normInf    = maxElement . vectorMapR Abs

instance Vectors Vector (Complex Float) where
    norm2      = (:+ 0) . toScalarQ Norm2
    absSum     = (:+ 0) . toScalarQ AbsSum
    dot        = dotQ
    norm1      = (:+ 0) . sumElements . fst . fromComplex . vectorMapQ Abs
    normInf    = (:+ 0) . maxElement . fst . fromComplex . vectorMapQ Abs

instance Vectors Vector (Complex Double) where
    norm2      = (:+ 0) . toScalarC Norm2
    absSum     = (:+ 0) . toScalarC AbsSum
    dot        = dotC
    norm1      = (:+ 0) . sumElements . fst . fromComplex . vectorMapC Abs
    normInf    = (:+ 0) . maxElement . fst . fromComplex . vectorMapC Abs

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
