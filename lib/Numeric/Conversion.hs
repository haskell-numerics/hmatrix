{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Container
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Numeric classes for containers of numbers, including conversion routines
--
-----------------------------------------------------------------------------

module Numeric.Conversion (
    RealElement, --Precision,
    ComplexContainer(toComplex,fromComplex,conj,comp),
    Convert(..), --AutoReal(..),
    RealOf, ComplexOf, SingleOf, DoubleOf,
    module Data.Complex
) where


import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix
--import Numeric.GSL.Vector

import Data.Complex
--import Control.Monad(ap)
import Control.Arrow((***))

--import Numeric.LinearAlgebra.LAPACK(multiplyR,multiplyC,multiplyF,multiplyQ)

-------------------------------------------------------------------

-- | Supported single-double precision type pairs
class (Element s, Element d) => Precision s d | s -> d, d -> s where
    double2FloatG :: Vector d -> Vector s
    float2DoubleG :: Vector s -> Vector d

instance Precision Float Double where
    double2FloatG = double2FloatV
    float2DoubleG = float2DoubleV

instance Precision (Complex Float) (Complex Double) where
    double2FloatG = asComplex . double2FloatV . asReal
    float2DoubleG = asComplex . float2DoubleV . asReal

-- | Supported real types
class (Element t, Element (Complex t), RealFloat t
--       , RealOf t ~ t, RealOf (Complex t) ~ t
       )
    => RealElement t

instance RealElement Double

instance RealElement Float

-- | Conversion utilities
class ComplexContainer c where
    toComplex   :: (RealElement e) => (c e, c e) -> c (Complex e)
    fromComplex :: (RealElement e) => c (Complex e) -> (c e, c e)
    comp        :: (RealElement e) => c e -> c (Complex e)
    conj        :: (RealElement e) => c (Complex e) -> c (Complex e)
    single'      :: Precision a b => c b -> c a
    double'      :: Precision a b => c a -> c b


instance ComplexContainer Vector where
    toComplex = toComplexV
    fromComplex = fromComplexV
    comp v = toComplex (v,constantD 0 (dim v))
    conj = conjugateD
    single' = double2FloatG
    double' = float2DoubleG


-- | creates a complex vector from vectors with real and imaginary parts
toComplexV :: (RealElement a) => (Vector a, Vector a) ->  Vector (Complex a)
toComplexV (r,i) = asComplex $ flatten $ fromColumns [r,i]

-- | the inverse of 'toComplex'
fromComplexV :: (RealElement a) => Vector (Complex a) -> (Vector a, Vector a)
fromComplexV z = (r,i) where
    [r,i] = toColumns $ reshape 2 $ asReal z


instance ComplexContainer Matrix where
    toComplex = uncurry $ liftMatrix2 $ curry toComplex
    fromComplex z = (reshape c *** reshape c) . fromComplex . flatten $ z
        where c = cols z
    comp = liftMatrix comp
    conj = liftMatrix conj
    single' = liftMatrix single'
    double' = liftMatrix double'

-------------------------------------------------------------------

type family RealOf x

type instance RealOf Double = Double
type instance RealOf (Complex Double) = Double

type instance RealOf Float = Float
type instance RealOf (Complex Float) = Float

type family ComplexOf x

type instance ComplexOf Double = Complex Double
type instance ComplexOf (Complex Double) = Complex Double

type instance ComplexOf Float = Complex Float
type instance ComplexOf (Complex Float) = Complex Float

type family SingleOf x

type instance SingleOf Double = Float
type instance SingleOf Float  = Float

type instance SingleOf (Complex a) = Complex (SingleOf a)

type family DoubleOf x

type instance DoubleOf Double = Double
type instance DoubleOf Float  = Double

type instance DoubleOf (Complex a) = Complex (DoubleOf a)

type family ElementOf c

type instance ElementOf (Vector a) = a
type instance ElementOf (Matrix a) = a


-------------------------------------------------------------------

class (Element t, Element (RealOf t)) => Convert t where
    real    :: ComplexContainer c => c (RealOf t) -> c t
    complex :: ComplexContainer c => c t -> c (ComplexOf t)
    single  :: ComplexContainer c => c t -> c (SingleOf t)
    double  :: ComplexContainer c => c t -> c (DoubleOf t)


instance Convert Double where
    real = id
    complex = comp
    single = single'
    double = id

instance Convert Float where
    real = id
    complex = comp
    single = id
    double = double'

instance Convert (Complex Double) where
    real = comp
    complex = id
    single = single'
    double = id

instance Convert (Complex Float) where
    real = comp
    complex = id
    single = id
    double = double'

-------------------------------------------------------------------

-- | to be replaced by Convert
class Convert t => AutoReal t where
    real'' :: ComplexContainer c => c Double -> c t
    complex'' :: ComplexContainer c => c t -> c (Complex Double)


instance AutoReal Double where
    real'' = real
    complex'' = complex

instance AutoReal (Complex Double) where
    real'' = real
    complex'' = complex

instance AutoReal Float where
    real'' = real . single
    complex'' = double . complex

instance AutoReal (Complex Float) where
    real'' = real . single
    complex'' = double . complex

