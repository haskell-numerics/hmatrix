{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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

module Numeric.Container (
    Linear(..),
    Container(..), RealElement, Precision(..), NumericContainer(..), comp, 
    Convert(..), --AutoReal(..), 
    RealOf, ComplexOf, SingleOf, DoubleOf, 

--    ElementOf, 

    IndexOf,

    module Data.Complex
) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.Internal.Vector
--import Data.Packed.Internal.Matrix
--import qualified Data.Packed.ST as ST

--import Control.Arrow((***))
import Data.Complex

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
class NumericContainer c where
    toComplex   :: (RealElement e) => (c e, c e) -> c (Complex e)
    fromComplex :: (RealElement e) => c (Complex e) -> (c e, c e)
    complex'    :: (RealElement e) => c e -> c (Complex e)
    conj        :: (RealElement e) => c (Complex e) -> c (Complex e)
--    cmap        :: (Element a, Element b) => (a -> b) -> c a -> c b
    single'      :: Precision a b => c b -> c a
    double'      :: Precision a b => c a -> c b

-- | a synonym for "complex'"
comp :: (NumericContainer c, RealElement e) => c e -> c (Complex e)
comp x = complex' x

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

type family IndexOf c

type instance IndexOf Vector = Int
type instance IndexOf Matrix = (Int,Int)

-------------------------------------------------------------------

class (Element t, Element (RealOf t)) => Convert t where
    real    :: NumericContainer c => c (RealOf t) -> c t
    complex :: NumericContainer c => c t -> c (ComplexOf t)
    single  :: NumericContainer c => c t -> c (SingleOf t)
    double  :: NumericContainer c => c t -> c (DoubleOf t)


instance Convert Double where
    real = id
    complex = complex'
    single = single'
    double = id

instance Convert Float where
    real = id
    complex = complex'
    single = id
    double = double'

instance Convert (Complex Double) where
    real = complex'
    complex = id
    single = single'
    double = id

instance Convert (Complex Float) where
    real = complex'
    complex = id
    single = id
    double = double'

-------------------------------------------------------------------

-- | to be replaced by Convert
class Convert t => AutoReal t where
    real'' :: NumericContainer c => c Double -> c t
    complex'' :: NumericContainer c => c t -> c (Complex Double)


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

-------------------------------------------------------------------

-- | Basic element-by-element functions for numeric containers
class (Element e) => Container c e where
{-
    -- | create a structure with a single element
    scalar      :: e -> c e
    -- | multiply every element by a scalar
    scale       :: e -> c e -> c e
    -- | scale the element by element reciprocal of the object:
    --
    -- @scaleRecip 2 (fromList [5,i]) == 2 |> [0.4 :+ 0.0,0.0 :+ (-2.0)]@
    scaleRecip  :: e -> c e -> c e
    -- | add a constant to each element
    addConstant :: e -> c e -> c e
    add         :: c e -> c e -> c e
    sub         :: c e -> c e -> c e
    -- | element by element multiplication
    mul         :: c e -> c e -> c e
    -- | element by element division
    divide      :: c e -> c e -> c e
    equal       :: c e -> c e -> Bool
-}
    -- | cannot implement instance Functor because of Element class constraint
    cmap        :: (Element a, Element b) => (a -> b) -> c a -> c b
    --
    -- | indexing function
    atIndex     :: c e -> IndexOf c -> e
    -- | index of min/max element
    minIndex    :: c e -> IndexOf c
    maxIndex    :: c e -> IndexOf c
    -- | value of min/max element
    minElement  :: c e -> e
    maxElement  :: c e -> e
    -- the C functions sumX/prodX are twice as fast as using foldVector
    -- | the sum/product of elements (faster than using @fold@
    sumElements :: c e -> e
    prodElements :: c e -> e

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



