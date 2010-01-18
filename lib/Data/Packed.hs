{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Data.Packed
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

The Vector and Matrix types and some utilities.

-}
-----------------------------------------------------------------------------

module Data.Packed (
    module Data.Packed.Vector,
    module Data.Packed.Matrix,
    module Data.Complex,
    Container(..)
) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Complex
import Data.Packed.Internal(fromComplex,toComplex,conj)

-- | conversion utilities
class (Element e) => Container c e where
    toComplex   :: RealFloat e => (c e, c e) -> c (Complex e)
    fromComplex :: RealFloat e => c (Complex e) -> (c e, c e)
    comp        :: RealFloat e => c e -> c (Complex e)
    conj        :: RealFloat e => c (Complex e) -> c (Complex e)
    real        :: c Double -> c e
    complex     :: c e -> c (Complex Double)

instance Container Vector Double where
    toComplex = Data.Packed.Internal.toComplex
    fromComplex = Data.Packed.Internal.fromComplex
    comp = internalcomp
    conj = Data.Packed.Internal.conj
    real = id
    complex = Data.Packed.comp

instance Container Vector (Complex Double) where
    toComplex = undefined -- can't match
    fromComplex = undefined
    comp = undefined
    conj = undefined
    real = Data.Packed.comp
    complex = id

instance Container Matrix Double where
    toComplex = uncurry $ liftMatrix2 $ curry Data.Packed.toComplex
    fromComplex z = (reshape c r, reshape c i)
        where (r,i) = Data.Packed.fromComplex (flatten z)
              c = cols z
    comp = liftMatrix internalcomp
    conj = liftMatrix Data.Packed.Internal.conj
    real = id
    complex = Data.Packed.comp

instance Container Matrix (Complex Double) where
    toComplex = undefined
    fromComplex = undefined
    comp = undefined
    conj = undefined
    real = Data.Packed.comp
    complex = id


-- | converts a real vector into a complex representation (with zero imaginary parts)
internalcomp :: Vector Double -> Vector (Complex Double)
internalcomp v = Data.Packed.Internal.toComplex (v,constant 0 (dim v))
