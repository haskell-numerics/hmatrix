{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}


{- |
Module      :  Numeric.HMatrix.Static.Complex
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  experimental

-}

module Numeric.Complex(
    C, M,
    vec2, vec3, vec4, (&), (#),
    vect,
    Her, her, 𝑖,
) where

import GHC.TypeLits
import Numeric.LinearAlgebra.Util(ℂ,iC)
import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.Static


𝑖 :: Sized ℂ s c => s
𝑖 = konst iC

newtype Her n = Her (M n n)

her :: KnownNat n => M n n -> Her n
her m = Her $ (m + LA.tr m)/2




infixl 4 &
(&) :: forall n . KnownNat n
    => C n -> ℂ -> C (n+1)
u & x = u # (mkC (LA.scalar x) :: C 1)

infixl 4 #
(#) :: forall n m . (KnownNat n, KnownNat m)
    => C n -> C m -> C (n+m)
(C u) # (C v) = C (vconcat u v)



vec2 :: ℂ -> ℂ -> C 2
vec2 a b = C (gvec2 a b)

vec3 :: ℂ -> ℂ -> ℂ -> C 3
vec3 a b c = C (gvec3 a b c)


vec4 :: ℂ -> ℂ -> ℂ -> ℂ -> C 4
vec4 a b c d = C (gvec4 a b c d)

vect :: forall n . KnownNat n => [ℂ] -> C n
vect xs = C (gvect "C" xs)

