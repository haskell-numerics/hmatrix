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
Module      :  Numeric.LinearAlgebra.Complex
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Stability   :  experimental

-}

module Numeric.LinearAlgebra.Complex(
    C,
    vec2, vec3, vec4, (&), (#),
    vect,
    R
) where

import GHC.TypeLits
import Numeric.HMatrix hiding (
    (<>),(#>),(<·>),Konst(..),diag, disp,(¦),(——),row,col,vect,mat,linspace)
import qualified Numeric.HMatrix as LA
import Data.Proxy(Proxy)
import Numeric.LinearAlgebra.Static



instance forall n . KnownNat n => Show (C n)
  where
    show (ud1 -> v)
      | size v == 1 = "("++show (v!0)++" :: C "++show d++")"
      | otherwise   = "(vect"++ drop 8 (show v)++" :: C "++show d++")"
      where
        d = fromIntegral . natVal $ (undefined :: Proxy n) :: Int


ud1 :: C n -> Vector ℂ
ud1 (C (Dim v)) = v

mkC :: Vector ℂ -> C n
mkC = C . Dim


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

