{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  LinearAlgebra.Linear
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi


-}
-----------------------------------------------------------------------------

module LinearAlgebra.Linear (
    Linear(..),
    toComplex, comp,
    conj,
    multiply, dot, outer
) where


import Data.Packed.Internal
import Data.Packed.Matrix
import GSL.Vector
import Complex


class (Field e) => Linear c e where
    scale       :: e -> c e -> c e
    addConstant :: e -> c e -> c e
    add         :: c e -> c e -> c e
    sub         :: c e -> c e -> c e
    mul         :: c e -> c e -> c e

instance Linear Vector Double where
    scale = vectorMapValR Scale
    addConstant = vectorMapValR AddConstant
    add = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul

instance Linear Vector (Complex Double) where
    scale = vectorMapValC Scale
    addConstant = vectorMapValC AddConstant
    add = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul

--------------------------------------------------


-- | euclidean inner product
dot :: (Field t) => Vector t -> Vector t -> t
dot u v = dat (multiply r c) `at` 0
    where r = asRow u
          c = asColumn v




{- | Outer product of two vectors.

@\> 'fromList' [1,2,3] \`outer\` 'fromList' [5,2,3]
(3><3)
 [  5.0, 2.0, 3.0
 , 10.0, 4.0, 6.0
 , 15.0, 6.0, 9.0 ]@
-}
outer :: (Field t) => Vector t -> Vector t -> Matrix t
outer u v = asColumn u `multiply` asRow v
