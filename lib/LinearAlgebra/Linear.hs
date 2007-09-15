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
    dot, outer,
    Mul(..)
) where


import Data.Packed.Internal
import Data.Packed.Matrix
import GSL.Vector
import Complex


class (Field e) => Linear c e where
    scale       :: e -> c e -> c e
    scaleRecip  :: e -> c e -> c e
    addConstant :: e -> c e -> c e
    add         :: c e -> c e -> c e
    sub         :: c e -> c e -> c e
    mul         :: c e -> c e -> c e
    divide      :: c e -> c e -> c e
    toComplex   :: RealFloat e => (c e, c e) -> c (Complex e)
    fromComplex :: RealFloat e => c (Complex e) -> (c e, c e)
    comp        :: RealFloat e => c e -> c (Complex e)
    conj        :: RealFloat e => c (Complex e) -> c (Complex e)

instance Linear Vector Double where
    scale = vectorMapValR Scale
    scaleRecip = vectorMapValR Recip
    addConstant = vectorMapValR AddConstant
    add = vectorZipR Add
    sub = vectorZipR Sub
    mul = vectorZipR Mul
    divide = vectorZipR Div
    toComplex = Data.Packed.Internal.toComplex
    fromComplex = Data.Packed.Internal.fromComplex
    comp = Data.Packed.Internal.comp
    conj = Data.Packed.Internal.conj

instance Linear Vector (Complex Double) where
    scale = vectorMapValC Scale
    scaleRecip = vectorMapValC Recip
    addConstant = vectorMapValC AddConstant
    add = vectorZipC Add
    sub = vectorZipC Sub
    mul = vectorZipC Mul
    divide = vectorZipC Div
    toComplex = undefined -- can't match
    fromComplex = undefined
    comp = undefined
    conj = undefined

instance Linear Matrix Double where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    toComplex = uncurry $ liftMatrix2 $ curry LinearAlgebra.Linear.toComplex
    fromComplex z = (reshape c r, reshape c i)
        where (r,i) = LinearAlgebra.Linear.fromComplex (cdat z)
              c = cols z
    comp = liftMatrix Data.Packed.Internal.comp
    conj = liftMatrix Data.Packed.Internal.conj

instance Linear Matrix (Complex Double) where
    scale x = liftMatrix (scale x)
    scaleRecip x = liftMatrix (scaleRecip x)
    addConstant x = liftMatrix (addConstant x)
    add = liftMatrix2 add
    sub = liftMatrix2 sub
    mul = liftMatrix2 mul
    divide = liftMatrix2 divide
    toComplex = undefined
    fromComplex = undefined
    comp = undefined
    conj = undefined

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

