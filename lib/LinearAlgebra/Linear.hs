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
    Linear(..)
) where


import Data.Packed.Internal
import GSL.Vector
import Complex


class (Num e, Field e) => Linear c e where
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
