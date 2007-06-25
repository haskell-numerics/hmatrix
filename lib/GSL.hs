{- |

Module      :  GSL
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

This module reexports the basic functionality and a collection of utilities (old interface)

-}

module GSL (

module Data.Packed.Vector,
module Data.Packed.Matrix,
module Data.Packed.Tensor,
module Data.Packed.Instances,
module LinearAlgebra.Algorithms,
module LAPACK,
module GSL.Integration,
module GSL.Differentiation,
module GSL.Special,
module GSL.Fourier,
module GSL.Polynomials,
module GSL.Minimization,
module Data.Packed.Plot

) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.Tensor
import Data.Packed.Instances
import LinearAlgebra.Algorithms
import LAPACK
import GSL.Integration
import GSL.Differentiation
import GSL.Special
import GSL.Fourier
import GSL.Polynomials
import GSL.Minimization
import Data.Packed.Plot
