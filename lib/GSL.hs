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
module LinearAlgebra.Algorithms,
module LAPACK,
module GSL.Integration,
module GSL.Differentiation,
module GSL.Special,
module GSL.Fourier,
module GSL.Polynomials,
module GSL.Minimization,
module GSL.Matrix,
module GSL.Compat,
module Data.Packed.Plot,
module Complex

) where

import Data.Packed.Vector hiding (constant)
import Data.Packed.Matrix hiding ((><), multiply)
import Data.Packed.Tensor
import LinearAlgebra.Algorithms hiding (pnorm)
import LAPACK
import GSL.Integration
import GSL.Differentiation
import GSL.Special
import GSL.Fourier
import GSL.Polynomials
import GSL.Minimization
import GSL.Matrix
import GSL.Compat
import Data.Packed.Plot
import Complex
