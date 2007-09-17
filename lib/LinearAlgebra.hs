-----------------------------------------------------------------------------
{- |
Module      :  LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms, implemented by means of BLAS, LAPACK or GSL.

-}
-----------------------------------------------------------------------------
module LinearAlgebra (
    module Data.Packed.Vector,
    module Data.Packed.Matrix,
    module LinearAlgebra.Linear,
    module LinearAlgebra.Instances,
    module LinearAlgebra.Interface,
    module LAPACK,
    module GSL.Matrix,
    module LinearAlgebra.Algorithms,
    module Complex
) where

import LinearAlgebra.Linear
import LinearAlgebra.Instances
import LinearAlgebra.Interface
import LinearAlgebra.Algorithms
import LAPACK
import GSL.Matrix
import Data.Packed.Matrix
import Data.Packed.Vector
import Complex