-----------------------------------------------------------------------------
{- |
Module      :  LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic matrix computations implemented by BLAS, LAPACK and GSL.

-}
-----------------------------------------------------------------------------
module LinearAlgebra (
    module Data.Packed.Vector,
    module Data.Packed.Matrix,
    module LinearAlgebra.Instances,
    module LinearAlgebra.Interface,
    module LinearAlgebra.Algorithms,
    module Complex
) where

import LinearAlgebra.Instances
import LinearAlgebra.Interface
import LinearAlgebra.Algorithms
import Data.Packed.Matrix
import Data.Packed.Vector
import Complex