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
    module Data.Packed,
    module LinearAlgebra.Linear,
    module LinearAlgebra.Algorithms,
    module LinearAlgebra.Instances,
    module LinearAlgebra.Interface
) where

import Data.Packed
import LinearAlgebra.Linear
import LinearAlgebra.Algorithms
import LinearAlgebra.Instances
import LinearAlgebra.Interface
