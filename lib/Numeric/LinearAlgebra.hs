-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic matrix computations implemented by BLAS, LAPACK and GSL.

-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra (
    module Data.Packed,
    module Numeric.LinearAlgebra.Linear,
    module Numeric.LinearAlgebra.Algorithms,
    module Numeric.LinearAlgebra.Instances,
    module Numeric.LinearAlgebra.Interface
) where

import Data.Packed
import Numeric.LinearAlgebra.Linear
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Instances
import Numeric.LinearAlgebra.Interface
