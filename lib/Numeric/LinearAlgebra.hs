-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-9
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic matrix computations implemented by BLAS, LAPACK and GSL.

This module reexports the most comon functions (including "Numeric.LinearAlgebra.Instances").

-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra (
    module Data.Packed,
    module Data.Packed.Random,
    module Numeric.LinearAlgebra.Linear,
    module Numeric.LinearAlgebra.Algorithms,
    module Numeric.LinearAlgebra.Interface
) where

import Data.Packed
import Data.Packed.Random
import Numeric.LinearAlgebra.Linear
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Instances()
import Numeric.LinearAlgebra.Interface
