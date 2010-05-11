-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-9
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

This module reexports all normally required functions for Linear Algebra applications.

-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra (
    module Data.Packed,
    module Data.Packed.Random,
    module Numeric.LinearAlgebra.Algorithms,
    module Numeric.LinearAlgebra.Interface
) where

import Data.Packed
import Data.Packed.Random
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Instances()
import Numeric.LinearAlgebra.Interface
