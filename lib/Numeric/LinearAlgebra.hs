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
    module Numeric.LinearAlgebra.Algorithms,
--    module Numeric.LinearAlgebra.Interface,
    module Numeric.Matrix,
    module Data.Packed.Random
) where

import Numeric.LinearAlgebra.Algorithms
--import Numeric.LinearAlgebra.Interface
import Numeric.Matrix
import Data.Packed.Random
