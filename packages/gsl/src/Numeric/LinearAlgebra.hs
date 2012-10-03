-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

This module reexports all normally required functions for Linear Algebra applications.

It also provides instances of standard classes 'Show', 'Read', 'Eq',
'Num', 'Fractional', and 'Floating' for 'Vector' and 'Matrix'.
In arithmetic operations one-component vectors and matrices automatically
expand to match the dimensions of the other operand.

-}
-----------------------------------------------------------------------------
module Numeric.LinearAlgebra (
    module Numeric.LinearAlgebra.Base,
    module Numeric.GSL.Random,
    module Numeric.GSL.Vector
) where

import Numeric.LinearAlgebra.Base
import Numeric.GSL.Vector hiding (RandDist(..), randomVector)
import Numeric.GSL.Random hiding (rand, randn)

