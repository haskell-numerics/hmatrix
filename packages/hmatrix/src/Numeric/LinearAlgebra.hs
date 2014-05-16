-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  GPL

Maintainer  :  Alberto Ruiz
Stability   :  provisional


This module reexports all normally required functions for Linear Algebra applications.

It also provides instances of standard classes 'Show', 'Read', 'Eq',
'Num', 'Fractional', and 'Floating' for 'Vector' and 'Matrix'.
In arithmetic operations one-component vectors and matrices automatically
expand to match the dimensions of the other operand.

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra (
    module Numeric.Container,
    module Numeric.LinearAlgebra.Algorithms,
    module Numeric.LinearAlgebra.IO,
    module Numeric.LinearAlgebra.Random
) where

import Numeric.Container
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Util()
import Numeric.LinearAlgebra.IO
import Numeric.LinearAlgebra.Random

