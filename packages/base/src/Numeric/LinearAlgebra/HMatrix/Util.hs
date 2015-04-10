-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.HMatrix.Util
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Numeric.LinearAlgebra.HMatrix.Util(
    -- * Tools for the Kronecker product
    --
    -- | (see A. Fusiello, A matter of notation: Several uses of the Kronecker product in
    --  3d computer vision, Pattern Recognition Letters 28 (15) (2007) 2127-2132)

    --
    -- | @`vec` (a \<> x \<> b) == ('trans' b ` 'kronecker' ` a) \<> 'vec' x@
    vec,
    vech,
    dup,
    vtrans
) where

import Numeric.LinearAlgebra.Util

