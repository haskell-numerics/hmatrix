--------------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.HMatrix
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

compatibility with previous version, to be removed

-}
--------------------------------------------------------------------------------

module Numeric.LinearAlgebra.HMatrix (
    module Numeric.LinearAlgebra,
    (¦),(——),ℝ,ℂ,(<·>),app,mul
) where

import Numeric.LinearAlgebra
import Internal.Util

infixr 8 <·>
(<·>) :: Numeric t => Vector t -> Vector t -> t
(<·>) = dot

app m v = m #> v

mul a b = a <> b

