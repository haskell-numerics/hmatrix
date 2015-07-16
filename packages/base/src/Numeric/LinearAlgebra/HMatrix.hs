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
    (¦),(——),ℝ,ℂ,(<·>),app,mul, cholSH, mbCholSH, eigSH', eigenvaluesSH', geigSH'
) where

import Numeric.LinearAlgebra
import Internal.Util
import Internal.Algorithms(cholSH, mbCholSH, eigSH', eigenvaluesSH', geigSH')

infixr 8 <·>
(<·>) :: Numeric t => Vector t -> Vector t -> t
(<·>) = dot

app m v = m #> v

mul a b = a <> b

