{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

infixr 8 <·>
(<·>) :: Numeric t => Vector t -> Vector t -> t
(<·>) = dot

app :: Numeric t => Matrix t -> Vector t -> Vector t
app m v = m #> v

mul :: Numeric t => Matrix t -> Matrix t -> Matrix t
mul a b = a <> b

