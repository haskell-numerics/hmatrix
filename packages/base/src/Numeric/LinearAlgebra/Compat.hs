-----------------------------------------------------------------------------
{- |
Module      :  Numeric.LinearAlgebra.Compat
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK hide #-}

module Numeric.LinearAlgebra.Compat (
    module Numeric.Container,
    module Numeric.LinearAlgebra.Algorithms,
    meanCov
) where

import Numeric.Container
import Numeric.LinearAlgebra.Algorithms
import Numeric.Matrix()
import Numeric.Vector()
import Numeric.LinearAlgebra.Util(meanCov)


