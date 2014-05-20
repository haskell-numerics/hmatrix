-----------------------------------------------------------------------------
{- |
Module      :  Numeric.Container
Copyright   :  (c) Alberto Ruiz 2006-14
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Numeric.Container (
    module Data.Packed.Numeric,
    module Numeric.LinearAlgebra.IO,
    module Numeric.LinearAlgebra.Random,
    meanCov
) where

import Data.Packed.Numeric hiding (saveMatrix, loadMatrix)
import Numeric.LinearAlgebra.IO
import Numeric.LinearAlgebra.Random hiding (Seed)
import Numeric.LinearAlgebra.Util(meanCov)

