------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Elementary
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_elementary.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Elementary(
  multiply_e
, multiply
, multiply_err_e
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

multiply_e :: Double -> Double -> (Double,Double)
multiply_e x y = createSFR "multiply_e" $ gsl_sf_multiply_e x y
foreign import ccall "gsl_sf_multiply_e" gsl_sf_multiply_e :: Double -> Double -> Ptr () -> IO CInt

multiply :: Double -> Double -> Double
multiply = gsl_sf_multiply
foreign import ccall "gsl_sf_multiply" gsl_sf_multiply :: Double -> Double -> Double

multiply_err_e :: Double -> Double -> Double -> Double -> (Double,Double)
multiply_err_e x dx y dy = createSFR "multiply_err_e" $ gsl_sf_multiply_err_e x dx y dy
foreign import ccall "gsl_sf_multiply_err_e" gsl_sf_multiply_err_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt
