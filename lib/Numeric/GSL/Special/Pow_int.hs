------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Pow_int
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_pow_int.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Pow_int(
  pow_int_e
, pow_int
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

pow_int_e :: Double -> CInt -> (Double,Double)
pow_int_e x n = createSFR "pow_int_e" $ gsl_sf_pow_int_e x n
foreign import ccall "gsl_sf_pow_int_e" gsl_sf_pow_int_e :: Double -> CInt -> Ptr () -> IO CInt

pow_int :: Double -> CInt -> Double
pow_int = gsl_sf_pow_int
foreign import ccall "gsl_sf_pow_int" gsl_sf_pow_int :: Double -> CInt -> Double
