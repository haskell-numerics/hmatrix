------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Dawson
-- Copyright   :  (c) Alberto Ruiz 2006-11
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_dawson.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Dawson(
  dawson_e
, dawson
) where

import Foreign(Ptr)
import Foreign.C.Types
import Numeric.GSL.Special.Internal

dawson_e :: Double -> (Double,Double)
dawson_e x = createSFR "dawson_e" $ gsl_sf_dawson_e x
foreign import ccall SAFE_CHEAP "gsl_sf_dawson_e" gsl_sf_dawson_e :: Double -> Ptr () -> IO CInt

dawson :: Double -> Double
dawson = gsl_sf_dawson
foreign import ccall SAFE_CHEAP "gsl_sf_dawson" gsl_sf_dawson :: Double -> Double
