------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Dawson
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_dawson.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Dawson(
  dawson_e
, dawson
) where

import Foreign(Ptr)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_dawson_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_dawson_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
dawson_e :: Double -> (Double,Double)
dawson_e x = createSFR "dawson_e" $ gsl_sf_dawson_e x
foreign import ccall "dawson.h gsl_sf_dawson_e" gsl_sf_dawson_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_dawson(double x);
--
--   <http://www.google.com/search?q=gsl_sf_dawson&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
dawson :: Double -> Double
dawson = gsl_sf_dawson
foreign import ccall "dawson.h gsl_sf_dawson" gsl_sf_dawson :: Double -> Double
