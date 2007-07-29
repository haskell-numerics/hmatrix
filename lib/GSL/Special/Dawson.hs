------------------------------------------------------------
{- |
Module      :  GSL.Special.Dawson
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Dawson(
  dawson_e
, dawson
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_dawson_e(double x,gsl_sf_result* result);
dawson_e :: Double -> (Double,Double)
dawson_e x = createSFR "dawson_e" $ gsl_sf_dawson_e x
foreign import ccall "dawson.h gsl_sf_dawson_e" gsl_sf_dawson_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_dawson(double x);
dawson :: Double -> Double
dawson = gsl_sf_dawson
foreign import ccall "dawson.h gsl_sf_dawson" gsl_sf_dawson :: Double -> Double
