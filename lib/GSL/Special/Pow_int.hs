------------------------------------------------------------
{- |
Module      :  GSL.Special.Pow_int
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Pow_int(
  pow_int_e
, pow_int
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_pow_int_e(double x,int n,gsl_sf_result* result);
pow_int_e :: Double -> Int -> (Double,Double)
pow_int_e x n = createSFR "pow_int_e" $ gsl_sf_pow_int_e x n
foreign import ccall "pow_int.h gsl_sf_pow_int_e" gsl_sf_pow_int_e :: Double -> Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_pow_int(double x,int n);
pow_int :: Double -> Int -> Double
pow_int = gsl_sf_pow_int
foreign import ccall "pow_int.h gsl_sf_pow_int" gsl_sf_pow_int :: Double -> Int -> Double
