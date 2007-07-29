------------------------------------------------------------
{- |
Module      :  GSL.Special.Elementary
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Elementary(
  multiply_e
, multiply
, multiply_err_e
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_multiply_e(double x,double y,gsl_sf_result* result);
multiply_e :: Double -> Double -> (Double,Double)
multiply_e x y = createSFR "multiply_e" $ gsl_sf_multiply_e x y
foreign import ccall "elementary.h gsl_sf_multiply_e" gsl_sf_multiply_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_multiply(double x,double y);
multiply :: Double -> Double -> Double
multiply = gsl_sf_multiply
foreign import ccall "elementary.h gsl_sf_multiply" gsl_sf_multiply :: Double -> Double -> Double

-- | wrapper for int gsl_sf_multiply_err_e(double x,double dx,double y,double dy,gsl_sf_result* result);
multiply_err_e :: Double -> Double -> Double -> Double -> (Double,Double)
multiply_err_e x dx y dy = createSFR "multiply_err_e" $ gsl_sf_multiply_err_e x dx y dy
foreign import ccall "elementary.h gsl_sf_multiply_err_e" gsl_sf_multiply_err_e :: Double -> Double -> Double -> Double -> Ptr Double -> IO(Int)
