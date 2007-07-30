------------------------------------------------------------
{- |
Module      :  GSL.Special.Lambert
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Lambert-W-Functions.html>

-}
------------------------------------------------------------

module GSL.Special.Lambert(
  lambert_W0_e
, lambert_W0
, lambert_Wm1_e
, lambert_Wm1
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_lambert_W0_e(double x,gsl_sf_result* result);
lambert_W0_e :: Double -> (Double,Double)
lambert_W0_e x = createSFR "lambert_W0_e" $ gsl_sf_lambert_W0_e x
foreign import ccall "lambert.h gsl_sf_lambert_W0_e" gsl_sf_lambert_W0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lambert_W0(double x);
lambert_W0 :: Double -> Double
lambert_W0 = gsl_sf_lambert_W0
foreign import ccall "lambert.h gsl_sf_lambert_W0" gsl_sf_lambert_W0 :: Double -> Double

-- | wrapper for int gsl_sf_lambert_Wm1_e(double x,gsl_sf_result* result);
lambert_Wm1_e :: Double -> (Double,Double)
lambert_Wm1_e x = createSFR "lambert_Wm1_e" $ gsl_sf_lambert_Wm1_e x
foreign import ccall "lambert.h gsl_sf_lambert_Wm1_e" gsl_sf_lambert_Wm1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lambert_Wm1(double x);
lambert_Wm1 :: Double -> Double
lambert_Wm1 = gsl_sf_lambert_Wm1
foreign import ccall "lambert.h gsl_sf_lambert_Wm1" gsl_sf_lambert_Wm1 :: Double -> Double
