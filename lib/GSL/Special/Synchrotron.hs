------------------------------------------------------------
{- |
Module      :  GSL.Special.Synchrotron
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Synchrotron(
  synchrotron_1_e
, synchrotron_1
, synchrotron_2_e
, synchrotron_2
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_synchrotron_1_e(double x,gsl_sf_result* result);
synchrotron_1_e :: Double -> (Double,Double)
synchrotron_1_e x = createSFR "synchrotron_1_e" $ gsl_sf_synchrotron_1_e x
foreign import ccall "synchrotron.h gsl_sf_synchrotron_1_e" gsl_sf_synchrotron_1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_synchrotron_1(double x);
synchrotron_1 :: Double -> Double
synchrotron_1 = gsl_sf_synchrotron_1
foreign import ccall "synchrotron.h gsl_sf_synchrotron_1" gsl_sf_synchrotron_1 :: Double -> Double

-- | wrapper for int gsl_sf_synchrotron_2_e(double x,gsl_sf_result* result);
synchrotron_2_e :: Double -> (Double,Double)
synchrotron_2_e x = createSFR "synchrotron_2_e" $ gsl_sf_synchrotron_2_e x
foreign import ccall "synchrotron.h gsl_sf_synchrotron_2_e" gsl_sf_synchrotron_2_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_synchrotron_2(double x);
synchrotron_2 :: Double -> Double
synchrotron_2 = gsl_sf_synchrotron_2
foreign import ccall "synchrotron.h gsl_sf_synchrotron_2" gsl_sf_synchrotron_2 :: Double -> Double
