------------------------------------------------------------
{- |
Module      :  GSL.Special.Debye
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Debye(
  debye_1_e
, debye_1
, debye_2_e
, debye_2
, debye_3_e
, debye_3
, debye_4_e
, debye_4
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_debye_1_e(double x,gsl_sf_result* result);
debye_1_e :: Double -> (Double,Double)
debye_1_e x = createSFR "debye_1_e" $ gsl_sf_debye_1_e x
foreign import ccall "debye.h gsl_sf_debye_1_e" gsl_sf_debye_1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_debye_1(double x);
debye_1 :: Double -> Double
debye_1 = gsl_sf_debye_1
foreign import ccall "debye.h gsl_sf_debye_1" gsl_sf_debye_1 :: Double -> Double

-- | wrapper for int gsl_sf_debye_2_e(double x,gsl_sf_result* result);
debye_2_e :: Double -> (Double,Double)
debye_2_e x = createSFR "debye_2_e" $ gsl_sf_debye_2_e x
foreign import ccall "debye.h gsl_sf_debye_2_e" gsl_sf_debye_2_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_debye_2(double x);
debye_2 :: Double -> Double
debye_2 = gsl_sf_debye_2
foreign import ccall "debye.h gsl_sf_debye_2" gsl_sf_debye_2 :: Double -> Double

-- | wrapper for int gsl_sf_debye_3_e(double x,gsl_sf_result* result);
debye_3_e :: Double -> (Double,Double)
debye_3_e x = createSFR "debye_3_e" $ gsl_sf_debye_3_e x
foreign import ccall "debye.h gsl_sf_debye_3_e" gsl_sf_debye_3_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_debye_3(double x);
debye_3 :: Double -> Double
debye_3 = gsl_sf_debye_3
foreign import ccall "debye.h gsl_sf_debye_3" gsl_sf_debye_3 :: Double -> Double

-- | wrapper for int gsl_sf_debye_4_e(double x,gsl_sf_result* result);
debye_4_e :: Double -> (Double,Double)
debye_4_e x = createSFR "debye_4_e" $ gsl_sf_debye_4_e x
foreign import ccall "debye.h gsl_sf_debye_4_e" gsl_sf_debye_4_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_debye_4(double x);
debye_4 :: Double -> Double
debye_4 = gsl_sf_debye_4
foreign import ccall "debye.h gsl_sf_debye_4" gsl_sf_debye_4 :: Double -> Double
