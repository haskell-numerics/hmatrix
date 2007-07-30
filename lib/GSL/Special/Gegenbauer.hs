------------------------------------------------------------
{- |
Module      :  GSL.Special.Gegenbauer
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Gegenbauer-Functions.html>

-}
------------------------------------------------------------

module GSL.Special.Gegenbauer(
  gegenpoly_1_e
, gegenpoly_2_e
, gegenpoly_3_e
, gegenpoly_1
, gegenpoly_2
, gegenpoly_3
, gegenpoly_n_e
, gegenpoly_n
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_gegenpoly_1_e(double lambda,double x,gsl_sf_result* result);
gegenpoly_1_e :: Double -> Double -> (Double,Double)
gegenpoly_1_e lambda x = createSFR "gegenpoly_1_e" $ gsl_sf_gegenpoly_1_e lambda x
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_1_e" gsl_sf_gegenpoly_1_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_gegenpoly_2_e(double lambda,double x,gsl_sf_result* result);
gegenpoly_2_e :: Double -> Double -> (Double,Double)
gegenpoly_2_e lambda x = createSFR "gegenpoly_2_e" $ gsl_sf_gegenpoly_2_e lambda x
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_2_e" gsl_sf_gegenpoly_2_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_gegenpoly_3_e(double lambda,double x,gsl_sf_result* result);
gegenpoly_3_e :: Double -> Double -> (Double,Double)
gegenpoly_3_e lambda x = createSFR "gegenpoly_3_e" $ gsl_sf_gegenpoly_3_e lambda x
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_3_e" gsl_sf_gegenpoly_3_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gegenpoly_1(double lambda,double x);
gegenpoly_1 :: Double -> Double -> Double
gegenpoly_1 = gsl_sf_gegenpoly_1
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_1" gsl_sf_gegenpoly_1 :: Double -> Double -> Double

-- | wrapper for double gsl_sf_gegenpoly_2(double lambda,double x);
gegenpoly_2 :: Double -> Double -> Double
gegenpoly_2 = gsl_sf_gegenpoly_2
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_2" gsl_sf_gegenpoly_2 :: Double -> Double -> Double

-- | wrapper for double gsl_sf_gegenpoly_3(double lambda,double x);
gegenpoly_3 :: Double -> Double -> Double
gegenpoly_3 = gsl_sf_gegenpoly_3
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_3" gsl_sf_gegenpoly_3 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_gegenpoly_n_e(int n,double lambda,double x,gsl_sf_result* result);
gegenpoly_n_e :: Int -> Double -> Double -> (Double,Double)
gegenpoly_n_e n lambda x = createSFR "gegenpoly_n_e" $ gsl_sf_gegenpoly_n_e n lambda x
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_n_e" gsl_sf_gegenpoly_n_e :: Int -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gegenpoly_n(int n,double lambda,double x);
gegenpoly_n :: Int -> Double -> Double -> Double
gegenpoly_n = gsl_sf_gegenpoly_n
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_n" gsl_sf_gegenpoly_n :: Int -> Double -> Double -> Double

-- | wrapper for int gsl_sf_gegenpoly_array(int nmax,double lambda,double x,double* result_array);
gegenpoly_array :: Int -> Double -> Double -> Ptr Double -> Int
gegenpoly_array = gsl_sf_gegenpoly_array
foreign import ccall "gegenbauer.h gsl_sf_gegenpoly_array" gsl_sf_gegenpoly_array :: Int -> Double -> Double -> Ptr Double -> Int
