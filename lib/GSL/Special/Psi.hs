------------------------------------------------------------
{- |
Module      :  GSL.Special.Psi
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Psi(
  psi_int_e
, psi_int
, psi_e
, psi
, psi_1piy_e
, psi_1piy
, psi_1_int_e
, psi_1_int
, psi_1_e
, psi_1
, psi_n_e
, psi_n
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_psi_int_e(int n,gsl_sf_result* result);
psi_int_e :: Int -> (Double,Double)
psi_int_e n = createSFR "psi_int_e" $ gsl_sf_psi_int_e n
foreign import ccall "psi.h gsl_sf_psi_int_e" gsl_sf_psi_int_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_psi_int(int n);
psi_int :: Int -> Double
psi_int = gsl_sf_psi_int
foreign import ccall "psi.h gsl_sf_psi_int" gsl_sf_psi_int :: Int -> Double

-- | wrapper for int gsl_sf_psi_e(double x,gsl_sf_result* result);
psi_e :: Double -> (Double,Double)
psi_e x = createSFR "psi_e" $ gsl_sf_psi_e x
foreign import ccall "psi.h gsl_sf_psi_e" gsl_sf_psi_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_psi(double x);
psi :: Double -> Double
psi = gsl_sf_psi
foreign import ccall "psi.h gsl_sf_psi" gsl_sf_psi :: Double -> Double

-- | wrapper for int gsl_sf_psi_1piy_e(double y,gsl_sf_result* result);
psi_1piy_e :: Double -> (Double,Double)
psi_1piy_e y = createSFR "psi_1piy_e" $ gsl_sf_psi_1piy_e y
foreign import ccall "psi.h gsl_sf_psi_1piy_e" gsl_sf_psi_1piy_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_psi_1piy(double y);
psi_1piy :: Double -> Double
psi_1piy = gsl_sf_psi_1piy
foreign import ccall "psi.h gsl_sf_psi_1piy" gsl_sf_psi_1piy :: Double -> Double

-- | wrapper for int gsl_sf_psi_1_int_e(int n,gsl_sf_result* result);
psi_1_int_e :: Int -> (Double,Double)
psi_1_int_e n = createSFR "psi_1_int_e" $ gsl_sf_psi_1_int_e n
foreign import ccall "psi.h gsl_sf_psi_1_int_e" gsl_sf_psi_1_int_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_psi_1_int(int n);
psi_1_int :: Int -> Double
psi_1_int = gsl_sf_psi_1_int
foreign import ccall "psi.h gsl_sf_psi_1_int" gsl_sf_psi_1_int :: Int -> Double

-- | wrapper for int gsl_sf_psi_1_e(double x,gsl_sf_result* result);
psi_1_e :: Double -> (Double,Double)
psi_1_e x = createSFR "psi_1_e" $ gsl_sf_psi_1_e x
foreign import ccall "psi.h gsl_sf_psi_1_e" gsl_sf_psi_1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_psi_1(double x);
psi_1 :: Double -> Double
psi_1 = gsl_sf_psi_1
foreign import ccall "psi.h gsl_sf_psi_1" gsl_sf_psi_1 :: Double -> Double

-- | wrapper for int gsl_sf_psi_n_e(int n,double x,gsl_sf_result* result);
psi_n_e :: Int -> Double -> (Double,Double)
psi_n_e n x = createSFR "psi_n_e" $ gsl_sf_psi_n_e n x
foreign import ccall "psi.h gsl_sf_psi_n_e" gsl_sf_psi_n_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_psi_n(int n,double x);
psi_n :: Int -> Double -> Double
psi_n = gsl_sf_psi_n
foreign import ccall "psi.h gsl_sf_psi_n" gsl_sf_psi_n :: Int -> Double -> Double
