------------------------------------------------------------
{- |
Module      :  GSL.Special.Zeta
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi



-}
------------------------------------------------------------

module GSL.Special.Zeta(
  zeta_int_e
, zeta_int
, zeta_e
, zeta
, zetam1_e
, zetam1
, zetam1_int_e
, zetam1_int
, hzeta_e
, hzeta
, eta_int_e
, eta_int
, eta_e
, eta
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_zeta_int_e(int n,gsl_sf_result* result);
zeta_int_e :: Int -> (Double,Double)
zeta_int_e n = createSFR "zeta_int_e" $ gsl_sf_zeta_int_e n
foreign import ccall "zeta.h gsl_sf_zeta_int_e" gsl_sf_zeta_int_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_zeta_int(int n);
zeta_int :: Int -> Double
zeta_int = gsl_sf_zeta_int
foreign import ccall "zeta.h gsl_sf_zeta_int" gsl_sf_zeta_int :: Int -> Double

-- | wrapper for int gsl_sf_zeta_e(double s,gsl_sf_result* result);
zeta_e :: Double -> (Double,Double)
zeta_e s = createSFR "zeta_e" $ gsl_sf_zeta_e s
foreign import ccall "zeta.h gsl_sf_zeta_e" gsl_sf_zeta_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_zeta(double s);
zeta :: Double -> Double
zeta = gsl_sf_zeta
foreign import ccall "zeta.h gsl_sf_zeta" gsl_sf_zeta :: Double -> Double

-- | wrapper for int gsl_sf_zetam1_e(double s,gsl_sf_result* result);
zetam1_e :: Double -> (Double,Double)
zetam1_e s = createSFR "zetam1_e" $ gsl_sf_zetam1_e s
foreign import ccall "zeta.h gsl_sf_zetam1_e" gsl_sf_zetam1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_zetam1(double s);
zetam1 :: Double -> Double
zetam1 = gsl_sf_zetam1
foreign import ccall "zeta.h gsl_sf_zetam1" gsl_sf_zetam1 :: Double -> Double

-- | wrapper for int gsl_sf_zetam1_int_e(int s,gsl_sf_result* result);
zetam1_int_e :: Int -> (Double,Double)
zetam1_int_e s = createSFR "zetam1_int_e" $ gsl_sf_zetam1_int_e s
foreign import ccall "zeta.h gsl_sf_zetam1_int_e" gsl_sf_zetam1_int_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_zetam1_int(int s);
zetam1_int :: Int -> Double
zetam1_int = gsl_sf_zetam1_int
foreign import ccall "zeta.h gsl_sf_zetam1_int" gsl_sf_zetam1_int :: Int -> Double

-- | wrapper for int gsl_sf_hzeta_e(double s,double q,gsl_sf_result* result);
hzeta_e :: Double -> Double -> (Double,Double)
hzeta_e s q = createSFR "hzeta_e" $ gsl_sf_hzeta_e s q
foreign import ccall "zeta.h gsl_sf_hzeta_e" gsl_sf_hzeta_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hzeta(double s,double q);
hzeta :: Double -> Double -> Double
hzeta = gsl_sf_hzeta
foreign import ccall "zeta.h gsl_sf_hzeta" gsl_sf_hzeta :: Double -> Double -> Double

-- | wrapper for int gsl_sf_eta_int_e(int n,gsl_sf_result* result);
eta_int_e :: Int -> (Double,Double)
eta_int_e n = createSFR "eta_int_e" $ gsl_sf_eta_int_e n
foreign import ccall "zeta.h gsl_sf_eta_int_e" gsl_sf_eta_int_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_eta_int(int n);
eta_int :: Int -> Double
eta_int = gsl_sf_eta_int
foreign import ccall "zeta.h gsl_sf_eta_int" gsl_sf_eta_int :: Int -> Double

-- | wrapper for int gsl_sf_eta_e(double s,gsl_sf_result* result);
eta_e :: Double -> (Double,Double)
eta_e s = createSFR "eta_e" $ gsl_sf_eta_e s
foreign import ccall "zeta.h gsl_sf_eta_e" gsl_sf_eta_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_eta(double s);
eta :: Double -> Double
eta = gsl_sf_eta
foreign import ccall "zeta.h gsl_sf_eta" gsl_sf_eta :: Double -> Double
