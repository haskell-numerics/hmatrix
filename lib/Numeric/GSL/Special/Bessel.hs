------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Bessel
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_bessel.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Bessel(
  bessel_J0_e
, bessel_J0
, bessel_J1_e
, bessel_J1
, bessel_Jn_e
, bessel_Jn
, bessel_Y0_e
, bessel_Y0
, bessel_Y1_e
, bessel_Y1
, bessel_Yn_e
, bessel_Yn
, bessel_I0_e
, bessel_I0
, bessel_I1_e
, bessel_I1
, bessel_In_e
, bessel_In
, bessel_I0_scaled_e
, bessel_I0_scaled
, bessel_I1_scaled_e
, bessel_I1_scaled
, bessel_In_scaled_e
, bessel_In_scaled
, bessel_K0_e
, bessel_K0
, bessel_K1_e
, bessel_K1
, bessel_Kn_e
, bessel_Kn
, bessel_K0_scaled_e
, bessel_K0_scaled
, bessel_K1_scaled_e
, bessel_K1_scaled
, bessel_Kn_scaled_e
, bessel_Kn_scaled
, bessel_j0_e
, bessel_j0
, bessel_j1_e
, bessel_j1
, bessel_j2_e
, bessel_j2
, bessel_jl_e
, bessel_jl
, bessel_y0_e
, bessel_y0
, bessel_y1_e
, bessel_y1
, bessel_y2_e
, bessel_y2
, bessel_yl_e
, bessel_yl
, bessel_i0_scaled_e
, bessel_i0_scaled
, bessel_i1_scaled_e
, bessel_i1_scaled
, bessel_i2_scaled_e
, bessel_i2_scaled
, bessel_il_scaled_e
, bessel_il_scaled
, bessel_k0_scaled_e
, bessel_k0_scaled
, bessel_k1_scaled_e
, bessel_k1_scaled
, bessel_k2_scaled_e
, bessel_k2_scaled
, bessel_kl_scaled_e
, bessel_kl_scaled
, bessel_Jnu_e
, bessel_Jnu
, bessel_Ynu_e
, bessel_Ynu
, bessel_Inu_scaled_e
, bessel_Inu_scaled
, bessel_Inu_e
, bessel_Inu
, bessel_Knu_scaled_e
, bessel_Knu_scaled
, bessel_Knu_e
, bessel_Knu
, bessel_lnKnu_e
, bessel_lnKnu
, bessel_zero_J0_e
, bessel_zero_J0
, bessel_zero_J1_e
, bessel_zero_J1
, bessel_zero_Jnu_e
, bessel_zero_Jnu
) where

import Foreign(Ptr)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_bessel_J0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_J0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_J0_e :: Double -> (Double,Double)
bessel_J0_e x = createSFR "bessel_J0_e" $ gsl_sf_bessel_J0_e x
foreign import ccall "bessel.h gsl_sf_bessel_J0_e" gsl_sf_bessel_J0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_J0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_J0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_J0 :: Double -> Double
bessel_J0 = gsl_sf_bessel_J0
foreign import ccall "bessel.h gsl_sf_bessel_J0" gsl_sf_bessel_J0 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_J1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_J1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_J1_e :: Double -> (Double,Double)
bessel_J1_e x = createSFR "bessel_J1_e" $ gsl_sf_bessel_J1_e x
foreign import ccall "bessel.h gsl_sf_bessel_J1_e" gsl_sf_bessel_J1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_J1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_J1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_J1 :: Double -> Double
bessel_J1 = gsl_sf_bessel_J1
foreign import ccall "bessel.h gsl_sf_bessel_J1" gsl_sf_bessel_J1 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_Jn_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Jn_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Jn_e :: Int -> Double -> (Double,Double)
bessel_Jn_e n x = createSFR "bessel_Jn_e" $ gsl_sf_bessel_Jn_e n x
foreign import ccall "bessel.h gsl_sf_bessel_Jn_e" gsl_sf_bessel_Jn_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Jn(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Jn&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Jn :: Int -> Double -> Double
bessel_Jn = gsl_sf_bessel_Jn
foreign import ccall "bessel.h gsl_sf_bessel_Jn" gsl_sf_bessel_Jn :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Jn_array(int nmin,int nmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Jn_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Jn_array :: Int -> Int -> Double -> Ptr Double -> Int
bessel_Jn_array = gsl_sf_bessel_Jn_array
foreign import ccall "bessel.h gsl_sf_bessel_Jn_array" gsl_sf_bessel_Jn_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_Y0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Y0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Y0_e :: Double -> (Double,Double)
bessel_Y0_e x = createSFR "bessel_Y0_e" $ gsl_sf_bessel_Y0_e x
foreign import ccall "bessel.h gsl_sf_bessel_Y0_e" gsl_sf_bessel_Y0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Y0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Y0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Y0 :: Double -> Double
bessel_Y0 = gsl_sf_bessel_Y0
foreign import ccall "bessel.h gsl_sf_bessel_Y0" gsl_sf_bessel_Y0 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_Y1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Y1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Y1_e :: Double -> (Double,Double)
bessel_Y1_e x = createSFR "bessel_Y1_e" $ gsl_sf_bessel_Y1_e x
foreign import ccall "bessel.h gsl_sf_bessel_Y1_e" gsl_sf_bessel_Y1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Y1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Y1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Y1 :: Double -> Double
bessel_Y1 = gsl_sf_bessel_Y1
foreign import ccall "bessel.h gsl_sf_bessel_Y1" gsl_sf_bessel_Y1 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_Yn_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Yn_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Yn_e :: Int -> Double -> (Double,Double)
bessel_Yn_e n x = createSFR "bessel_Yn_e" $ gsl_sf_bessel_Yn_e n x
foreign import ccall "bessel.h gsl_sf_bessel_Yn_e" gsl_sf_bessel_Yn_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Yn(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Yn&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Yn :: Int -> Double -> Double
bessel_Yn = gsl_sf_bessel_Yn
foreign import ccall "bessel.h gsl_sf_bessel_Yn" gsl_sf_bessel_Yn :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Yn_array(int nmin,int nmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Yn_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Yn_array :: Int -> Int -> Double -> Ptr Double -> Int
bessel_Yn_array = gsl_sf_bessel_Yn_array
foreign import ccall "bessel.h gsl_sf_bessel_Yn_array" gsl_sf_bessel_Yn_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_I0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I0_e :: Double -> (Double,Double)
bessel_I0_e x = createSFR "bessel_I0_e" $ gsl_sf_bessel_I0_e x
foreign import ccall "bessel.h gsl_sf_bessel_I0_e" gsl_sf_bessel_I0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_I0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I0 :: Double -> Double
bessel_I0 = gsl_sf_bessel_I0
foreign import ccall "bessel.h gsl_sf_bessel_I0" gsl_sf_bessel_I0 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_I1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I1_e :: Double -> (Double,Double)
bessel_I1_e x = createSFR "bessel_I1_e" $ gsl_sf_bessel_I1_e x
foreign import ccall "bessel.h gsl_sf_bessel_I1_e" gsl_sf_bessel_I1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_I1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I1 :: Double -> Double
bessel_I1 = gsl_sf_bessel_I1
foreign import ccall "bessel.h gsl_sf_bessel_I1" gsl_sf_bessel_I1 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_In_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_In_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_In_e :: Int -> Double -> (Double,Double)
bessel_In_e n x = createSFR "bessel_In_e" $ gsl_sf_bessel_In_e n x
foreign import ccall "bessel.h gsl_sf_bessel_In_e" gsl_sf_bessel_In_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_In(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_In&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_In :: Int -> Double -> Double
bessel_In = gsl_sf_bessel_In
foreign import ccall "bessel.h gsl_sf_bessel_In" gsl_sf_bessel_In :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_In_array(int nmin,int nmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_In_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_In_array :: Int -> Int -> Double -> Ptr Double -> Int
bessel_In_array = gsl_sf_bessel_In_array
foreign import ccall "bessel.h gsl_sf_bessel_In_array" gsl_sf_bessel_In_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_I0_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I0_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I0_scaled_e :: Double -> (Double,Double)
bessel_I0_scaled_e x = createSFR "bessel_I0_scaled_e" $ gsl_sf_bessel_I0_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_I0_scaled_e" gsl_sf_bessel_I0_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_I0_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I0_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I0_scaled :: Double -> Double
bessel_I0_scaled = gsl_sf_bessel_I0_scaled
foreign import ccall "bessel.h gsl_sf_bessel_I0_scaled" gsl_sf_bessel_I0_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_I1_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I1_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I1_scaled_e :: Double -> (Double,Double)
bessel_I1_scaled_e x = createSFR "bessel_I1_scaled_e" $ gsl_sf_bessel_I1_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_I1_scaled_e" gsl_sf_bessel_I1_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_I1_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_I1_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_I1_scaled :: Double -> Double
bessel_I1_scaled = gsl_sf_bessel_I1_scaled
foreign import ccall "bessel.h gsl_sf_bessel_I1_scaled" gsl_sf_bessel_I1_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_In_scaled_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_In_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_In_scaled_e :: Int -> Double -> (Double,Double)
bessel_In_scaled_e n x = createSFR "bessel_In_scaled_e" $ gsl_sf_bessel_In_scaled_e n x
foreign import ccall "bessel.h gsl_sf_bessel_In_scaled_e" gsl_sf_bessel_In_scaled_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_In_scaled(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_In_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_In_scaled :: Int -> Double -> Double
bessel_In_scaled = gsl_sf_bessel_In_scaled
foreign import ccall "bessel.h gsl_sf_bessel_In_scaled" gsl_sf_bessel_In_scaled :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_In_scaled_array(int nmin,int nmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_In_scaled_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_In_scaled_array :: Int -> Int -> Double -> Ptr Double -> Int
bessel_In_scaled_array = gsl_sf_bessel_In_scaled_array
foreign import ccall "bessel.h gsl_sf_bessel_In_scaled_array" gsl_sf_bessel_In_scaled_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_K0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K0_e :: Double -> (Double,Double)
bessel_K0_e x = createSFR "bessel_K0_e" $ gsl_sf_bessel_K0_e x
foreign import ccall "bessel.h gsl_sf_bessel_K0_e" gsl_sf_bessel_K0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_K0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K0 :: Double -> Double
bessel_K0 = gsl_sf_bessel_K0
foreign import ccall "bessel.h gsl_sf_bessel_K0" gsl_sf_bessel_K0 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_K1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K1_e :: Double -> (Double,Double)
bessel_K1_e x = createSFR "bessel_K1_e" $ gsl_sf_bessel_K1_e x
foreign import ccall "bessel.h gsl_sf_bessel_K1_e" gsl_sf_bessel_K1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_K1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K1 :: Double -> Double
bessel_K1 = gsl_sf_bessel_K1
foreign import ccall "bessel.h gsl_sf_bessel_K1" gsl_sf_bessel_K1 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_Kn_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Kn_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Kn_e :: Int -> Double -> (Double,Double)
bessel_Kn_e n x = createSFR "bessel_Kn_e" $ gsl_sf_bessel_Kn_e n x
foreign import ccall "bessel.h gsl_sf_bessel_Kn_e" gsl_sf_bessel_Kn_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Kn(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Kn&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Kn :: Int -> Double -> Double
bessel_Kn = gsl_sf_bessel_Kn
foreign import ccall "bessel.h gsl_sf_bessel_Kn" gsl_sf_bessel_Kn :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Kn_array(int nmin,int nmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Kn_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Kn_array :: Int -> Int -> Double -> Ptr Double -> Int
bessel_Kn_array = gsl_sf_bessel_Kn_array
foreign import ccall "bessel.h gsl_sf_bessel_Kn_array" gsl_sf_bessel_Kn_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_K0_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K0_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K0_scaled_e :: Double -> (Double,Double)
bessel_K0_scaled_e x = createSFR "bessel_K0_scaled_e" $ gsl_sf_bessel_K0_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_K0_scaled_e" gsl_sf_bessel_K0_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_K0_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K0_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K0_scaled :: Double -> Double
bessel_K0_scaled = gsl_sf_bessel_K0_scaled
foreign import ccall "bessel.h gsl_sf_bessel_K0_scaled" gsl_sf_bessel_K0_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_K1_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K1_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K1_scaled_e :: Double -> (Double,Double)
bessel_K1_scaled_e x = createSFR "bessel_K1_scaled_e" $ gsl_sf_bessel_K1_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_K1_scaled_e" gsl_sf_bessel_K1_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_K1_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_K1_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_K1_scaled :: Double -> Double
bessel_K1_scaled = gsl_sf_bessel_K1_scaled
foreign import ccall "bessel.h gsl_sf_bessel_K1_scaled" gsl_sf_bessel_K1_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_Kn_scaled_e(int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Kn_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Kn_scaled_e :: Int -> Double -> (Double,Double)
bessel_Kn_scaled_e n x = createSFR "bessel_Kn_scaled_e" $ gsl_sf_bessel_Kn_scaled_e n x
foreign import ccall "bessel.h gsl_sf_bessel_Kn_scaled_e" gsl_sf_bessel_Kn_scaled_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Kn_scaled(int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Kn_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Kn_scaled :: Int -> Double -> Double
bessel_Kn_scaled = gsl_sf_bessel_Kn_scaled
foreign import ccall "bessel.h gsl_sf_bessel_Kn_scaled" gsl_sf_bessel_Kn_scaled :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Kn_scaled_array(int nmin,int nmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Kn_scaled_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Kn_scaled_array :: Int -> Int -> Double -> Ptr Double -> Int
bessel_Kn_scaled_array = gsl_sf_bessel_Kn_scaled_array
foreign import ccall "bessel.h gsl_sf_bessel_Kn_scaled_array" gsl_sf_bessel_Kn_scaled_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_j0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_j0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_j0_e :: Double -> (Double,Double)
bessel_j0_e x = createSFR "bessel_j0_e" $ gsl_sf_bessel_j0_e x
foreign import ccall "bessel.h gsl_sf_bessel_j0_e" gsl_sf_bessel_j0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_j0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_j0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_j0 :: Double -> Double
bessel_j0 = gsl_sf_bessel_j0
foreign import ccall "bessel.h gsl_sf_bessel_j0" gsl_sf_bessel_j0 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_j1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_j1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_j1_e :: Double -> (Double,Double)
bessel_j1_e x = createSFR "bessel_j1_e" $ gsl_sf_bessel_j1_e x
foreign import ccall "bessel.h gsl_sf_bessel_j1_e" gsl_sf_bessel_j1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_j1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_j1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_j1 :: Double -> Double
bessel_j1 = gsl_sf_bessel_j1
foreign import ccall "bessel.h gsl_sf_bessel_j1" gsl_sf_bessel_j1 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_j2_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_j2_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_j2_e :: Double -> (Double,Double)
bessel_j2_e x = createSFR "bessel_j2_e" $ gsl_sf_bessel_j2_e x
foreign import ccall "bessel.h gsl_sf_bessel_j2_e" gsl_sf_bessel_j2_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_j2(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_j2&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_j2 :: Double -> Double
bessel_j2 = gsl_sf_bessel_j2
foreign import ccall "bessel.h gsl_sf_bessel_j2" gsl_sf_bessel_j2 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_jl_e(int l,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_jl_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_jl_e :: Int -> Double -> (Double,Double)
bessel_jl_e l x = createSFR "bessel_jl_e" $ gsl_sf_bessel_jl_e l x
foreign import ccall "bessel.h gsl_sf_bessel_jl_e" gsl_sf_bessel_jl_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_jl(int l,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_jl&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_jl :: Int -> Double -> Double
bessel_jl = gsl_sf_bessel_jl
foreign import ccall "bessel.h gsl_sf_bessel_jl" gsl_sf_bessel_jl :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_jl_array(int lmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_jl_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_jl_array :: Int -> Double -> Ptr Double -> Int
bessel_jl_array = gsl_sf_bessel_jl_array
foreign import ccall "bessel.h gsl_sf_bessel_jl_array" gsl_sf_bessel_jl_array :: Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_jl_steed_array(int lmax,double x,double* jl_x_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_jl_steed_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_jl_steed_array :: Int -> Double -> Ptr Double -> Int
bessel_jl_steed_array = gsl_sf_bessel_jl_steed_array
foreign import ccall "bessel.h gsl_sf_bessel_jl_steed_array" gsl_sf_bessel_jl_steed_array :: Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_y0_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_y0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_y0_e :: Double -> (Double,Double)
bessel_y0_e x = createSFR "bessel_y0_e" $ gsl_sf_bessel_y0_e x
foreign import ccall "bessel.h gsl_sf_bessel_y0_e" gsl_sf_bessel_y0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_y0(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_y0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_y0 :: Double -> Double
bessel_y0 = gsl_sf_bessel_y0
foreign import ccall "bessel.h gsl_sf_bessel_y0" gsl_sf_bessel_y0 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_y1_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_y1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_y1_e :: Double -> (Double,Double)
bessel_y1_e x = createSFR "bessel_y1_e" $ gsl_sf_bessel_y1_e x
foreign import ccall "bessel.h gsl_sf_bessel_y1_e" gsl_sf_bessel_y1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_y1(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_y1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_y1 :: Double -> Double
bessel_y1 = gsl_sf_bessel_y1
foreign import ccall "bessel.h gsl_sf_bessel_y1" gsl_sf_bessel_y1 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_y2_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_y2_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_y2_e :: Double -> (Double,Double)
bessel_y2_e x = createSFR "bessel_y2_e" $ gsl_sf_bessel_y2_e x
foreign import ccall "bessel.h gsl_sf_bessel_y2_e" gsl_sf_bessel_y2_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_y2(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_y2&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_y2 :: Double -> Double
bessel_y2 = gsl_sf_bessel_y2
foreign import ccall "bessel.h gsl_sf_bessel_y2" gsl_sf_bessel_y2 :: Double -> Double

-- | wrapper for int gsl_sf_bessel_yl_e(int l,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_yl_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_yl_e :: Int -> Double -> (Double,Double)
bessel_yl_e l x = createSFR "bessel_yl_e" $ gsl_sf_bessel_yl_e l x
foreign import ccall "bessel.h gsl_sf_bessel_yl_e" gsl_sf_bessel_yl_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_yl(int l,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_yl&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_yl :: Int -> Double -> Double
bessel_yl = gsl_sf_bessel_yl
foreign import ccall "bessel.h gsl_sf_bessel_yl" gsl_sf_bessel_yl :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_yl_array(int lmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_yl_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_yl_array :: Int -> Double -> Ptr Double -> Int
bessel_yl_array = gsl_sf_bessel_yl_array
foreign import ccall "bessel.h gsl_sf_bessel_yl_array" gsl_sf_bessel_yl_array :: Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_i0_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_i0_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_i0_scaled_e :: Double -> (Double,Double)
bessel_i0_scaled_e x = createSFR "bessel_i0_scaled_e" $ gsl_sf_bessel_i0_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_i0_scaled_e" gsl_sf_bessel_i0_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_i0_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_i0_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_i0_scaled :: Double -> Double
bessel_i0_scaled = gsl_sf_bessel_i0_scaled
foreign import ccall "bessel.h gsl_sf_bessel_i0_scaled" gsl_sf_bessel_i0_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_i1_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_i1_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_i1_scaled_e :: Double -> (Double,Double)
bessel_i1_scaled_e x = createSFR "bessel_i1_scaled_e" $ gsl_sf_bessel_i1_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_i1_scaled_e" gsl_sf_bessel_i1_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_i1_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_i1_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_i1_scaled :: Double -> Double
bessel_i1_scaled = gsl_sf_bessel_i1_scaled
foreign import ccall "bessel.h gsl_sf_bessel_i1_scaled" gsl_sf_bessel_i1_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_i2_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_i2_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_i2_scaled_e :: Double -> (Double,Double)
bessel_i2_scaled_e x = createSFR "bessel_i2_scaled_e" $ gsl_sf_bessel_i2_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_i2_scaled_e" gsl_sf_bessel_i2_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_i2_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_i2_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_i2_scaled :: Double -> Double
bessel_i2_scaled = gsl_sf_bessel_i2_scaled
foreign import ccall "bessel.h gsl_sf_bessel_i2_scaled" gsl_sf_bessel_i2_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_il_scaled_e(int l,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_il_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_il_scaled_e :: Int -> Double -> (Double,Double)
bessel_il_scaled_e l x = createSFR "bessel_il_scaled_e" $ gsl_sf_bessel_il_scaled_e l x
foreign import ccall "bessel.h gsl_sf_bessel_il_scaled_e" gsl_sf_bessel_il_scaled_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_il_scaled(int l,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_il_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_il_scaled :: Int -> Double -> Double
bessel_il_scaled = gsl_sf_bessel_il_scaled
foreign import ccall "bessel.h gsl_sf_bessel_il_scaled" gsl_sf_bessel_il_scaled :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_il_scaled_array(int lmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_il_scaled_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_il_scaled_array :: Int -> Double -> Ptr Double -> Int
bessel_il_scaled_array = gsl_sf_bessel_il_scaled_array
foreign import ccall "bessel.h gsl_sf_bessel_il_scaled_array" gsl_sf_bessel_il_scaled_array :: Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_k0_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_k0_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_k0_scaled_e :: Double -> (Double,Double)
bessel_k0_scaled_e x = createSFR "bessel_k0_scaled_e" $ gsl_sf_bessel_k0_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_k0_scaled_e" gsl_sf_bessel_k0_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_k0_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_k0_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_k0_scaled :: Double -> Double
bessel_k0_scaled = gsl_sf_bessel_k0_scaled
foreign import ccall "bessel.h gsl_sf_bessel_k0_scaled" gsl_sf_bessel_k0_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_k1_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_k1_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_k1_scaled_e :: Double -> (Double,Double)
bessel_k1_scaled_e x = createSFR "bessel_k1_scaled_e" $ gsl_sf_bessel_k1_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_k1_scaled_e" gsl_sf_bessel_k1_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_k1_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_k1_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_k1_scaled :: Double -> Double
bessel_k1_scaled = gsl_sf_bessel_k1_scaled
foreign import ccall "bessel.h gsl_sf_bessel_k1_scaled" gsl_sf_bessel_k1_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_k2_scaled_e(double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_k2_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_k2_scaled_e :: Double -> (Double,Double)
bessel_k2_scaled_e x = createSFR "bessel_k2_scaled_e" $ gsl_sf_bessel_k2_scaled_e x
foreign import ccall "bessel.h gsl_sf_bessel_k2_scaled_e" gsl_sf_bessel_k2_scaled_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_k2_scaled(double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_k2_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_k2_scaled :: Double -> Double
bessel_k2_scaled = gsl_sf_bessel_k2_scaled
foreign import ccall "bessel.h gsl_sf_bessel_k2_scaled" gsl_sf_bessel_k2_scaled :: Double -> Double

-- | wrapper for int gsl_sf_bessel_kl_scaled_e(int l,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_kl_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_kl_scaled_e :: Int -> Double -> (Double,Double)
bessel_kl_scaled_e l x = createSFR "bessel_kl_scaled_e" $ gsl_sf_bessel_kl_scaled_e l x
foreign import ccall "bessel.h gsl_sf_bessel_kl_scaled_e" gsl_sf_bessel_kl_scaled_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_kl_scaled(int l,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_kl_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_kl_scaled :: Int -> Double -> Double
bessel_kl_scaled = gsl_sf_bessel_kl_scaled
foreign import ccall "bessel.h gsl_sf_bessel_kl_scaled" gsl_sf_bessel_kl_scaled :: Int -> Double -> Double

-- | wrapper for int gsl_sf_bessel_kl_scaled_array(int lmax,double x,double* result_array);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_kl_scaled_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_kl_scaled_array :: Int -> Double -> Ptr Double -> Int
bessel_kl_scaled_array = gsl_sf_bessel_kl_scaled_array
foreign import ccall "bessel.h gsl_sf_bessel_kl_scaled_array" gsl_sf_bessel_kl_scaled_array :: Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_Jnu_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Jnu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Jnu_e :: Double -> Double -> (Double,Double)
bessel_Jnu_e nu x = createSFR "bessel_Jnu_e" $ gsl_sf_bessel_Jnu_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_Jnu_e" gsl_sf_bessel_Jnu_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Jnu(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Jnu&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Jnu :: Double -> Double -> Double
bessel_Jnu = gsl_sf_bessel_Jnu
foreign import ccall "bessel.h gsl_sf_bessel_Jnu" gsl_sf_bessel_Jnu :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Ynu_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Ynu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Ynu_e :: Double -> Double -> (Double,Double)
bessel_Ynu_e nu x = createSFR "bessel_Ynu_e" $ gsl_sf_bessel_Ynu_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_Ynu_e" gsl_sf_bessel_Ynu_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Ynu(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Ynu&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Ynu :: Double -> Double -> Double
bessel_Ynu = gsl_sf_bessel_Ynu
foreign import ccall "bessel.h gsl_sf_bessel_Ynu" gsl_sf_bessel_Ynu :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_sequence_Jnu_e(double nu,gsl_mode_t mode,size_t size,double* v);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_sequence_Jnu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_sequence_Jnu_e :: Double -> Precision -> Size_t -> Ptr Double -> Int
bessel_sequence_Jnu_e nu mode size v = gsl_sf_bessel_sequence_Jnu_e nu  (precCode mode) size v
foreign import ccall "bessel.h gsl_sf_bessel_sequence_Jnu_e" gsl_sf_bessel_sequence_Jnu_e :: Double -> Gsl_mode_t -> Size_t -> Ptr Double -> Int

-- | wrapper for int gsl_sf_bessel_Inu_scaled_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Inu_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Inu_scaled_e :: Double -> Double -> (Double,Double)
bessel_Inu_scaled_e nu x = createSFR "bessel_Inu_scaled_e" $ gsl_sf_bessel_Inu_scaled_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_Inu_scaled_e" gsl_sf_bessel_Inu_scaled_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Inu_scaled(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Inu_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Inu_scaled :: Double -> Double -> Double
bessel_Inu_scaled = gsl_sf_bessel_Inu_scaled
foreign import ccall "bessel.h gsl_sf_bessel_Inu_scaled" gsl_sf_bessel_Inu_scaled :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Inu_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Inu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Inu_e :: Double -> Double -> (Double,Double)
bessel_Inu_e nu x = createSFR "bessel_Inu_e" $ gsl_sf_bessel_Inu_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_Inu_e" gsl_sf_bessel_Inu_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Inu(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Inu&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Inu :: Double -> Double -> Double
bessel_Inu = gsl_sf_bessel_Inu
foreign import ccall "bessel.h gsl_sf_bessel_Inu" gsl_sf_bessel_Inu :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Knu_scaled_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Knu_scaled_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Knu_scaled_e :: Double -> Double -> (Double,Double)
bessel_Knu_scaled_e nu x = createSFR "bessel_Knu_scaled_e" $ gsl_sf_bessel_Knu_scaled_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_Knu_scaled_e" gsl_sf_bessel_Knu_scaled_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Knu_scaled(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Knu_scaled&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Knu_scaled :: Double -> Double -> Double
bessel_Knu_scaled = gsl_sf_bessel_Knu_scaled
foreign import ccall "bessel.h gsl_sf_bessel_Knu_scaled" gsl_sf_bessel_Knu_scaled :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_Knu_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Knu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Knu_e :: Double -> Double -> (Double,Double)
bessel_Knu_e nu x = createSFR "bessel_Knu_e" $ gsl_sf_bessel_Knu_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_Knu_e" gsl_sf_bessel_Knu_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_Knu(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_Knu&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_Knu :: Double -> Double -> Double
bessel_Knu = gsl_sf_bessel_Knu
foreign import ccall "bessel.h gsl_sf_bessel_Knu" gsl_sf_bessel_Knu :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_lnKnu_e(double nu,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_lnKnu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_lnKnu_e :: Double -> Double -> (Double,Double)
bessel_lnKnu_e nu x = createSFR "bessel_lnKnu_e" $ gsl_sf_bessel_lnKnu_e nu x
foreign import ccall "bessel.h gsl_sf_bessel_lnKnu_e" gsl_sf_bessel_lnKnu_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_lnKnu(double nu,double x);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_lnKnu&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_lnKnu :: Double -> Double -> Double
bessel_lnKnu = gsl_sf_bessel_lnKnu
foreign import ccall "bessel.h gsl_sf_bessel_lnKnu" gsl_sf_bessel_lnKnu :: Double -> Double -> Double

-- | wrapper for int gsl_sf_bessel_zero_J0_e(int s,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_zero_J0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_zero_J0_e :: Int -> (Double,Double)
bessel_zero_J0_e s = createSFR "bessel_zero_J0_e" $ gsl_sf_bessel_zero_J0_e s
foreign import ccall "bessel.h gsl_sf_bessel_zero_J0_e" gsl_sf_bessel_zero_J0_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_zero_J0(int s);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_zero_J0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_zero_J0 :: Int -> Double
bessel_zero_J0 = gsl_sf_bessel_zero_J0
foreign import ccall "bessel.h gsl_sf_bessel_zero_J0" gsl_sf_bessel_zero_J0 :: Int -> Double

-- | wrapper for int gsl_sf_bessel_zero_J1_e(int s,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_zero_J1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_zero_J1_e :: Int -> (Double,Double)
bessel_zero_J1_e s = createSFR "bessel_zero_J1_e" $ gsl_sf_bessel_zero_J1_e s
foreign import ccall "bessel.h gsl_sf_bessel_zero_J1_e" gsl_sf_bessel_zero_J1_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_zero_J1(int s);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_zero_J1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_zero_J1 :: Int -> Double
bessel_zero_J1 = gsl_sf_bessel_zero_J1
foreign import ccall "bessel.h gsl_sf_bessel_zero_J1" gsl_sf_bessel_zero_J1 :: Int -> Double

-- | wrapper for int gsl_sf_bessel_zero_Jnu_e(double nu,int s,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_zero_Jnu_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_zero_Jnu_e :: Double -> Int -> (Double,Double)
bessel_zero_Jnu_e nu s = createSFR "bessel_zero_Jnu_e" $ gsl_sf_bessel_zero_Jnu_e nu s
foreign import ccall "bessel.h gsl_sf_bessel_zero_Jnu_e" gsl_sf_bessel_zero_Jnu_e :: Double -> Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_bessel_zero_Jnu(double nu,int s);
--
--   <http://www.google.com/search?q=gsl_sf_bessel_zero_Jnu&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
bessel_zero_Jnu :: Double -> Int -> Double
bessel_zero_Jnu = gsl_sf_bessel_zero_Jnu
foreign import ccall "bessel.h gsl_sf_bessel_zero_Jnu" gsl_sf_bessel_zero_Jnu :: Double -> Int -> Double
