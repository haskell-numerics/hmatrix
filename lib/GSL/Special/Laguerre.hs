------------------------------------------------------------
{- |
Module      :  GSL.Special.Laguerre
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_laguerre.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module GSL.Special.Laguerre(
  laguerre_1_e
, laguerre_2_e
, laguerre_3_e
, laguerre_1
, laguerre_2
, laguerre_3
, laguerre_n_e
, laguerre_n
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_laguerre_1_e(double a,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_1_e :: Double -> Double -> (Double,Double)
laguerre_1_e a x = createSFR "laguerre_1_e" $ gsl_sf_laguerre_1_e a x
foreign import ccall "laguerre.h gsl_sf_laguerre_1_e" gsl_sf_laguerre_1_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_laguerre_2_e(double a,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_2_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_2_e :: Double -> Double -> (Double,Double)
laguerre_2_e a x = createSFR "laguerre_2_e" $ gsl_sf_laguerre_2_e a x
foreign import ccall "laguerre.h gsl_sf_laguerre_2_e" gsl_sf_laguerre_2_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_laguerre_3_e(double a,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_3_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_3_e :: Double -> Double -> (Double,Double)
laguerre_3_e a x = createSFR "laguerre_3_e" $ gsl_sf_laguerre_3_e a x
foreign import ccall "laguerre.h gsl_sf_laguerre_3_e" gsl_sf_laguerre_3_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_laguerre_1(double a,double x);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_1 :: Double -> Double -> Double
laguerre_1 = gsl_sf_laguerre_1
foreign import ccall "laguerre.h gsl_sf_laguerre_1" gsl_sf_laguerre_1 :: Double -> Double -> Double

-- | wrapper for double gsl_sf_laguerre_2(double a,double x);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_2&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_2 :: Double -> Double -> Double
laguerre_2 = gsl_sf_laguerre_2
foreign import ccall "laguerre.h gsl_sf_laguerre_2" gsl_sf_laguerre_2 :: Double -> Double -> Double

-- | wrapper for double gsl_sf_laguerre_3(double a,double x);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_3&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_3 :: Double -> Double -> Double
laguerre_3 = gsl_sf_laguerre_3
foreign import ccall "laguerre.h gsl_sf_laguerre_3" gsl_sf_laguerre_3 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_laguerre_n_e(int n,double a,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_n_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_n_e :: Int -> Double -> Double -> (Double,Double)
laguerre_n_e n a x = createSFR "laguerre_n_e" $ gsl_sf_laguerre_n_e n a x
foreign import ccall "laguerre.h gsl_sf_laguerre_n_e" gsl_sf_laguerre_n_e :: Int -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_laguerre_n(int n,double a,double x);
--
--   <http://www.google.com/search?q=gsl_sf_laguerre_n&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
laguerre_n :: Int -> Double -> Double -> Double
laguerre_n = gsl_sf_laguerre_n
foreign import ccall "laguerre.h gsl_sf_laguerre_n" gsl_sf_laguerre_n :: Int -> Double -> Double -> Double
