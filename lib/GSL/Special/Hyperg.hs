------------------------------------------------------------
{- |
Module      :  GSL.Special.Hyperg
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_hyperg.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module GSL.Special.Hyperg(
  hyperg_0F1_e
, hyperg_0F1
, hyperg_1F1_int_e
, hyperg_1F1_int
, hyperg_1F1_e
, hyperg_1F1
, hyperg_U_int_e
, hyperg_U_int
, hyperg_U_int_e10_e
, hyperg_U_e
, hyperg_U
, hyperg_U_e10_e
, hyperg_2F1_e
, hyperg_2F1
, hyperg_2F1_conj_e
, hyperg_2F1_conj
, hyperg_2F1_renorm_e
, hyperg_2F1_renorm
, hyperg_2F1_conj_renorm_e
, hyperg_2F1_conj_renorm
, hyperg_2F0_e
, hyperg_2F0
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_hyperg_0F1_e(double c,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_0F1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_0F1_e :: Double -> Double -> (Double,Double)
hyperg_0F1_e c x = createSFR "hyperg_0F1_e" $ gsl_sf_hyperg_0F1_e c x
foreign import ccall "hyperg.h gsl_sf_hyperg_0F1_e" gsl_sf_hyperg_0F1_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_0F1(double c,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_0F1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_0F1 :: Double -> Double -> Double
hyperg_0F1 = gsl_sf_hyperg_0F1
foreign import ccall "hyperg.h gsl_sf_hyperg_0F1" gsl_sf_hyperg_0F1 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_1F1_int_e(int m,int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_1F1_int_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_1F1_int_e :: Int -> Int -> Double -> (Double,Double)
hyperg_1F1_int_e m n x = createSFR "hyperg_1F1_int_e" $ gsl_sf_hyperg_1F1_int_e m n x
foreign import ccall "hyperg.h gsl_sf_hyperg_1F1_int_e" gsl_sf_hyperg_1F1_int_e :: Int -> Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_1F1_int(int m,int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_1F1_int&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_1F1_int :: Int -> Int -> Double -> Double
hyperg_1F1_int = gsl_sf_hyperg_1F1_int
foreign import ccall "hyperg.h gsl_sf_hyperg_1F1_int" gsl_sf_hyperg_1F1_int :: Int -> Int -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_1F1_e(double a,double b,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_1F1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_1F1_e :: Double -> Double -> Double -> (Double,Double)
hyperg_1F1_e a b x = createSFR "hyperg_1F1_e" $ gsl_sf_hyperg_1F1_e a b x
foreign import ccall "hyperg.h gsl_sf_hyperg_1F1_e" gsl_sf_hyperg_1F1_e :: Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_1F1(double a,double b,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_1F1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_1F1 :: Double -> Double -> Double -> Double
hyperg_1F1 = gsl_sf_hyperg_1F1
foreign import ccall "hyperg.h gsl_sf_hyperg_1F1" gsl_sf_hyperg_1F1 :: Double -> Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_U_int_e(int m,int n,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_U_int_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_U_int_e :: Int -> Int -> Double -> (Double,Double)
hyperg_U_int_e m n x = createSFR "hyperg_U_int_e" $ gsl_sf_hyperg_U_int_e m n x
foreign import ccall "hyperg.h gsl_sf_hyperg_U_int_e" gsl_sf_hyperg_U_int_e :: Int -> Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_U_int(int m,int n,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_U_int&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_U_int :: Int -> Int -> Double -> Double
hyperg_U_int = gsl_sf_hyperg_U_int
foreign import ccall "hyperg.h gsl_sf_hyperg_U_int" gsl_sf_hyperg_U_int :: Int -> Int -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_U_int_e10_e(int m,int n,double x,gsl_sf_result_e10* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_U_int_e10_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_U_int_e10_e :: Int -> Int -> Double -> (Double,Int,Double)
hyperg_U_int_e10_e m n x = createSFR_E10 "hyperg_U_int_e10_e" $ gsl_sf_hyperg_U_int_e10_e m n x
foreign import ccall "hyperg.h gsl_sf_hyperg_U_int_e10_e" gsl_sf_hyperg_U_int_e10_e :: Int -> Int -> Double -> Ptr () -> IO(Int)

-- | wrapper for int gsl_sf_hyperg_U_e(double a,double b,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_U_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_U_e :: Double -> Double -> Double -> (Double,Double)
hyperg_U_e a b x = createSFR "hyperg_U_e" $ gsl_sf_hyperg_U_e a b x
foreign import ccall "hyperg.h gsl_sf_hyperg_U_e" gsl_sf_hyperg_U_e :: Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_U(double a,double b,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_U&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_U :: Double -> Double -> Double -> Double
hyperg_U = gsl_sf_hyperg_U
foreign import ccall "hyperg.h gsl_sf_hyperg_U" gsl_sf_hyperg_U :: Double -> Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_U_e10_e(double a,double b,double x,gsl_sf_result_e10* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_U_e10_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_U_e10_e :: Double -> Double -> Double -> (Double,Int,Double)
hyperg_U_e10_e a b x = createSFR_E10 "hyperg_U_e10_e" $ gsl_sf_hyperg_U_e10_e a b x
foreign import ccall "hyperg.h gsl_sf_hyperg_U_e10_e" gsl_sf_hyperg_U_e10_e :: Double -> Double -> Double -> Ptr () -> IO(Int)

-- | wrapper for int gsl_sf_hyperg_2F1_e(double a,double b,double c,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_e a b c x = createSFR "hyperg_2F1_e" $ gsl_sf_hyperg_2F1_e a b c x
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_e" gsl_sf_hyperg_2F1_e :: Double -> Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_2F1(double a,double b,double c,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1 :: Double -> Double -> Double -> Double -> Double
hyperg_2F1 = gsl_sf_hyperg_2F1
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1" gsl_sf_hyperg_2F1 :: Double -> Double -> Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_2F1_conj_e(double aR,double aI,double c,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_conj_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_conj_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_conj_e aR aI c x = createSFR "hyperg_2F1_conj_e" $ gsl_sf_hyperg_2F1_conj_e aR aI c x
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_conj_e" gsl_sf_hyperg_2F1_conj_e :: Double -> Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_2F1_conj(double aR,double aI,double c,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_conj&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_conj :: Double -> Double -> Double -> Double -> Double
hyperg_2F1_conj = gsl_sf_hyperg_2F1_conj
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_conj" gsl_sf_hyperg_2F1_conj :: Double -> Double -> Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_2F1_renorm_e(double a,double b,double c,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_renorm_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_renorm_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_renorm_e a b c x = createSFR "hyperg_2F1_renorm_e" $ gsl_sf_hyperg_2F1_renorm_e a b c x
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_renorm_e" gsl_sf_hyperg_2F1_renorm_e :: Double -> Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_2F1_renorm(double a,double b,double c,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_renorm&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_renorm :: Double -> Double -> Double -> Double -> Double
hyperg_2F1_renorm = gsl_sf_hyperg_2F1_renorm
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_renorm" gsl_sf_hyperg_2F1_renorm :: Double -> Double -> Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_2F1_conj_renorm_e(double aR,double aI,double c,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_conj_renorm_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_conj_renorm_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_conj_renorm_e aR aI c x = createSFR "hyperg_2F1_conj_renorm_e" $ gsl_sf_hyperg_2F1_conj_renorm_e aR aI c x
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_conj_renorm_e" gsl_sf_hyperg_2F1_conj_renorm_e :: Double -> Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_2F1_conj_renorm(double aR,double aI,double c,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F1_conj_renorm&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F1_conj_renorm :: Double -> Double -> Double -> Double -> Double
hyperg_2F1_conj_renorm = gsl_sf_hyperg_2F1_conj_renorm
foreign import ccall "hyperg.h gsl_sf_hyperg_2F1_conj_renorm" gsl_sf_hyperg_2F1_conj_renorm :: Double -> Double -> Double -> Double -> Double

-- | wrapper for int gsl_sf_hyperg_2F0_e(double a,double b,double x,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F0_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F0_e :: Double -> Double -> Double -> (Double,Double)
hyperg_2F0_e a b x = createSFR "hyperg_2F0_e" $ gsl_sf_hyperg_2F0_e a b x
foreign import ccall "hyperg.h gsl_sf_hyperg_2F0_e" gsl_sf_hyperg_2F0_e :: Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hyperg_2F0(double a,double b,double x);
--
--   <http://www.google.com/search?q=gsl_sf_hyperg_2F0&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hyperg_2F0 :: Double -> Double -> Double -> Double
hyperg_2F0 = gsl_sf_hyperg_2F0
foreign import ccall "hyperg.h gsl_sf_hyperg_2F0" gsl_sf_hyperg_2F0 :: Double -> Double -> Double -> Double
