------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special.Legendre
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Legendre-Functions-and-Spherical-Harmonics.html>

-}
------------------------------------------------------------

module Numeric.GSL.Special.Legendre(
  legendre_Pl_e
, legendre_Pl
, legendre_P1_e
, legendre_P2_e
, legendre_P3_e
, legendre_P1
, legendre_P2
, legendre_P3
, legendre_Q0_e
, legendre_Q0
, legendre_Q1_e
, legendre_Q1
, legendre_Ql_e
, legendre_Ql
, legendre_Plm_e
, legendre_Plm
, legendre_sphPlm_e
, legendre_sphPlm
, legendre_array_size
, conicalP_half_e
, conicalP_half
, conicalP_mhalf_e
, conicalP_mhalf
, conicalP_0_e
, conicalP_0
, conicalP_1_e
, conicalP_1
, conicalP_sph_reg_e
, conicalP_sph_reg
, conicalP_cyl_reg_e
, conicalP_cyl_reg
, legendre_H3d_0_e
, legendre_H3d_0
, legendre_H3d_1_e
, legendre_H3d_1
, legendre_H3d_e
, legendre_H3d
) where

import Foreign(Ptr)
import Numeric.GSL.Special.Internal

-- | wrapper for int gsl_sf_legendre_Pl_e(int l,double x,gsl_sf_result* result);
legendre_Pl_e :: Int -> Double -> (Double,Double)
legendre_Pl_e l x = createSFR "legendre_Pl_e" $ gsl_sf_legendre_Pl_e l x
foreign import ccall "legendre.h gsl_sf_legendre_Pl_e" gsl_sf_legendre_Pl_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_Pl(int l,double x);
legendre_Pl :: Int -> Double -> Double
legendre_Pl = gsl_sf_legendre_Pl
foreign import ccall "legendre.h gsl_sf_legendre_Pl" gsl_sf_legendre_Pl :: Int -> Double -> Double

-- | wrapper for int gsl_sf_legendre_Pl_array(int lmax,double x,double* result_array);
legendre_Pl_array :: Int -> Double -> Ptr Double -> Int
legendre_Pl_array = gsl_sf_legendre_Pl_array
foreign import ccall "legendre.h gsl_sf_legendre_Pl_array" gsl_sf_legendre_Pl_array :: Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_legendre_Pl_deriv_array(int lmax,double x,double* result_array,double* result_deriv_array);
legendre_Pl_deriv_array :: Int -> Double -> Ptr Double -> Ptr Double -> Int
legendre_Pl_deriv_array = gsl_sf_legendre_Pl_deriv_array
foreign import ccall "legendre.h gsl_sf_legendre_Pl_deriv_array" gsl_sf_legendre_Pl_deriv_array :: Int -> Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_legendre_P1_e(double x,gsl_sf_result* result);
legendre_P1_e :: Double -> (Double,Double)
legendre_P1_e x = createSFR "legendre_P1_e" $ gsl_sf_legendre_P1_e x
foreign import ccall "legendre.h gsl_sf_legendre_P1_e" gsl_sf_legendre_P1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_legendre_P2_e(double x,gsl_sf_result* result);
legendre_P2_e :: Double -> (Double,Double)
legendre_P2_e x = createSFR "legendre_P2_e" $ gsl_sf_legendre_P2_e x
foreign import ccall "legendre.h gsl_sf_legendre_P2_e" gsl_sf_legendre_P2_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_legendre_P3_e(double x,gsl_sf_result* result);
legendre_P3_e :: Double -> (Double,Double)
legendre_P3_e x = createSFR "legendre_P3_e" $ gsl_sf_legendre_P3_e x
foreign import ccall "legendre.h gsl_sf_legendre_P3_e" gsl_sf_legendre_P3_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_P1(double x);
legendre_P1 :: Double -> Double
legendre_P1 = gsl_sf_legendre_P1
foreign import ccall "legendre.h gsl_sf_legendre_P1" gsl_sf_legendre_P1 :: Double -> Double

-- | wrapper for double gsl_sf_legendre_P2(double x);
legendre_P2 :: Double -> Double
legendre_P2 = gsl_sf_legendre_P2
foreign import ccall "legendre.h gsl_sf_legendre_P2" gsl_sf_legendre_P2 :: Double -> Double

-- | wrapper for double gsl_sf_legendre_P3(double x);
legendre_P3 :: Double -> Double
legendre_P3 = gsl_sf_legendre_P3
foreign import ccall "legendre.h gsl_sf_legendre_P3" gsl_sf_legendre_P3 :: Double -> Double

-- | wrapper for int gsl_sf_legendre_Q0_e(double x,gsl_sf_result* result);
legendre_Q0_e :: Double -> (Double,Double)
legendre_Q0_e x = createSFR "legendre_Q0_e" $ gsl_sf_legendre_Q0_e x
foreign import ccall "legendre.h gsl_sf_legendre_Q0_e" gsl_sf_legendre_Q0_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_Q0(double x);
legendre_Q0 :: Double -> Double
legendre_Q0 = gsl_sf_legendre_Q0
foreign import ccall "legendre.h gsl_sf_legendre_Q0" gsl_sf_legendre_Q0 :: Double -> Double

-- | wrapper for int gsl_sf_legendre_Q1_e(double x,gsl_sf_result* result);
legendre_Q1_e :: Double -> (Double,Double)
legendre_Q1_e x = createSFR "legendre_Q1_e" $ gsl_sf_legendre_Q1_e x
foreign import ccall "legendre.h gsl_sf_legendre_Q1_e" gsl_sf_legendre_Q1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_Q1(double x);
legendre_Q1 :: Double -> Double
legendre_Q1 = gsl_sf_legendre_Q1
foreign import ccall "legendre.h gsl_sf_legendre_Q1" gsl_sf_legendre_Q1 :: Double -> Double

-- | wrapper for int gsl_sf_legendre_Ql_e(int l,double x,gsl_sf_result* result);
legendre_Ql_e :: Int -> Double -> (Double,Double)
legendre_Ql_e l x = createSFR "legendre_Ql_e" $ gsl_sf_legendre_Ql_e l x
foreign import ccall "legendre.h gsl_sf_legendre_Ql_e" gsl_sf_legendre_Ql_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_Ql(int l,double x);
legendre_Ql :: Int -> Double -> Double
legendre_Ql = gsl_sf_legendre_Ql
foreign import ccall "legendre.h gsl_sf_legendre_Ql" gsl_sf_legendre_Ql :: Int -> Double -> Double

-- | wrapper for int gsl_sf_legendre_Plm_e(int l,int m,double x,gsl_sf_result* result);
legendre_Plm_e :: Int -> Int -> Double -> (Double,Double)
legendre_Plm_e l m x = createSFR "legendre_Plm_e" $ gsl_sf_legendre_Plm_e l m x
foreign import ccall "legendre.h gsl_sf_legendre_Plm_e" gsl_sf_legendre_Plm_e :: Int -> Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_Plm(int l,int m,double x);
legendre_Plm :: Int -> Int -> Double -> Double
legendre_Plm = gsl_sf_legendre_Plm
foreign import ccall "legendre.h gsl_sf_legendre_Plm" gsl_sf_legendre_Plm :: Int -> Int -> Double -> Double

-- | wrapper for int gsl_sf_legendre_Plm_array(int lmax,int m,double x,double* result_array);
legendre_Plm_array :: Int -> Int -> Double -> Ptr Double -> Int
legendre_Plm_array = gsl_sf_legendre_Plm_array
foreign import ccall "legendre.h gsl_sf_legendre_Plm_array" gsl_sf_legendre_Plm_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_legendre_Plm_deriv_array(int lmax,int m,double x,double* result_array,double* result_deriv_array);
legendre_Plm_deriv_array :: Int -> Int -> Double -> Ptr Double -> Ptr Double -> Int
legendre_Plm_deriv_array = gsl_sf_legendre_Plm_deriv_array
foreign import ccall "legendre.h gsl_sf_legendre_Plm_deriv_array" gsl_sf_legendre_Plm_deriv_array :: Int -> Int -> Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_legendre_sphPlm_e(int l,int m,double x,gsl_sf_result* result);
legendre_sphPlm_e :: Int -> Int -> Double -> (Double,Double)
legendre_sphPlm_e l m x = createSFR "legendre_sphPlm_e" $ gsl_sf_legendre_sphPlm_e l m x
foreign import ccall "legendre.h gsl_sf_legendre_sphPlm_e" gsl_sf_legendre_sphPlm_e :: Int -> Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_sphPlm(int l,int m,double x);
legendre_sphPlm :: Int -> Int -> Double -> Double
legendre_sphPlm = gsl_sf_legendre_sphPlm
foreign import ccall "legendre.h gsl_sf_legendre_sphPlm" gsl_sf_legendre_sphPlm :: Int -> Int -> Double -> Double

-- | wrapper for int gsl_sf_legendre_sphPlm_array(int lmax,int m,double x,double* result_array);
legendre_sphPlm_array :: Int -> Int -> Double -> Ptr Double -> Int
legendre_sphPlm_array = gsl_sf_legendre_sphPlm_array
foreign import ccall "legendre.h gsl_sf_legendre_sphPlm_array" gsl_sf_legendre_sphPlm_array :: Int -> Int -> Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_legendre_sphPlm_deriv_array(int lmax,int m,double x,double* result_array,double* result_deriv_array);
legendre_sphPlm_deriv_array :: Int -> Int -> Double -> Ptr Double -> Ptr Double -> Int
legendre_sphPlm_deriv_array = gsl_sf_legendre_sphPlm_deriv_array
foreign import ccall "legendre.h gsl_sf_legendre_sphPlm_deriv_array" gsl_sf_legendre_sphPlm_deriv_array :: Int -> Int -> Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_legendre_array_size(int lmax,int m);
legendre_array_size :: Int -> Int -> Int
legendre_array_size = gsl_sf_legendre_array_size
foreign import ccall "legendre.h gsl_sf_legendre_array_size" gsl_sf_legendre_array_size :: Int -> Int -> Int

-- | wrapper for int gsl_sf_conicalP_half_e(double lambda,double x,gsl_sf_result* result);
conicalP_half_e :: Double -> Double -> (Double,Double)
conicalP_half_e lambda x = createSFR "conicalP_half_e" $ gsl_sf_conicalP_half_e lambda x
foreign import ccall "legendre.h gsl_sf_conicalP_half_e" gsl_sf_conicalP_half_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_conicalP_half(double lambda,double x);
conicalP_half :: Double -> Double -> Double
conicalP_half = gsl_sf_conicalP_half
foreign import ccall "legendre.h gsl_sf_conicalP_half" gsl_sf_conicalP_half :: Double -> Double -> Double

-- | wrapper for int gsl_sf_conicalP_mhalf_e(double lambda,double x,gsl_sf_result* result);
conicalP_mhalf_e :: Double -> Double -> (Double,Double)
conicalP_mhalf_e lambda x = createSFR "conicalP_mhalf_e" $ gsl_sf_conicalP_mhalf_e lambda x
foreign import ccall "legendre.h gsl_sf_conicalP_mhalf_e" gsl_sf_conicalP_mhalf_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_conicalP_mhalf(double lambda,double x);
conicalP_mhalf :: Double -> Double -> Double
conicalP_mhalf = gsl_sf_conicalP_mhalf
foreign import ccall "legendre.h gsl_sf_conicalP_mhalf" gsl_sf_conicalP_mhalf :: Double -> Double -> Double

-- | wrapper for int gsl_sf_conicalP_0_e(double lambda,double x,gsl_sf_result* result);
conicalP_0_e :: Double -> Double -> (Double,Double)
conicalP_0_e lambda x = createSFR "conicalP_0_e" $ gsl_sf_conicalP_0_e lambda x
foreign import ccall "legendre.h gsl_sf_conicalP_0_e" gsl_sf_conicalP_0_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_conicalP_0(double lambda,double x);
conicalP_0 :: Double -> Double -> Double
conicalP_0 = gsl_sf_conicalP_0
foreign import ccall "legendre.h gsl_sf_conicalP_0" gsl_sf_conicalP_0 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_conicalP_1_e(double lambda,double x,gsl_sf_result* result);
conicalP_1_e :: Double -> Double -> (Double,Double)
conicalP_1_e lambda x = createSFR "conicalP_1_e" $ gsl_sf_conicalP_1_e lambda x
foreign import ccall "legendre.h gsl_sf_conicalP_1_e" gsl_sf_conicalP_1_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_conicalP_1(double lambda,double x);
conicalP_1 :: Double -> Double -> Double
conicalP_1 = gsl_sf_conicalP_1
foreign import ccall "legendre.h gsl_sf_conicalP_1" gsl_sf_conicalP_1 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_conicalP_sph_reg_e(int l,double lambda,double x,gsl_sf_result* result);
conicalP_sph_reg_e :: Int -> Double -> Double -> (Double,Double)
conicalP_sph_reg_e l lambda x = createSFR "conicalP_sph_reg_e" $ gsl_sf_conicalP_sph_reg_e l lambda x
foreign import ccall "legendre.h gsl_sf_conicalP_sph_reg_e" gsl_sf_conicalP_sph_reg_e :: Int -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_conicalP_sph_reg(int l,double lambda,double x);
conicalP_sph_reg :: Int -> Double -> Double -> Double
conicalP_sph_reg = gsl_sf_conicalP_sph_reg
foreign import ccall "legendre.h gsl_sf_conicalP_sph_reg" gsl_sf_conicalP_sph_reg :: Int -> Double -> Double -> Double

-- | wrapper for int gsl_sf_conicalP_cyl_reg_e(int m,double lambda,double x,gsl_sf_result* result);
conicalP_cyl_reg_e :: Int -> Double -> Double -> (Double,Double)
conicalP_cyl_reg_e m lambda x = createSFR "conicalP_cyl_reg_e" $ gsl_sf_conicalP_cyl_reg_e m lambda x
foreign import ccall "legendre.h gsl_sf_conicalP_cyl_reg_e" gsl_sf_conicalP_cyl_reg_e :: Int -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_conicalP_cyl_reg(int m,double lambda,double x);
conicalP_cyl_reg :: Int -> Double -> Double -> Double
conicalP_cyl_reg = gsl_sf_conicalP_cyl_reg
foreign import ccall "legendre.h gsl_sf_conicalP_cyl_reg" gsl_sf_conicalP_cyl_reg :: Int -> Double -> Double -> Double

-- | wrapper for int gsl_sf_legendre_H3d_0_e(double lambda,double eta,gsl_sf_result* result);
legendre_H3d_0_e :: Double -> Double -> (Double,Double)
legendre_H3d_0_e lambda eta = createSFR "legendre_H3d_0_e" $ gsl_sf_legendre_H3d_0_e lambda eta
foreign import ccall "legendre.h gsl_sf_legendre_H3d_0_e" gsl_sf_legendre_H3d_0_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_H3d_0(double lambda,double eta);
legendre_H3d_0 :: Double -> Double -> Double
legendre_H3d_0 = gsl_sf_legendre_H3d_0
foreign import ccall "legendre.h gsl_sf_legendre_H3d_0" gsl_sf_legendre_H3d_0 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_legendre_H3d_1_e(double lambda,double eta,gsl_sf_result* result);
legendre_H3d_1_e :: Double -> Double -> (Double,Double)
legendre_H3d_1_e lambda eta = createSFR "legendre_H3d_1_e" $ gsl_sf_legendre_H3d_1_e lambda eta
foreign import ccall "legendre.h gsl_sf_legendre_H3d_1_e" gsl_sf_legendre_H3d_1_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_H3d_1(double lambda,double eta);
legendre_H3d_1 :: Double -> Double -> Double
legendre_H3d_1 = gsl_sf_legendre_H3d_1
foreign import ccall "legendre.h gsl_sf_legendre_H3d_1" gsl_sf_legendre_H3d_1 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_legendre_H3d_e(int l,double lambda,double eta,gsl_sf_result* result);
legendre_H3d_e :: Int -> Double -> Double -> (Double,Double)
legendre_H3d_e l lambda eta = createSFR "legendre_H3d_e" $ gsl_sf_legendre_H3d_e l lambda eta
foreign import ccall "legendre.h gsl_sf_legendre_H3d_e" gsl_sf_legendre_H3d_e :: Int -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_legendre_H3d(int l,double lambda,double eta);
legendre_H3d :: Int -> Double -> Double -> Double
legendre_H3d = gsl_sf_legendre_H3d
foreign import ccall "legendre.h gsl_sf_legendre_H3d" gsl_sf_legendre_H3d :: Int -> Double -> Double -> Double

-- | wrapper for int gsl_sf_legendre_H3d_array(int lmax,double lambda,double eta,double* result_array);
legendre_H3d_array :: Int -> Double -> Double -> Ptr Double -> Int
legendre_H3d_array = gsl_sf_legendre_H3d_array
foreign import ccall "legendre.h gsl_sf_legendre_H3d_array" gsl_sf_legendre_H3d_array :: Int -> Double -> Double -> Ptr Double -> Int
