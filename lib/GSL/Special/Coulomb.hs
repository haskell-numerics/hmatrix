------------------------------------------------------------
{- |
Module      :  GSL.Special.Coulomb
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.google.com/search?q=gsl_sf_coulomb.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>

-}
------------------------------------------------------------

module GSL.Special.Coulomb(
  hydrogenicR_1_e
, hydrogenicR_1
, hydrogenicR_e
, hydrogenicR
, coulomb_CL_e
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_hydrogenicR_1_e(double Z,double r,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hydrogenicR_1_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hydrogenicR_1_e :: Double -> Double -> (Double,Double)
hydrogenicR_1_e zZ r = createSFR "hydrogenicR_1_e" $ gsl_sf_hydrogenicR_1_e zZ r
foreign import ccall "coulomb.h gsl_sf_hydrogenicR_1_e" gsl_sf_hydrogenicR_1_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hydrogenicR_1(double Z,double r);
--
--   <http://www.google.com/search?q=gsl_sf_hydrogenicR_1&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hydrogenicR_1 :: Double -> Double -> Double
hydrogenicR_1 = gsl_sf_hydrogenicR_1
foreign import ccall "coulomb.h gsl_sf_hydrogenicR_1" gsl_sf_hydrogenicR_1 :: Double -> Double -> Double

-- | wrapper for int gsl_sf_hydrogenicR_e(int n,int l,double Z,double r,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_hydrogenicR_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hydrogenicR_e :: Int -> Int -> Double -> Double -> (Double,Double)
hydrogenicR_e n l zZ r = createSFR "hydrogenicR_e" $ gsl_sf_hydrogenicR_e n l zZ r
foreign import ccall "coulomb.h gsl_sf_hydrogenicR_e" gsl_sf_hydrogenicR_e :: Int -> Int -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_hydrogenicR(int n,int l,double Z,double r);
--
--   <http://www.google.com/search?q=gsl_sf_hydrogenicR&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
hydrogenicR :: Int -> Int -> Double -> Double -> Double
hydrogenicR = gsl_sf_hydrogenicR
foreign import ccall "coulomb.h gsl_sf_hydrogenicR" gsl_sf_hydrogenicR :: Int -> Int -> Double -> Double -> Double

-- | wrapper for int gsl_sf_coulomb_wave_FG_e(double eta,double x,double lam_F,int k_lam_G,gsl_sf_result* F,gsl_sf_result* Fp,gsl_sf_result* G,gsl_sf_result* Gp,double* exp_F,double* exp_G);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_wave_FG_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_wave_FG_e :: Double -> Double -> Double -> Int -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Int
coulomb_wave_FG_e = gsl_sf_coulomb_wave_FG_e
foreign import ccall "coulomb.h gsl_sf_coulomb_wave_FG_e" gsl_sf_coulomb_wave_FG_e :: Double -> Double -> Double -> Int -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_coulomb_wave_F_array(double lam_min,int kmax,double eta,double x,double* fc_array,double* F_exponent);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_wave_F_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_wave_F_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Int
coulomb_wave_F_array = gsl_sf_coulomb_wave_F_array
foreign import ccall "coulomb.h gsl_sf_coulomb_wave_F_array" gsl_sf_coulomb_wave_F_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_coulomb_wave_FG_array(double lam_min,int kmax,double eta,double x,double* fc_array,double* gc_array,double* F_exponent,double* G_exponent);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_wave_FG_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_wave_FG_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Int
coulomb_wave_FG_array = gsl_sf_coulomb_wave_FG_array
foreign import ccall "coulomb.h gsl_sf_coulomb_wave_FG_array" gsl_sf_coulomb_wave_FG_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_coulomb_wave_FGp_array(double lam_min,int kmax,double eta,double x,double* fc_array,double* fcp_array,double* gc_array,double* gcp_array,double* F_exponent,double* G_exponent);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_wave_FGp_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_wave_FGp_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Int
coulomb_wave_FGp_array = gsl_sf_coulomb_wave_FGp_array
foreign import ccall "coulomb.h gsl_sf_coulomb_wave_FGp_array" gsl_sf_coulomb_wave_FGp_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_coulomb_wave_sphF_array(double lam_min,int kmax,double eta,double x,double* fc_array,double* F_exponent);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_wave_sphF_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_wave_sphF_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Int
coulomb_wave_sphF_array = gsl_sf_coulomb_wave_sphF_array
foreign import ccall "coulomb.h gsl_sf_coulomb_wave_sphF_array" gsl_sf_coulomb_wave_sphF_array :: Double -> Int -> Double -> Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_coulomb_CL_e(double L,double eta,gsl_sf_result* result);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_CL_e&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_CL_e :: Double -> Double -> (Double,Double)
coulomb_CL_e lL eta = createSFR "coulomb_CL_e" $ gsl_sf_coulomb_CL_e lL eta
foreign import ccall "coulomb.h gsl_sf_coulomb_CL_e" gsl_sf_coulomb_CL_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_coulomb_CL_array(double Lmin,int kmax,double eta,double* cl);
--
--   <http://www.google.com/search?q=gsl_sf_coulomb_CL_array&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
coulomb_CL_array :: Double -> Int -> Double -> Ptr Double -> Int
coulomb_CL_array = gsl_sf_coulomb_CL_array
foreign import ccall "coulomb.h gsl_sf_coulomb_CL_array" gsl_sf_coulomb_CL_array :: Double -> Int -> Double -> Ptr Double -> Int
