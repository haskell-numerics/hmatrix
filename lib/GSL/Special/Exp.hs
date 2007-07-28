------------------------------------------------------------
{- |
Module      :  GSL.Special.Exp
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Exponential-Functions.html>

-}
------------------------------------------------------------

module GSL.Special.Exp(
  exp_e
, GSL.Special.Exp.exp
, exp_e10_e
, exp_mult_e
, exp_mult
, exp_mult_e10_e
, expm1_e
, expm1
, exprel_e
, exprel
, exprel_2_e
, exprel_2
, exprel_n_e
, exprel_n
, exp_err_e
, exp_err_e10_e
, exp_mult_err_e
, exp_mult_err_e10_e
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_exp_e(double x,gsl_sf_result* result);
exp_e :: Double -> (Double,Double)
exp_e x = createSFR "exp_e" $ gsl_sf_exp_e x
foreign import ccall "exp.h gsl_sf_exp_e" gsl_sf_exp_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_exp(double x);
exp :: Double -> Double
exp = gsl_sf_exp
foreign import ccall "exp.h gsl_sf_exp" gsl_sf_exp :: Double -> Double

-- | wrapper for int gsl_sf_exp_e10_e(double x,gsl_sf_result_e10* result);
exp_e10_e :: Double -> (Double,Int,Double)
exp_e10_e x = createSFR_E10 "exp_e10_e" $ gsl_sf_exp_e10_e x
foreign import ccall "exp.h gsl_sf_exp_e10_e" gsl_sf_exp_e10_e :: Double -> Ptr () -> IO(Int)

-- | wrapper for int gsl_sf_exp_mult_e(double x,double y,gsl_sf_result* result);
exp_mult_e :: Double -> Double -> (Double,Double)
exp_mult_e x y = createSFR "exp_mult_e" $ gsl_sf_exp_mult_e x y
foreign import ccall "exp.h gsl_sf_exp_mult_e" gsl_sf_exp_mult_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_exp_mult(double x,double y);
exp_mult :: Double -> Double -> Double
exp_mult = gsl_sf_exp_mult
foreign import ccall "exp.h gsl_sf_exp_mult" gsl_sf_exp_mult :: Double -> Double -> Double

-- | wrapper for int gsl_sf_exp_mult_e10_e(double x,double y,gsl_sf_result_e10* result);
exp_mult_e10_e :: Double -> Double -> (Double,Int,Double)
exp_mult_e10_e x y = createSFR_E10 "exp_mult_e10_e" $ gsl_sf_exp_mult_e10_e x y
foreign import ccall "exp.h gsl_sf_exp_mult_e10_e" gsl_sf_exp_mult_e10_e :: Double -> Double -> Ptr () -> IO(Int)

-- | wrapper for int gsl_sf_expm1_e(double x,gsl_sf_result* result);
expm1_e :: Double -> (Double,Double)
expm1_e x = createSFR "expm1_e" $ gsl_sf_expm1_e x
foreign import ccall "exp.h gsl_sf_expm1_e" gsl_sf_expm1_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_expm1(double x);
expm1 :: Double -> Double
expm1 = gsl_sf_expm1
foreign import ccall "exp.h gsl_sf_expm1" gsl_sf_expm1 :: Double -> Double

-- | wrapper for int gsl_sf_exprel_e(double x,gsl_sf_result* result);
exprel_e :: Double -> (Double,Double)
exprel_e x = createSFR "exprel_e" $ gsl_sf_exprel_e x
foreign import ccall "exp.h gsl_sf_exprel_e" gsl_sf_exprel_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_exprel(double x);
exprel :: Double -> Double
exprel = gsl_sf_exprel
foreign import ccall "exp.h gsl_sf_exprel" gsl_sf_exprel :: Double -> Double

-- | wrapper for int gsl_sf_exprel_2_e(double x,gsl_sf_result* result);
exprel_2_e :: Double -> (Double,Double)
exprel_2_e x = createSFR "exprel_2_e" $ gsl_sf_exprel_2_e x
foreign import ccall "exp.h gsl_sf_exprel_2_e" gsl_sf_exprel_2_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_exprel_2(double x);
exprel_2 :: Double -> Double
exprel_2 = gsl_sf_exprel_2
foreign import ccall "exp.h gsl_sf_exprel_2" gsl_sf_exprel_2 :: Double -> Double

-- | wrapper for int gsl_sf_exprel_n_e(int n,double x,gsl_sf_result* result);
exprel_n_e :: Int -> Double -> (Double,Double)
exprel_n_e n x = createSFR "exprel_n_e" $ gsl_sf_exprel_n_e n x
foreign import ccall "exp.h gsl_sf_exprel_n_e" gsl_sf_exprel_n_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_exprel_n(int n,double x);
exprel_n :: Int -> Double -> Double
exprel_n = gsl_sf_exprel_n
foreign import ccall "exp.h gsl_sf_exprel_n" gsl_sf_exprel_n :: Int -> Double -> Double

-- | wrapper for int gsl_sf_exp_err_e(double x,double dx,gsl_sf_result* result);
exp_err_e :: Double -> Double -> (Double,Double)
exp_err_e x dx = createSFR "exp_err_e" $ gsl_sf_exp_err_e x dx
foreign import ccall "exp.h gsl_sf_exp_err_e" gsl_sf_exp_err_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_exp_err_e10_e(double x,double dx,gsl_sf_result_e10* result);
exp_err_e10_e :: Double -> Double -> (Double,Int,Double)
exp_err_e10_e x dx = createSFR_E10 "exp_err_e10_e" $ gsl_sf_exp_err_e10_e x dx
foreign import ccall "exp.h gsl_sf_exp_err_e10_e" gsl_sf_exp_err_e10_e :: Double -> Double -> Ptr () -> IO(Int)

-- | wrapper for int gsl_sf_exp_mult_err_e(double x,double dx,double y,double dy,gsl_sf_result* result);
exp_mult_err_e :: Double -> Double -> Double -> Double -> (Double,Double)
exp_mult_err_e x dx y dy = createSFR "exp_mult_err_e" $ gsl_sf_exp_mult_err_e x dx y dy
foreign import ccall "exp.h gsl_sf_exp_mult_err_e" gsl_sf_exp_mult_err_e :: Double -> Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_exp_mult_err_e10_e(double x,double dx,double y,double dy,gsl_sf_result_e10* result);
exp_mult_err_e10_e :: Double -> Double -> Double -> Double -> (Double,Int,Double)
exp_mult_err_e10_e x dx y dy = createSFR_E10 "exp_mult_err_e10_e" $ gsl_sf_exp_mult_err_e10_e x dx y dy
foreign import ccall "exp.h gsl_sf_exp_mult_err_e10_e" gsl_sf_exp_mult_err_e10_e :: Double -> Double -> Double -> Double -> Ptr () -> IO(Int)
