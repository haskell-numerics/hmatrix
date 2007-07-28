------------------------------------------------------------
{- |
Module      :  GSL.Special.Gamma
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Gamma-and-Beta-Functions.html>


-}
------------------------------------------------------------

module GSL.Special.Gamma(
  lngamma_e
, lngamma
, gamma_e
, gamma
, gammastar_e
, gammastar
, gammainv_e
, gammainv
, taylorcoeff_e
, taylorcoeff
, fact_e
, fact
, doublefact_e
, doublefact
, lnfact_e
, lnfact
, lndoublefact_e
, lndoublefact
, lnchoose_e
, lnchoose
, choose_e
, choose
, lnpoch_e
, lnpoch
, poch_e
, poch
, pochrel_e
, pochrel
, gamma_inc_Q_e
, gamma_inc_Q
, gamma_inc_P_e
, gamma_inc_P
, gamma_inc_e
, gamma_inc
, lnbeta_e
, lnbeta
, beta_e
, beta
, beta_inc_e
, beta_inc
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_lngamma_e(double x,gsl_sf_result* result);
lngamma_e :: Double -> (Double,Double)
lngamma_e x = createSFR "lngamma_e" $ gsl_sf_lngamma_e x
foreign import ccall "gamma.h gsl_sf_lngamma_e" gsl_sf_lngamma_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lngamma(double x);
lngamma :: Double -> Double
lngamma = gsl_sf_lngamma
foreign import ccall "gamma.h gsl_sf_lngamma" gsl_sf_lngamma :: Double -> Double

-- | wrapper for int gsl_sf_lngamma_sgn_e(double x,gsl_sf_result* result_lg,double* sgn);
lngamma_sgn_e :: Double -> Ptr Double -> Ptr Double -> Int
lngamma_sgn_e = gsl_sf_lngamma_sgn_e
foreign import ccall "gamma.h gsl_sf_lngamma_sgn_e" gsl_sf_lngamma_sgn_e :: Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_gamma_e(double x,gsl_sf_result* result);
gamma_e :: Double -> (Double,Double)
gamma_e x = createSFR "gamma_e" $ gsl_sf_gamma_e x
foreign import ccall "gamma.h gsl_sf_gamma_e" gsl_sf_gamma_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gamma(double x);
gamma :: Double -> Double
gamma = gsl_sf_gamma
foreign import ccall "gamma.h gsl_sf_gamma" gsl_sf_gamma :: Double -> Double

-- | wrapper for int gsl_sf_gammastar_e(double x,gsl_sf_result* result);
gammastar_e :: Double -> (Double,Double)
gammastar_e x = createSFR "gammastar_e" $ gsl_sf_gammastar_e x
foreign import ccall "gamma.h gsl_sf_gammastar_e" gsl_sf_gammastar_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gammastar(double x);
gammastar :: Double -> Double
gammastar = gsl_sf_gammastar
foreign import ccall "gamma.h gsl_sf_gammastar" gsl_sf_gammastar :: Double -> Double

-- | wrapper for int gsl_sf_gammainv_e(double x,gsl_sf_result* result);
gammainv_e :: Double -> (Double,Double)
gammainv_e x = createSFR "gammainv_e" $ gsl_sf_gammainv_e x
foreign import ccall "gamma.h gsl_sf_gammainv_e" gsl_sf_gammainv_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gammainv(double x);
gammainv :: Double -> Double
gammainv = gsl_sf_gammainv
foreign import ccall "gamma.h gsl_sf_gammainv" gsl_sf_gammainv :: Double -> Double

-- | wrapper for int gsl_sf_lngamma_complex_e(double zr,double zi,gsl_sf_result* lnr,gsl_sf_result* arg);
lngamma_complex_e :: Double -> Double -> Ptr Double -> (Double,Double)
lngamma_complex_e zr zi lnr = createSFR "lngamma_complex_e" $ gsl_sf_lngamma_complex_e zr zi lnr
foreign import ccall "gamma.h gsl_sf_lngamma_complex_e" gsl_sf_lngamma_complex_e :: Double -> Double -> Ptr Double -> Ptr Double -> IO(Int)

-- | wrapper for int gsl_sf_taylorcoeff_e(int n,double x,gsl_sf_result* result);
taylorcoeff_e :: Int -> Double -> (Double,Double)
taylorcoeff_e n x = createSFR "taylorcoeff_e" $ gsl_sf_taylorcoeff_e n x
foreign import ccall "gamma.h gsl_sf_taylorcoeff_e" gsl_sf_taylorcoeff_e :: Int -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_taylorcoeff(int n,double x);
taylorcoeff :: Int -> Double -> Double
taylorcoeff = gsl_sf_taylorcoeff
foreign import ccall "gamma.h gsl_sf_taylorcoeff" gsl_sf_taylorcoeff :: Int -> Double -> Double

-- | wrapper for int gsl_sf_fact_e(int n,gsl_sf_result* result);
fact_e :: Int -> (Double,Double)
fact_e n = createSFR "fact_e" $ gsl_sf_fact_e n
foreign import ccall "gamma.h gsl_sf_fact_e" gsl_sf_fact_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_fact(int n);
fact :: Int -> Double
fact = gsl_sf_fact
foreign import ccall "gamma.h gsl_sf_fact" gsl_sf_fact :: Int -> Double

-- | wrapper for int gsl_sf_doublefact_e(int n,gsl_sf_result* result);
doublefact_e :: Int -> (Double,Double)
doublefact_e n = createSFR "doublefact_e" $ gsl_sf_doublefact_e n
foreign import ccall "gamma.h gsl_sf_doublefact_e" gsl_sf_doublefact_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_doublefact(int n);
doublefact :: Int -> Double
doublefact = gsl_sf_doublefact
foreign import ccall "gamma.h gsl_sf_doublefact" gsl_sf_doublefact :: Int -> Double

-- | wrapper for int gsl_sf_lnfact_e(int n,gsl_sf_result* result);
lnfact_e :: Int -> (Double,Double)
lnfact_e n = createSFR "lnfact_e" $ gsl_sf_lnfact_e n
foreign import ccall "gamma.h gsl_sf_lnfact_e" gsl_sf_lnfact_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lnfact(int n);
lnfact :: Int -> Double
lnfact = gsl_sf_lnfact
foreign import ccall "gamma.h gsl_sf_lnfact" gsl_sf_lnfact :: Int -> Double

-- | wrapper for int gsl_sf_lndoublefact_e(int n,gsl_sf_result* result);
lndoublefact_e :: Int -> (Double,Double)
lndoublefact_e n = createSFR "lndoublefact_e" $ gsl_sf_lndoublefact_e n
foreign import ccall "gamma.h gsl_sf_lndoublefact_e" gsl_sf_lndoublefact_e :: Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lndoublefact(int n);
lndoublefact :: Int -> Double
lndoublefact = gsl_sf_lndoublefact
foreign import ccall "gamma.h gsl_sf_lndoublefact" gsl_sf_lndoublefact :: Int -> Double

-- | wrapper for int gsl_sf_lnchoose_e(int n,int m,gsl_sf_result* result);
lnchoose_e :: Int -> Int -> (Double,Double)
lnchoose_e n m = createSFR "lnchoose_e" $ gsl_sf_lnchoose_e n m
foreign import ccall "gamma.h gsl_sf_lnchoose_e" gsl_sf_lnchoose_e :: Int -> Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lnchoose(int n,int m);
lnchoose :: Int -> Int -> Double
lnchoose = gsl_sf_lnchoose
foreign import ccall "gamma.h gsl_sf_lnchoose" gsl_sf_lnchoose :: Int -> Int -> Double

-- | wrapper for int gsl_sf_choose_e(int n,int m,gsl_sf_result* result);
choose_e :: Int -> Int -> (Double,Double)
choose_e n m = createSFR "choose_e" $ gsl_sf_choose_e n m
foreign import ccall "gamma.h gsl_sf_choose_e" gsl_sf_choose_e :: Int -> Int -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_choose(int n,int m);
choose :: Int -> Int -> Double
choose = gsl_sf_choose
foreign import ccall "gamma.h gsl_sf_choose" gsl_sf_choose :: Int -> Int -> Double

-- | wrapper for int gsl_sf_lnpoch_e(double a,double x,gsl_sf_result* result);
lnpoch_e :: Double -> Double -> (Double,Double)
lnpoch_e a x = createSFR "lnpoch_e" $ gsl_sf_lnpoch_e a x
foreign import ccall "gamma.h gsl_sf_lnpoch_e" gsl_sf_lnpoch_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lnpoch(double a,double x);
lnpoch :: Double -> Double -> Double
lnpoch = gsl_sf_lnpoch
foreign import ccall "gamma.h gsl_sf_lnpoch" gsl_sf_lnpoch :: Double -> Double -> Double

-- | wrapper for int gsl_sf_lnpoch_sgn_e(double a,double x,gsl_sf_result* result,double* sgn);
lnpoch_sgn_e :: Double -> Double -> Ptr Double -> Ptr Double -> Int
lnpoch_sgn_e = gsl_sf_lnpoch_sgn_e
foreign import ccall "gamma.h gsl_sf_lnpoch_sgn_e" gsl_sf_lnpoch_sgn_e :: Double -> Double -> Ptr Double -> Ptr Double -> Int

-- | wrapper for int gsl_sf_poch_e(double a,double x,gsl_sf_result* result);
poch_e :: Double -> Double -> (Double,Double)
poch_e a x = createSFR "poch_e" $ gsl_sf_poch_e a x
foreign import ccall "gamma.h gsl_sf_poch_e" gsl_sf_poch_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_poch(double a,double x);
poch :: Double -> Double -> Double
poch = gsl_sf_poch
foreign import ccall "gamma.h gsl_sf_poch" gsl_sf_poch :: Double -> Double -> Double

-- | wrapper for int gsl_sf_pochrel_e(double a,double x,gsl_sf_result* result);
pochrel_e :: Double -> Double -> (Double,Double)
pochrel_e a x = createSFR "pochrel_e" $ gsl_sf_pochrel_e a x
foreign import ccall "gamma.h gsl_sf_pochrel_e" gsl_sf_pochrel_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_pochrel(double a,double x);
pochrel :: Double -> Double -> Double
pochrel = gsl_sf_pochrel
foreign import ccall "gamma.h gsl_sf_pochrel" gsl_sf_pochrel :: Double -> Double -> Double

-- | wrapper for int gsl_sf_gamma_inc_Q_e(double a,double x,gsl_sf_result* result);
gamma_inc_Q_e :: Double -> Double -> (Double,Double)
gamma_inc_Q_e a x = createSFR "gamma_inc_Q_e" $ gsl_sf_gamma_inc_Q_e a x
foreign import ccall "gamma.h gsl_sf_gamma_inc_Q_e" gsl_sf_gamma_inc_Q_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gamma_inc_Q(double a,double x);
gamma_inc_Q :: Double -> Double -> Double
gamma_inc_Q = gsl_sf_gamma_inc_Q
foreign import ccall "gamma.h gsl_sf_gamma_inc_Q" gsl_sf_gamma_inc_Q :: Double -> Double -> Double

-- | wrapper for int gsl_sf_gamma_inc_P_e(double a,double x,gsl_sf_result* result);
gamma_inc_P_e :: Double -> Double -> (Double,Double)
gamma_inc_P_e a x = createSFR "gamma_inc_P_e" $ gsl_sf_gamma_inc_P_e a x
foreign import ccall "gamma.h gsl_sf_gamma_inc_P_e" gsl_sf_gamma_inc_P_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gamma_inc_P(double a,double x);
gamma_inc_P :: Double -> Double -> Double
gamma_inc_P = gsl_sf_gamma_inc_P
foreign import ccall "gamma.h gsl_sf_gamma_inc_P" gsl_sf_gamma_inc_P :: Double -> Double -> Double

-- | wrapper for int gsl_sf_gamma_inc_e(double a,double x,gsl_sf_result* result);
gamma_inc_e :: Double -> Double -> (Double,Double)
gamma_inc_e a x = createSFR "gamma_inc_e" $ gsl_sf_gamma_inc_e a x
foreign import ccall "gamma.h gsl_sf_gamma_inc_e" gsl_sf_gamma_inc_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_gamma_inc(double a,double x);
gamma_inc :: Double -> Double -> Double
gamma_inc = gsl_sf_gamma_inc
foreign import ccall "gamma.h gsl_sf_gamma_inc" gsl_sf_gamma_inc :: Double -> Double -> Double

-- | wrapper for int gsl_sf_lnbeta_e(double a,double b,gsl_sf_result* result);
lnbeta_e :: Double -> Double -> (Double,Double)
lnbeta_e a b = createSFR "lnbeta_e" $ gsl_sf_lnbeta_e a b
foreign import ccall "gamma.h gsl_sf_lnbeta_e" gsl_sf_lnbeta_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_lnbeta(double a,double b);
lnbeta :: Double -> Double -> Double
lnbeta = gsl_sf_lnbeta
foreign import ccall "gamma.h gsl_sf_lnbeta" gsl_sf_lnbeta :: Double -> Double -> Double

-- | wrapper for int gsl_sf_beta_e(double a,double b,gsl_sf_result* result);
beta_e :: Double -> Double -> (Double,Double)
beta_e a b = createSFR "beta_e" $ gsl_sf_beta_e a b
foreign import ccall "gamma.h gsl_sf_beta_e" gsl_sf_beta_e :: Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_beta(double a,double b);
beta :: Double -> Double -> Double
beta = gsl_sf_beta
foreign import ccall "gamma.h gsl_sf_beta" gsl_sf_beta :: Double -> Double -> Double

-- | wrapper for int gsl_sf_beta_inc_e(double a,double b,double x,gsl_sf_result* result);
beta_inc_e :: Double -> Double -> Double -> (Double,Double)
beta_inc_e a b x = createSFR "beta_inc_e" $ gsl_sf_beta_inc_e a b x
foreign import ccall "gamma.h gsl_sf_beta_inc_e" gsl_sf_beta_inc_e :: Double -> Double -> Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_beta_inc(double a,double b,double x);
beta_inc :: Double -> Double -> Double -> Double
beta_inc = gsl_sf_beta_inc
foreign import ccall "gamma.h gsl_sf_beta_inc" gsl_sf_beta_inc :: Double -> Double -> Double -> Double
