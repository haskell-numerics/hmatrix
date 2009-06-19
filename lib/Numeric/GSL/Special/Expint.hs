------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Expint
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_expint.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Expint(
  expint_E1_e
, expint_E1
, expint_E2_e
, expint_E2
, expint_En_e
, expint_En
, expint_E1_scaled_e
, expint_E1_scaled
, expint_E2_scaled_e
, expint_E2_scaled
, expint_En_scaled_e
, expint_En_scaled
, expint_Ei_e
, expint_Ei
, expint_Ei_scaled_e
, expint_Ei_scaled
, shi_e
, shi
, chi_e
, chi
, expint_3_e
, expint_3
, si_e
, si
, ci_e
, ci
, atanint_e
, atanint
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

expint_E1_e :: Double -> (Double,Double)
expint_E1_e x = createSFR "expint_E1_e" $ gsl_sf_expint_E1_e x
foreign import ccall "gsl_sf_expint_E1_e" gsl_sf_expint_E1_e :: Double -> Ptr () -> IO CInt

expint_E1 :: Double -> Double
expint_E1 = gsl_sf_expint_E1
foreign import ccall "gsl_sf_expint_E1" gsl_sf_expint_E1 :: Double -> Double

expint_E2_e :: Double -> (Double,Double)
expint_E2_e x = createSFR "expint_E2_e" $ gsl_sf_expint_E2_e x
foreign import ccall "gsl_sf_expint_E2_e" gsl_sf_expint_E2_e :: Double -> Ptr () -> IO CInt

expint_E2 :: Double -> Double
expint_E2 = gsl_sf_expint_E2
foreign import ccall "gsl_sf_expint_E2" gsl_sf_expint_E2 :: Double -> Double

expint_En_e :: CInt -> Double -> (Double,Double)
expint_En_e n x = createSFR "expint_En_e" $ gsl_sf_expint_En_e n x
foreign import ccall "gsl_sf_expint_En_e" gsl_sf_expint_En_e :: CInt -> Double -> Ptr () -> IO CInt

expint_En :: CInt -> Double -> Double
expint_En = gsl_sf_expint_En
foreign import ccall "gsl_sf_expint_En" gsl_sf_expint_En :: CInt -> Double -> Double

expint_E1_scaled_e :: Double -> (Double,Double)
expint_E1_scaled_e x = createSFR "expint_E1_scaled_e" $ gsl_sf_expint_E1_scaled_e x
foreign import ccall "gsl_sf_expint_E1_scaled_e" gsl_sf_expint_E1_scaled_e :: Double -> Ptr () -> IO CInt

expint_E1_scaled :: Double -> Double
expint_E1_scaled = gsl_sf_expint_E1_scaled
foreign import ccall "gsl_sf_expint_E1_scaled" gsl_sf_expint_E1_scaled :: Double -> Double

expint_E2_scaled_e :: Double -> (Double,Double)
expint_E2_scaled_e x = createSFR "expint_E2_scaled_e" $ gsl_sf_expint_E2_scaled_e x
foreign import ccall "gsl_sf_expint_E2_scaled_e" gsl_sf_expint_E2_scaled_e :: Double -> Ptr () -> IO CInt

expint_E2_scaled :: Double -> Double
expint_E2_scaled = gsl_sf_expint_E2_scaled
foreign import ccall "gsl_sf_expint_E2_scaled" gsl_sf_expint_E2_scaled :: Double -> Double

expint_En_scaled_e :: CInt -> Double -> (Double,Double)
expint_En_scaled_e n x = createSFR "expint_En_scaled_e" $ gsl_sf_expint_En_scaled_e n x
foreign import ccall "gsl_sf_expint_En_scaled_e" gsl_sf_expint_En_scaled_e :: CInt -> Double -> Ptr () -> IO CInt

expint_En_scaled :: CInt -> Double -> Double
expint_En_scaled = gsl_sf_expint_En_scaled
foreign import ccall "gsl_sf_expint_En_scaled" gsl_sf_expint_En_scaled :: CInt -> Double -> Double

expint_Ei_e :: Double -> (Double,Double)
expint_Ei_e x = createSFR "expint_Ei_e" $ gsl_sf_expint_Ei_e x
foreign import ccall "gsl_sf_expint_Ei_e" gsl_sf_expint_Ei_e :: Double -> Ptr () -> IO CInt

expint_Ei :: Double -> Double
expint_Ei = gsl_sf_expint_Ei
foreign import ccall "gsl_sf_expint_Ei" gsl_sf_expint_Ei :: Double -> Double

expint_Ei_scaled_e :: Double -> (Double,Double)
expint_Ei_scaled_e x = createSFR "expint_Ei_scaled_e" $ gsl_sf_expint_Ei_scaled_e x
foreign import ccall "gsl_sf_expint_Ei_scaled_e" gsl_sf_expint_Ei_scaled_e :: Double -> Ptr () -> IO CInt

expint_Ei_scaled :: Double -> Double
expint_Ei_scaled = gsl_sf_expint_Ei_scaled
foreign import ccall "gsl_sf_expint_Ei_scaled" gsl_sf_expint_Ei_scaled :: Double -> Double

shi_e :: Double -> (Double,Double)
shi_e x = createSFR "shi_e" $ gsl_sf_Shi_e x
foreign import ccall "gsl_sf_Shi_e" gsl_sf_Shi_e :: Double -> Ptr () -> IO CInt

shi :: Double -> Double
shi = gsl_sf_Shi
foreign import ccall "gsl_sf_Shi" gsl_sf_Shi :: Double -> Double

chi_e :: Double -> (Double,Double)
chi_e x = createSFR "chi_e" $ gsl_sf_Chi_e x
foreign import ccall "gsl_sf_Chi_e" gsl_sf_Chi_e :: Double -> Ptr () -> IO CInt

chi :: Double -> Double
chi = gsl_sf_Chi
foreign import ccall "gsl_sf_Chi" gsl_sf_Chi :: Double -> Double

expint_3_e :: Double -> (Double,Double)
expint_3_e x = createSFR "expint_3_e" $ gsl_sf_expint_3_e x
foreign import ccall "gsl_sf_expint_3_e" gsl_sf_expint_3_e :: Double -> Ptr () -> IO CInt

expint_3 :: Double -> Double
expint_3 = gsl_sf_expint_3
foreign import ccall "gsl_sf_expint_3" gsl_sf_expint_3 :: Double -> Double

si_e :: Double -> (Double,Double)
si_e x = createSFR "si_e" $ gsl_sf_Si_e x
foreign import ccall "gsl_sf_Si_e" gsl_sf_Si_e :: Double -> Ptr () -> IO CInt

si :: Double -> Double
si = gsl_sf_Si
foreign import ccall "gsl_sf_Si" gsl_sf_Si :: Double -> Double

ci_e :: Double -> (Double,Double)
ci_e x = createSFR "ci_e" $ gsl_sf_Ci_e x
foreign import ccall "gsl_sf_Ci_e" gsl_sf_Ci_e :: Double -> Ptr () -> IO CInt

ci :: Double -> Double
ci = gsl_sf_Ci
foreign import ccall "gsl_sf_Ci" gsl_sf_Ci :: Double -> Double

atanint_e :: Double -> (Double,Double)
atanint_e x = createSFR "atanint_e" $ gsl_sf_atanint_e x
foreign import ccall "gsl_sf_atanint_e" gsl_sf_atanint_e :: Double -> Ptr () -> IO CInt

atanint :: Double -> Double
atanint = gsl_sf_atanint
foreign import ccall "gsl_sf_atanint" gsl_sf_atanint :: Double -> Double
