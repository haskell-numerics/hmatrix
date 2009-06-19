------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Hyperg
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_hyperg.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Hyperg(
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
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

hyperg_0F1_e :: Double -> Double -> (Double,Double)
hyperg_0F1_e c x = createSFR "hyperg_0F1_e" $ gsl_sf_hyperg_0F1_e c x
foreign import ccall "gsl_sf_hyperg_0F1_e" gsl_sf_hyperg_0F1_e :: Double -> Double -> Ptr () -> IO CInt

hyperg_0F1 :: Double -> Double -> Double
hyperg_0F1 = gsl_sf_hyperg_0F1
foreign import ccall "gsl_sf_hyperg_0F1" gsl_sf_hyperg_0F1 :: Double -> Double -> Double

hyperg_1F1_int_e :: CInt -> CInt -> Double -> (Double,Double)
hyperg_1F1_int_e m n x = createSFR "hyperg_1F1_int_e" $ gsl_sf_hyperg_1F1_int_e m n x
foreign import ccall "gsl_sf_hyperg_1F1_int_e" gsl_sf_hyperg_1F1_int_e :: CInt -> CInt -> Double -> Ptr () -> IO CInt

hyperg_1F1_int :: CInt -> CInt -> Double -> Double
hyperg_1F1_int = gsl_sf_hyperg_1F1_int
foreign import ccall "gsl_sf_hyperg_1F1_int" gsl_sf_hyperg_1F1_int :: CInt -> CInt -> Double -> Double

hyperg_1F1_e :: Double -> Double -> Double -> (Double,Double)
hyperg_1F1_e a b x = createSFR "hyperg_1F1_e" $ gsl_sf_hyperg_1F1_e a b x
foreign import ccall "gsl_sf_hyperg_1F1_e" gsl_sf_hyperg_1F1_e :: Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_1F1 :: Double -> Double -> Double -> Double
hyperg_1F1 = gsl_sf_hyperg_1F1
foreign import ccall "gsl_sf_hyperg_1F1" gsl_sf_hyperg_1F1 :: Double -> Double -> Double -> Double

hyperg_U_int_e :: CInt -> CInt -> Double -> (Double,Double)
hyperg_U_int_e m n x = createSFR "hyperg_U_int_e" $ gsl_sf_hyperg_U_int_e m n x
foreign import ccall "gsl_sf_hyperg_U_int_e" gsl_sf_hyperg_U_int_e :: CInt -> CInt -> Double -> Ptr () -> IO CInt

hyperg_U_int :: CInt -> CInt -> Double -> Double
hyperg_U_int = gsl_sf_hyperg_U_int
foreign import ccall "gsl_sf_hyperg_U_int" gsl_sf_hyperg_U_int :: CInt -> CInt -> Double -> Double

hyperg_U_int_e10_e :: CInt -> CInt -> Double -> (Double,Int,Double)
hyperg_U_int_e10_e m n x = createSFR_E10 "hyperg_U_int_e10_e" $ gsl_sf_hyperg_U_int_e10_e m n x
foreign import ccall "gsl_sf_hyperg_U_int_e10_e" gsl_sf_hyperg_U_int_e10_e :: CInt -> CInt -> Double -> Ptr () -> IO CInt

hyperg_U_e :: Double -> Double -> Double -> (Double,Double)
hyperg_U_e a b x = createSFR "hyperg_U_e" $ gsl_sf_hyperg_U_e a b x
foreign import ccall "gsl_sf_hyperg_U_e" gsl_sf_hyperg_U_e :: Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_U :: Double -> Double -> Double -> Double
hyperg_U = gsl_sf_hyperg_U
foreign import ccall "gsl_sf_hyperg_U" gsl_sf_hyperg_U :: Double -> Double -> Double -> Double

hyperg_U_e10_e :: Double -> Double -> Double -> (Double,Int,Double)
hyperg_U_e10_e a b x = createSFR_E10 "hyperg_U_e10_e" $ gsl_sf_hyperg_U_e10_e a b x
foreign import ccall "gsl_sf_hyperg_U_e10_e" gsl_sf_hyperg_U_e10_e :: Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_2F1_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_e a b c x = createSFR "hyperg_2F1_e" $ gsl_sf_hyperg_2F1_e a b c x
foreign import ccall "gsl_sf_hyperg_2F1_e" gsl_sf_hyperg_2F1_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_2F1 :: Double -> Double -> Double -> Double -> Double
hyperg_2F1 = gsl_sf_hyperg_2F1
foreign import ccall "gsl_sf_hyperg_2F1" gsl_sf_hyperg_2F1 :: Double -> Double -> Double -> Double -> Double

hyperg_2F1_conj_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_conj_e aR aI c x = createSFR "hyperg_2F1_conj_e" $ gsl_sf_hyperg_2F1_conj_e aR aI c x
foreign import ccall "gsl_sf_hyperg_2F1_conj_e" gsl_sf_hyperg_2F1_conj_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_2F1_conj :: Double -> Double -> Double -> Double -> Double
hyperg_2F1_conj = gsl_sf_hyperg_2F1_conj
foreign import ccall "gsl_sf_hyperg_2F1_conj" gsl_sf_hyperg_2F1_conj :: Double -> Double -> Double -> Double -> Double

hyperg_2F1_renorm_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_renorm_e a b c x = createSFR "hyperg_2F1_renorm_e" $ gsl_sf_hyperg_2F1_renorm_e a b c x
foreign import ccall "gsl_sf_hyperg_2F1_renorm_e" gsl_sf_hyperg_2F1_renorm_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_2F1_renorm :: Double -> Double -> Double -> Double -> Double
hyperg_2F1_renorm = gsl_sf_hyperg_2F1_renorm
foreign import ccall "gsl_sf_hyperg_2F1_renorm" gsl_sf_hyperg_2F1_renorm :: Double -> Double -> Double -> Double -> Double

hyperg_2F1_conj_renorm_e :: Double -> Double -> Double -> Double -> (Double,Double)
hyperg_2F1_conj_renorm_e aR aI c x = createSFR "hyperg_2F1_conj_renorm_e" $ gsl_sf_hyperg_2F1_conj_renorm_e aR aI c x
foreign import ccall "gsl_sf_hyperg_2F1_conj_renorm_e" gsl_sf_hyperg_2F1_conj_renorm_e :: Double -> Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_2F1_conj_renorm :: Double -> Double -> Double -> Double -> Double
hyperg_2F1_conj_renorm = gsl_sf_hyperg_2F1_conj_renorm
foreign import ccall "gsl_sf_hyperg_2F1_conj_renorm" gsl_sf_hyperg_2F1_conj_renorm :: Double -> Double -> Double -> Double -> Double

hyperg_2F0_e :: Double -> Double -> Double -> (Double,Double)
hyperg_2F0_e a b x = createSFR "hyperg_2F0_e" $ gsl_sf_hyperg_2F0_e a b x
foreign import ccall "gsl_sf_hyperg_2F0_e" gsl_sf_hyperg_2F0_e :: Double -> Double -> Double -> Ptr () -> IO CInt

hyperg_2F0 :: Double -> Double -> Double -> Double
hyperg_2F0 = gsl_sf_hyperg_2F0
foreign import ccall "gsl_sf_hyperg_2F0" gsl_sf_hyperg_2F0 :: Double -> Double -> Double -> Double
