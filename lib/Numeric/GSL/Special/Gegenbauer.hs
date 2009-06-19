------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Gegenbauer
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_gegenbauer.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Gegenbauer(
  gegenpoly_1_e
, gegenpoly_2_e
, gegenpoly_3_e
, gegenpoly_1
, gegenpoly_2
, gegenpoly_3
, gegenpoly_n_e
, gegenpoly_n
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

gegenpoly_1_e :: Double -> Double -> (Double,Double)
gegenpoly_1_e lambda x = createSFR "gegenpoly_1_e" $ gsl_sf_gegenpoly_1_e lambda x
foreign import ccall "gsl_sf_gegenpoly_1_e" gsl_sf_gegenpoly_1_e :: Double -> Double -> Ptr () -> IO CInt

gegenpoly_2_e :: Double -> Double -> (Double,Double)
gegenpoly_2_e lambda x = createSFR "gegenpoly_2_e" $ gsl_sf_gegenpoly_2_e lambda x
foreign import ccall "gsl_sf_gegenpoly_2_e" gsl_sf_gegenpoly_2_e :: Double -> Double -> Ptr () -> IO CInt

gegenpoly_3_e :: Double -> Double -> (Double,Double)
gegenpoly_3_e lambda x = createSFR "gegenpoly_3_e" $ gsl_sf_gegenpoly_3_e lambda x
foreign import ccall "gsl_sf_gegenpoly_3_e" gsl_sf_gegenpoly_3_e :: Double -> Double -> Ptr () -> IO CInt

gegenpoly_1 :: Double -> Double -> Double
gegenpoly_1 = gsl_sf_gegenpoly_1
foreign import ccall "gsl_sf_gegenpoly_1" gsl_sf_gegenpoly_1 :: Double -> Double -> Double

gegenpoly_2 :: Double -> Double -> Double
gegenpoly_2 = gsl_sf_gegenpoly_2
foreign import ccall "gsl_sf_gegenpoly_2" gsl_sf_gegenpoly_2 :: Double -> Double -> Double

gegenpoly_3 :: Double -> Double -> Double
gegenpoly_3 = gsl_sf_gegenpoly_3
foreign import ccall "gsl_sf_gegenpoly_3" gsl_sf_gegenpoly_3 :: Double -> Double -> Double

gegenpoly_n_e :: CInt -> Double -> Double -> (Double,Double)
gegenpoly_n_e n lambda x = createSFR "gegenpoly_n_e" $ gsl_sf_gegenpoly_n_e n lambda x
foreign import ccall "gsl_sf_gegenpoly_n_e" gsl_sf_gegenpoly_n_e :: CInt -> Double -> Double -> Ptr () -> IO CInt

gegenpoly_n :: CInt -> Double -> Double -> Double
gegenpoly_n = gsl_sf_gegenpoly_n
foreign import ccall "gsl_sf_gegenpoly_n" gsl_sf_gegenpoly_n :: CInt -> Double -> Double -> Double

gegenpoly_array :: CInt -> Double -> Double -> Ptr Double -> CInt
gegenpoly_array = gsl_sf_gegenpoly_array
foreign import ccall "gsl_sf_gegenpoly_array" gsl_sf_gegenpoly_array :: CInt -> Double -> Double -> Ptr Double -> CInt
