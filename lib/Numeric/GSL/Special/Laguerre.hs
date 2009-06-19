------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Laguerre
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_laguerre.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Laguerre(
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
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

laguerre_1_e :: Double -> Double -> (Double,Double)
laguerre_1_e a x = createSFR "laguerre_1_e" $ gsl_sf_laguerre_1_e a x
foreign import ccall "gsl_sf_laguerre_1_e" gsl_sf_laguerre_1_e :: Double -> Double -> Ptr () -> IO CInt

laguerre_2_e :: Double -> Double -> (Double,Double)
laguerre_2_e a x = createSFR "laguerre_2_e" $ gsl_sf_laguerre_2_e a x
foreign import ccall "gsl_sf_laguerre_2_e" gsl_sf_laguerre_2_e :: Double -> Double -> Ptr () -> IO CInt

laguerre_3_e :: Double -> Double -> (Double,Double)
laguerre_3_e a x = createSFR "laguerre_3_e" $ gsl_sf_laguerre_3_e a x
foreign import ccall "gsl_sf_laguerre_3_e" gsl_sf_laguerre_3_e :: Double -> Double -> Ptr () -> IO CInt

laguerre_1 :: Double -> Double -> Double
laguerre_1 = gsl_sf_laguerre_1
foreign import ccall "gsl_sf_laguerre_1" gsl_sf_laguerre_1 :: Double -> Double -> Double

laguerre_2 :: Double -> Double -> Double
laguerre_2 = gsl_sf_laguerre_2
foreign import ccall "gsl_sf_laguerre_2" gsl_sf_laguerre_2 :: Double -> Double -> Double

laguerre_3 :: Double -> Double -> Double
laguerre_3 = gsl_sf_laguerre_3
foreign import ccall "gsl_sf_laguerre_3" gsl_sf_laguerre_3 :: Double -> Double -> Double

laguerre_n_e :: CInt -> Double -> Double -> (Double,Double)
laguerre_n_e n a x = createSFR "laguerre_n_e" $ gsl_sf_laguerre_n_e n a x
foreign import ccall "gsl_sf_laguerre_n_e" gsl_sf_laguerre_n_e :: CInt -> Double -> Double -> Ptr () -> IO CInt

laguerre_n :: CInt -> Double -> Double -> Double
laguerre_n = gsl_sf_laguerre_n
foreign import ccall "gsl_sf_laguerre_n" gsl_sf_laguerre_n :: CInt -> Double -> Double -> Double
