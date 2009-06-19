------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Coupling
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_coupling.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Coupling(
  coupling_3j_e
, coupling_3j
, coupling_6j_e
, coupling_6j
, coupling_RacahW_e
, coupling_RacahW
, coupling_9j_e
, coupling_9j
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

coupling_3j_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> (Double,Double)
coupling_3j_e two_ja two_jb two_jc two_ma two_mb two_mc = createSFR "coupling_3j_e" $ gsl_sf_coupling_3j_e two_ja two_jb two_jc two_ma two_mb two_mc
foreign import ccall "gsl_sf_coupling_3j_e" gsl_sf_coupling_3j_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt

coupling_3j :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double
coupling_3j = gsl_sf_coupling_3j
foreign import ccall "gsl_sf_coupling_3j" gsl_sf_coupling_3j :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double

coupling_6j_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> (Double,Double)
coupling_6j_e two_ja two_jb two_jc two_jd two_je two_jf = createSFR "coupling_6j_e" $ gsl_sf_coupling_6j_e two_ja two_jb two_jc two_jd two_je two_jf
foreign import ccall "gsl_sf_coupling_6j_e" gsl_sf_coupling_6j_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt

coupling_6j :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double
coupling_6j = gsl_sf_coupling_6j
foreign import ccall "gsl_sf_coupling_6j" gsl_sf_coupling_6j :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double

coupling_RacahW_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> (Double,Double)
coupling_RacahW_e two_ja two_jb two_jc two_jd two_je two_jf = createSFR "coupling_RacahW_e" $ gsl_sf_coupling_RacahW_e two_ja two_jb two_jc two_jd two_je two_jf
foreign import ccall "gsl_sf_coupling_RacahW_e" gsl_sf_coupling_RacahW_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt

coupling_RacahW :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double
coupling_RacahW = gsl_sf_coupling_RacahW
foreign import ccall "gsl_sf_coupling_RacahW" gsl_sf_coupling_RacahW :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double

coupling_9j_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> (Double,Double)
coupling_9j_e two_ja two_jb two_jc two_jd two_je two_jf two_jg two_jh two_ji = createSFR "coupling_9j_e" $ gsl_sf_coupling_9j_e two_ja two_jb two_jc two_jd two_je two_jf two_jg two_jh two_ji
foreign import ccall "gsl_sf_coupling_9j_e" gsl_sf_coupling_9j_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt

coupling_9j :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double
coupling_9j = gsl_sf_coupling_9j
foreign import ccall "gsl_sf_coupling_9j" gsl_sf_coupling_9j :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double

coupling_6j_INCORRECT_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> (Double,Double)
coupling_6j_INCORRECT_e two_ja two_jb two_jc two_jd two_je two_jf = createSFR "coupling_6j_INCORRECT_e" $ gsl_sf_coupling_6j_INCORRECT_e two_ja two_jb two_jc two_jd two_je two_jf
foreign import ccall "gsl_sf_coupling_6j_INCORRECT_e" gsl_sf_coupling_6j_INCORRECT_e :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CInt

coupling_6j_INCORRECT :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double
coupling_6j_INCORRECT = gsl_sf_coupling_6j_INCORRECT
foreign import ccall "gsl_sf_coupling_6j_INCORRECT" gsl_sf_coupling_6j_INCORRECT :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Double
