------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Coulomb
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_coulomb.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Coulomb(
  hydrogenicR_1_e
, hydrogenicR_1
, hydrogenicR_e
, hydrogenicR
, coulomb_CL_e
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

hydrogenicR_1_e :: Double -> Double -> (Double,Double)
hydrogenicR_1_e zZ r = createSFR "hydrogenicR_1_e" $ gsl_sf_hydrogenicR_1_e zZ r
foreign import ccall "gsl_sf_hydrogenicR_1_e" gsl_sf_hydrogenicR_1_e :: Double -> Double -> Ptr () -> IO CInt

hydrogenicR_1 :: Double -> Double -> Double
hydrogenicR_1 = gsl_sf_hydrogenicR_1
foreign import ccall "gsl_sf_hydrogenicR_1" gsl_sf_hydrogenicR_1 :: Double -> Double -> Double

hydrogenicR_e :: CInt -> CInt -> Double -> Double -> (Double,Double)
hydrogenicR_e n l zZ r = createSFR "hydrogenicR_e" $ gsl_sf_hydrogenicR_e n l zZ r
foreign import ccall "gsl_sf_hydrogenicR_e" gsl_sf_hydrogenicR_e :: CInt -> CInt -> Double -> Double -> Ptr () -> IO CInt

hydrogenicR :: CInt -> CInt -> Double -> Double -> Double
hydrogenicR = gsl_sf_hydrogenicR
foreign import ccall "gsl_sf_hydrogenicR" gsl_sf_hydrogenicR :: CInt -> CInt -> Double -> Double -> Double

coulomb_wave_FG_e :: Double -> Double -> Double -> CInt -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr Double -> Ptr Double -> CInt
coulomb_wave_FG_e = gsl_sf_coulomb_wave_FG_e
foreign import ccall "gsl_sf_coulomb_wave_FG_e" gsl_sf_coulomb_wave_FG_e :: Double -> Double -> Double -> CInt -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr Double -> Ptr Double -> CInt

coulomb_wave_F_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> CInt
coulomb_wave_F_array = gsl_sf_coulomb_wave_F_array
foreign import ccall "gsl_sf_coulomb_wave_F_array" gsl_sf_coulomb_wave_F_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> CInt

coulomb_wave_FG_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt
coulomb_wave_FG_array = gsl_sf_coulomb_wave_FG_array
foreign import ccall "gsl_sf_coulomb_wave_FG_array" gsl_sf_coulomb_wave_FG_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt

coulomb_wave_FGp_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt
coulomb_wave_FGp_array = gsl_sf_coulomb_wave_FGp_array
foreign import ccall "gsl_sf_coulomb_wave_FGp_array" gsl_sf_coulomb_wave_FGp_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt

coulomb_wave_sphF_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> CInt
coulomb_wave_sphF_array = gsl_sf_coulomb_wave_sphF_array
foreign import ccall "gsl_sf_coulomb_wave_sphF_array" gsl_sf_coulomb_wave_sphF_array :: Double -> CInt -> Double -> Double -> Ptr Double -> Ptr Double -> CInt

coulomb_CL_e :: Double -> Double -> (Double,Double)
coulomb_CL_e lL eta = createSFR "coulomb_CL_e" $ gsl_sf_coulomb_CL_e lL eta
foreign import ccall "gsl_sf_coulomb_CL_e" gsl_sf_coulomb_CL_e :: Double -> Double -> Ptr () -> IO CInt

coulomb_CL_array :: Double -> CInt -> Double -> Ptr Double -> CInt
coulomb_CL_array = gsl_sf_coulomb_CL_array
foreign import ccall "gsl_sf_coulomb_CL_array" gsl_sf_coulomb_CL_array :: Double -> CInt -> Double -> Ptr Double -> CInt
