------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Bessel
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_bessel.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Bessel(
  bessel_J0_e
, bessel_J0
, bessel_J1_e
, bessel_J1
, bessel_Jn_e
, bessel_Jn
, bessel_Y0_e
, bessel_Y0
, bessel_Y1_e
, bessel_Y1
, bessel_Yn_e
, bessel_Yn
, bessel_I0_e
, bessel_I0
, bessel_I1_e
, bessel_I1
, bessel_In_e
, bessel_In
, bessel_I0_scaled_e
, bessel_I0_scaled
, bessel_I1_scaled_e
, bessel_I1_scaled
, bessel_In_scaled_e
, bessel_In_scaled
, bessel_K0_e
, bessel_K0
, bessel_K1_e
, bessel_K1
, bessel_Kn_e
, bessel_Kn
, bessel_K0_scaled_e
, bessel_K0_scaled
, bessel_K1_scaled_e
, bessel_K1_scaled
, bessel_Kn_scaled_e
, bessel_Kn_scaled
, bessel_j0_e
, bessel_j0
, bessel_j1_e
, bessel_j1
, bessel_j2_e
, bessel_j2
, bessel_jl_e
, bessel_jl
, bessel_y0_e
, bessel_y0
, bessel_y1_e
, bessel_y1
, bessel_y2_e
, bessel_y2
, bessel_yl_e
, bessel_yl
, bessel_i0_scaled_e
, bessel_i0_scaled
, bessel_i1_scaled_e
, bessel_i1_scaled
, bessel_i2_scaled_e
, bessel_i2_scaled
, bessel_il_scaled_e
, bessel_il_scaled
, bessel_k0_scaled_e
, bessel_k0_scaled
, bessel_k1_scaled_e
, bessel_k1_scaled
, bessel_k2_scaled_e
, bessel_k2_scaled
, bessel_kl_scaled_e
, bessel_kl_scaled
, bessel_Jnu_e
, bessel_Jnu
, bessel_Ynu_e
, bessel_Ynu
, bessel_Inu_scaled_e
, bessel_Inu_scaled
, bessel_Inu_e
, bessel_Inu
, bessel_Knu_scaled_e
, bessel_Knu_scaled
, bessel_Knu_e
, bessel_Knu
, bessel_lnKnu_e
, bessel_lnKnu
, bessel_zero_J0_e
, bessel_zero_J0
, bessel_zero_J1_e
, bessel_zero_J1
, bessel_zero_Jnu_e
, bessel_zero_Jnu
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

bessel_J0_e :: Double -> (Double,Double)
bessel_J0_e x = createSFR "bessel_J0_e" $ gsl_sf_bessel_J0_e x
foreign import ccall "gsl_sf_bessel_J0_e" gsl_sf_bessel_J0_e :: Double -> Ptr () -> IO CInt

bessel_J0 :: Double -> Double
bessel_J0 = gsl_sf_bessel_J0
foreign import ccall "gsl_sf_bessel_J0" gsl_sf_bessel_J0 :: Double -> Double

bessel_J1_e :: Double -> (Double,Double)
bessel_J1_e x = createSFR "bessel_J1_e" $ gsl_sf_bessel_J1_e x
foreign import ccall "gsl_sf_bessel_J1_e" gsl_sf_bessel_J1_e :: Double -> Ptr () -> IO CInt

bessel_J1 :: Double -> Double
bessel_J1 = gsl_sf_bessel_J1
foreign import ccall "gsl_sf_bessel_J1" gsl_sf_bessel_J1 :: Double -> Double

bessel_Jn_e :: CInt -> Double -> (Double,Double)
bessel_Jn_e n x = createSFR "bessel_Jn_e" $ gsl_sf_bessel_Jn_e n x
foreign import ccall "gsl_sf_bessel_Jn_e" gsl_sf_bessel_Jn_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_Jn :: CInt -> Double -> Double
bessel_Jn = gsl_sf_bessel_Jn
foreign import ccall "gsl_sf_bessel_Jn" gsl_sf_bessel_Jn :: CInt -> Double -> Double

bessel_Jn_array :: CInt -> CInt -> Double -> Ptr Double -> CInt
bessel_Jn_array = gsl_sf_bessel_Jn_array
foreign import ccall "gsl_sf_bessel_Jn_array" gsl_sf_bessel_Jn_array :: CInt -> CInt -> Double -> Ptr Double -> CInt

bessel_Y0_e :: Double -> (Double,Double)
bessel_Y0_e x = createSFR "bessel_Y0_e" $ gsl_sf_bessel_Y0_e x
foreign import ccall "gsl_sf_bessel_Y0_e" gsl_sf_bessel_Y0_e :: Double -> Ptr () -> IO CInt

bessel_Y0 :: Double -> Double
bessel_Y0 = gsl_sf_bessel_Y0
foreign import ccall "gsl_sf_bessel_Y0" gsl_sf_bessel_Y0 :: Double -> Double

bessel_Y1_e :: Double -> (Double,Double)
bessel_Y1_e x = createSFR "bessel_Y1_e" $ gsl_sf_bessel_Y1_e x
foreign import ccall "gsl_sf_bessel_Y1_e" gsl_sf_bessel_Y1_e :: Double -> Ptr () -> IO CInt

bessel_Y1 :: Double -> Double
bessel_Y1 = gsl_sf_bessel_Y1
foreign import ccall "gsl_sf_bessel_Y1" gsl_sf_bessel_Y1 :: Double -> Double

bessel_Yn_e :: CInt -> Double -> (Double,Double)
bessel_Yn_e n x = createSFR "bessel_Yn_e" $ gsl_sf_bessel_Yn_e n x
foreign import ccall "gsl_sf_bessel_Yn_e" gsl_sf_bessel_Yn_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_Yn :: CInt -> Double -> Double
bessel_Yn = gsl_sf_bessel_Yn
foreign import ccall "gsl_sf_bessel_Yn" gsl_sf_bessel_Yn :: CInt -> Double -> Double

bessel_Yn_array :: CInt -> CInt -> Double -> Ptr Double -> CInt
bessel_Yn_array = gsl_sf_bessel_Yn_array
foreign import ccall "gsl_sf_bessel_Yn_array" gsl_sf_bessel_Yn_array :: CInt -> CInt -> Double -> Ptr Double -> CInt

bessel_I0_e :: Double -> (Double,Double)
bessel_I0_e x = createSFR "bessel_I0_e" $ gsl_sf_bessel_I0_e x
foreign import ccall "gsl_sf_bessel_I0_e" gsl_sf_bessel_I0_e :: Double -> Ptr () -> IO CInt

bessel_I0 :: Double -> Double
bessel_I0 = gsl_sf_bessel_I0
foreign import ccall "gsl_sf_bessel_I0" gsl_sf_bessel_I0 :: Double -> Double

bessel_I1_e :: Double -> (Double,Double)
bessel_I1_e x = createSFR "bessel_I1_e" $ gsl_sf_bessel_I1_e x
foreign import ccall "gsl_sf_bessel_I1_e" gsl_sf_bessel_I1_e :: Double -> Ptr () -> IO CInt

bessel_I1 :: Double -> Double
bessel_I1 = gsl_sf_bessel_I1
foreign import ccall "gsl_sf_bessel_I1" gsl_sf_bessel_I1 :: Double -> Double

bessel_In_e :: CInt -> Double -> (Double,Double)
bessel_In_e n x = createSFR "bessel_In_e" $ gsl_sf_bessel_In_e n x
foreign import ccall "gsl_sf_bessel_In_e" gsl_sf_bessel_In_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_In :: CInt -> Double -> Double
bessel_In = gsl_sf_bessel_In
foreign import ccall "gsl_sf_bessel_In" gsl_sf_bessel_In :: CInt -> Double -> Double

bessel_In_array :: CInt -> CInt -> Double -> Ptr Double -> CInt
bessel_In_array = gsl_sf_bessel_In_array
foreign import ccall "gsl_sf_bessel_In_array" gsl_sf_bessel_In_array :: CInt -> CInt -> Double -> Ptr Double -> CInt

bessel_I0_scaled_e :: Double -> (Double,Double)
bessel_I0_scaled_e x = createSFR "bessel_I0_scaled_e" $ gsl_sf_bessel_I0_scaled_e x
foreign import ccall "gsl_sf_bessel_I0_scaled_e" gsl_sf_bessel_I0_scaled_e :: Double -> Ptr () -> IO CInt

bessel_I0_scaled :: Double -> Double
bessel_I0_scaled = gsl_sf_bessel_I0_scaled
foreign import ccall "gsl_sf_bessel_I0_scaled" gsl_sf_bessel_I0_scaled :: Double -> Double

bessel_I1_scaled_e :: Double -> (Double,Double)
bessel_I1_scaled_e x = createSFR "bessel_I1_scaled_e" $ gsl_sf_bessel_I1_scaled_e x
foreign import ccall "gsl_sf_bessel_I1_scaled_e" gsl_sf_bessel_I1_scaled_e :: Double -> Ptr () -> IO CInt

bessel_I1_scaled :: Double -> Double
bessel_I1_scaled = gsl_sf_bessel_I1_scaled
foreign import ccall "gsl_sf_bessel_I1_scaled" gsl_sf_bessel_I1_scaled :: Double -> Double

bessel_In_scaled_e :: CInt -> Double -> (Double,Double)
bessel_In_scaled_e n x = createSFR "bessel_In_scaled_e" $ gsl_sf_bessel_In_scaled_e n x
foreign import ccall "gsl_sf_bessel_In_scaled_e" gsl_sf_bessel_In_scaled_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_In_scaled :: CInt -> Double -> Double
bessel_In_scaled = gsl_sf_bessel_In_scaled
foreign import ccall "gsl_sf_bessel_In_scaled" gsl_sf_bessel_In_scaled :: CInt -> Double -> Double

bessel_In_scaled_array :: CInt -> CInt -> Double -> Ptr Double -> CInt
bessel_In_scaled_array = gsl_sf_bessel_In_scaled_array
foreign import ccall "gsl_sf_bessel_In_scaled_array" gsl_sf_bessel_In_scaled_array :: CInt -> CInt -> Double -> Ptr Double -> CInt

bessel_K0_e :: Double -> (Double,Double)
bessel_K0_e x = createSFR "bessel_K0_e" $ gsl_sf_bessel_K0_e x
foreign import ccall "gsl_sf_bessel_K0_e" gsl_sf_bessel_K0_e :: Double -> Ptr () -> IO CInt

bessel_K0 :: Double -> Double
bessel_K0 = gsl_sf_bessel_K0
foreign import ccall "gsl_sf_bessel_K0" gsl_sf_bessel_K0 :: Double -> Double

bessel_K1_e :: Double -> (Double,Double)
bessel_K1_e x = createSFR "bessel_K1_e" $ gsl_sf_bessel_K1_e x
foreign import ccall "gsl_sf_bessel_K1_e" gsl_sf_bessel_K1_e :: Double -> Ptr () -> IO CInt

bessel_K1 :: Double -> Double
bessel_K1 = gsl_sf_bessel_K1
foreign import ccall "gsl_sf_bessel_K1" gsl_sf_bessel_K1 :: Double -> Double

bessel_Kn_e :: CInt -> Double -> (Double,Double)
bessel_Kn_e n x = createSFR "bessel_Kn_e" $ gsl_sf_bessel_Kn_e n x
foreign import ccall "gsl_sf_bessel_Kn_e" gsl_sf_bessel_Kn_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_Kn :: CInt -> Double -> Double
bessel_Kn = gsl_sf_bessel_Kn
foreign import ccall "gsl_sf_bessel_Kn" gsl_sf_bessel_Kn :: CInt -> Double -> Double

bessel_Kn_array :: CInt -> CInt -> Double -> Ptr Double -> CInt
bessel_Kn_array = gsl_sf_bessel_Kn_array
foreign import ccall "gsl_sf_bessel_Kn_array" gsl_sf_bessel_Kn_array :: CInt -> CInt -> Double -> Ptr Double -> CInt

bessel_K0_scaled_e :: Double -> (Double,Double)
bessel_K0_scaled_e x = createSFR "bessel_K0_scaled_e" $ gsl_sf_bessel_K0_scaled_e x
foreign import ccall "gsl_sf_bessel_K0_scaled_e" gsl_sf_bessel_K0_scaled_e :: Double -> Ptr () -> IO CInt

bessel_K0_scaled :: Double -> Double
bessel_K0_scaled = gsl_sf_bessel_K0_scaled
foreign import ccall "gsl_sf_bessel_K0_scaled" gsl_sf_bessel_K0_scaled :: Double -> Double

bessel_K1_scaled_e :: Double -> (Double,Double)
bessel_K1_scaled_e x = createSFR "bessel_K1_scaled_e" $ gsl_sf_bessel_K1_scaled_e x
foreign import ccall "gsl_sf_bessel_K1_scaled_e" gsl_sf_bessel_K1_scaled_e :: Double -> Ptr () -> IO CInt

bessel_K1_scaled :: Double -> Double
bessel_K1_scaled = gsl_sf_bessel_K1_scaled
foreign import ccall "gsl_sf_bessel_K1_scaled" gsl_sf_bessel_K1_scaled :: Double -> Double

bessel_Kn_scaled_e :: CInt -> Double -> (Double,Double)
bessel_Kn_scaled_e n x = createSFR "bessel_Kn_scaled_e" $ gsl_sf_bessel_Kn_scaled_e n x
foreign import ccall "gsl_sf_bessel_Kn_scaled_e" gsl_sf_bessel_Kn_scaled_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_Kn_scaled :: CInt -> Double -> Double
bessel_Kn_scaled = gsl_sf_bessel_Kn_scaled
foreign import ccall "gsl_sf_bessel_Kn_scaled" gsl_sf_bessel_Kn_scaled :: CInt -> Double -> Double

bessel_Kn_scaled_array :: CInt -> CInt -> Double -> Ptr Double -> CInt
bessel_Kn_scaled_array = gsl_sf_bessel_Kn_scaled_array
foreign import ccall "gsl_sf_bessel_Kn_scaled_array" gsl_sf_bessel_Kn_scaled_array :: CInt -> CInt -> Double -> Ptr Double -> CInt

bessel_j0_e :: Double -> (Double,Double)
bessel_j0_e x = createSFR "bessel_j0_e" $ gsl_sf_bessel_j0_e x
foreign import ccall "gsl_sf_bessel_j0_e" gsl_sf_bessel_j0_e :: Double -> Ptr () -> IO CInt

bessel_j0 :: Double -> Double
bessel_j0 = gsl_sf_bessel_j0
foreign import ccall "gsl_sf_bessel_j0" gsl_sf_bessel_j0 :: Double -> Double

bessel_j1_e :: Double -> (Double,Double)
bessel_j1_e x = createSFR "bessel_j1_e" $ gsl_sf_bessel_j1_e x
foreign import ccall "gsl_sf_bessel_j1_e" gsl_sf_bessel_j1_e :: Double -> Ptr () -> IO CInt

bessel_j1 :: Double -> Double
bessel_j1 = gsl_sf_bessel_j1
foreign import ccall "gsl_sf_bessel_j1" gsl_sf_bessel_j1 :: Double -> Double

bessel_j2_e :: Double -> (Double,Double)
bessel_j2_e x = createSFR "bessel_j2_e" $ gsl_sf_bessel_j2_e x
foreign import ccall "gsl_sf_bessel_j2_e" gsl_sf_bessel_j2_e :: Double -> Ptr () -> IO CInt

bessel_j2 :: Double -> Double
bessel_j2 = gsl_sf_bessel_j2
foreign import ccall "gsl_sf_bessel_j2" gsl_sf_bessel_j2 :: Double -> Double

bessel_jl_e :: CInt -> Double -> (Double,Double)
bessel_jl_e l x = createSFR "bessel_jl_e" $ gsl_sf_bessel_jl_e l x
foreign import ccall "gsl_sf_bessel_jl_e" gsl_sf_bessel_jl_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_jl :: CInt -> Double -> Double
bessel_jl = gsl_sf_bessel_jl
foreign import ccall "gsl_sf_bessel_jl" gsl_sf_bessel_jl :: CInt -> Double -> Double

bessel_jl_array :: CInt -> Double -> Ptr Double -> CInt
bessel_jl_array = gsl_sf_bessel_jl_array
foreign import ccall "gsl_sf_bessel_jl_array" gsl_sf_bessel_jl_array :: CInt -> Double -> Ptr Double -> CInt

bessel_jl_steed_array :: CInt -> Double -> Ptr Double -> CInt
bessel_jl_steed_array = gsl_sf_bessel_jl_steed_array
foreign import ccall "gsl_sf_bessel_jl_steed_array" gsl_sf_bessel_jl_steed_array :: CInt -> Double -> Ptr Double -> CInt

bessel_y0_e :: Double -> (Double,Double)
bessel_y0_e x = createSFR "bessel_y0_e" $ gsl_sf_bessel_y0_e x
foreign import ccall "gsl_sf_bessel_y0_e" gsl_sf_bessel_y0_e :: Double -> Ptr () -> IO CInt

bessel_y0 :: Double -> Double
bessel_y0 = gsl_sf_bessel_y0
foreign import ccall "gsl_sf_bessel_y0" gsl_sf_bessel_y0 :: Double -> Double

bessel_y1_e :: Double -> (Double,Double)
bessel_y1_e x = createSFR "bessel_y1_e" $ gsl_sf_bessel_y1_e x
foreign import ccall "gsl_sf_bessel_y1_e" gsl_sf_bessel_y1_e :: Double -> Ptr () -> IO CInt

bessel_y1 :: Double -> Double
bessel_y1 = gsl_sf_bessel_y1
foreign import ccall "gsl_sf_bessel_y1" gsl_sf_bessel_y1 :: Double -> Double

bessel_y2_e :: Double -> (Double,Double)
bessel_y2_e x = createSFR "bessel_y2_e" $ gsl_sf_bessel_y2_e x
foreign import ccall "gsl_sf_bessel_y2_e" gsl_sf_bessel_y2_e :: Double -> Ptr () -> IO CInt

bessel_y2 :: Double -> Double
bessel_y2 = gsl_sf_bessel_y2
foreign import ccall "gsl_sf_bessel_y2" gsl_sf_bessel_y2 :: Double -> Double

bessel_yl_e :: CInt -> Double -> (Double,Double)
bessel_yl_e l x = createSFR "bessel_yl_e" $ gsl_sf_bessel_yl_e l x
foreign import ccall "gsl_sf_bessel_yl_e" gsl_sf_bessel_yl_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_yl :: CInt -> Double -> Double
bessel_yl = gsl_sf_bessel_yl
foreign import ccall "gsl_sf_bessel_yl" gsl_sf_bessel_yl :: CInt -> Double -> Double

bessel_yl_array :: CInt -> Double -> Ptr Double -> CInt
bessel_yl_array = gsl_sf_bessel_yl_array
foreign import ccall "gsl_sf_bessel_yl_array" gsl_sf_bessel_yl_array :: CInt -> Double -> Ptr Double -> CInt

bessel_i0_scaled_e :: Double -> (Double,Double)
bessel_i0_scaled_e x = createSFR "bessel_i0_scaled_e" $ gsl_sf_bessel_i0_scaled_e x
foreign import ccall "gsl_sf_bessel_i0_scaled_e" gsl_sf_bessel_i0_scaled_e :: Double -> Ptr () -> IO CInt

bessel_i0_scaled :: Double -> Double
bessel_i0_scaled = gsl_sf_bessel_i0_scaled
foreign import ccall "gsl_sf_bessel_i0_scaled" gsl_sf_bessel_i0_scaled :: Double -> Double

bessel_i1_scaled_e :: Double -> (Double,Double)
bessel_i1_scaled_e x = createSFR "bessel_i1_scaled_e" $ gsl_sf_bessel_i1_scaled_e x
foreign import ccall "gsl_sf_bessel_i1_scaled_e" gsl_sf_bessel_i1_scaled_e :: Double -> Ptr () -> IO CInt

bessel_i1_scaled :: Double -> Double
bessel_i1_scaled = gsl_sf_bessel_i1_scaled
foreign import ccall "gsl_sf_bessel_i1_scaled" gsl_sf_bessel_i1_scaled :: Double -> Double

bessel_i2_scaled_e :: Double -> (Double,Double)
bessel_i2_scaled_e x = createSFR "bessel_i2_scaled_e" $ gsl_sf_bessel_i2_scaled_e x
foreign import ccall "gsl_sf_bessel_i2_scaled_e" gsl_sf_bessel_i2_scaled_e :: Double -> Ptr () -> IO CInt

bessel_i2_scaled :: Double -> Double
bessel_i2_scaled = gsl_sf_bessel_i2_scaled
foreign import ccall "gsl_sf_bessel_i2_scaled" gsl_sf_bessel_i2_scaled :: Double -> Double

bessel_il_scaled_e :: CInt -> Double -> (Double,Double)
bessel_il_scaled_e l x = createSFR "bessel_il_scaled_e" $ gsl_sf_bessel_il_scaled_e l x
foreign import ccall "gsl_sf_bessel_il_scaled_e" gsl_sf_bessel_il_scaled_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_il_scaled :: CInt -> Double -> Double
bessel_il_scaled = gsl_sf_bessel_il_scaled
foreign import ccall "gsl_sf_bessel_il_scaled" gsl_sf_bessel_il_scaled :: CInt -> Double -> Double

bessel_il_scaled_array :: CInt -> Double -> Ptr Double -> CInt
bessel_il_scaled_array = gsl_sf_bessel_il_scaled_array
foreign import ccall "gsl_sf_bessel_il_scaled_array" gsl_sf_bessel_il_scaled_array :: CInt -> Double -> Ptr Double -> CInt

bessel_k0_scaled_e :: Double -> (Double,Double)
bessel_k0_scaled_e x = createSFR "bessel_k0_scaled_e" $ gsl_sf_bessel_k0_scaled_e x
foreign import ccall "gsl_sf_bessel_k0_scaled_e" gsl_sf_bessel_k0_scaled_e :: Double -> Ptr () -> IO CInt

bessel_k0_scaled :: Double -> Double
bessel_k0_scaled = gsl_sf_bessel_k0_scaled
foreign import ccall "gsl_sf_bessel_k0_scaled" gsl_sf_bessel_k0_scaled :: Double -> Double

bessel_k1_scaled_e :: Double -> (Double,Double)
bessel_k1_scaled_e x = createSFR "bessel_k1_scaled_e" $ gsl_sf_bessel_k1_scaled_e x
foreign import ccall "gsl_sf_bessel_k1_scaled_e" gsl_sf_bessel_k1_scaled_e :: Double -> Ptr () -> IO CInt

bessel_k1_scaled :: Double -> Double
bessel_k1_scaled = gsl_sf_bessel_k1_scaled
foreign import ccall "gsl_sf_bessel_k1_scaled" gsl_sf_bessel_k1_scaled :: Double -> Double

bessel_k2_scaled_e :: Double -> (Double,Double)
bessel_k2_scaled_e x = createSFR "bessel_k2_scaled_e" $ gsl_sf_bessel_k2_scaled_e x
foreign import ccall "gsl_sf_bessel_k2_scaled_e" gsl_sf_bessel_k2_scaled_e :: Double -> Ptr () -> IO CInt

bessel_k2_scaled :: Double -> Double
bessel_k2_scaled = gsl_sf_bessel_k2_scaled
foreign import ccall "gsl_sf_bessel_k2_scaled" gsl_sf_bessel_k2_scaled :: Double -> Double

bessel_kl_scaled_e :: CInt -> Double -> (Double,Double)
bessel_kl_scaled_e l x = createSFR "bessel_kl_scaled_e" $ gsl_sf_bessel_kl_scaled_e l x
foreign import ccall "gsl_sf_bessel_kl_scaled_e" gsl_sf_bessel_kl_scaled_e :: CInt -> Double -> Ptr () -> IO CInt

bessel_kl_scaled :: CInt -> Double -> Double
bessel_kl_scaled = gsl_sf_bessel_kl_scaled
foreign import ccall "gsl_sf_bessel_kl_scaled" gsl_sf_bessel_kl_scaled :: CInt -> Double -> Double

bessel_kl_scaled_array :: CInt -> Double -> Ptr Double -> CInt
bessel_kl_scaled_array = gsl_sf_bessel_kl_scaled_array
foreign import ccall "gsl_sf_bessel_kl_scaled_array" gsl_sf_bessel_kl_scaled_array :: CInt -> Double -> Ptr Double -> CInt

bessel_Jnu_e :: Double -> Double -> (Double,Double)
bessel_Jnu_e nu x = createSFR "bessel_Jnu_e" $ gsl_sf_bessel_Jnu_e nu x
foreign import ccall "gsl_sf_bessel_Jnu_e" gsl_sf_bessel_Jnu_e :: Double -> Double -> Ptr () -> IO CInt

bessel_Jnu :: Double -> Double -> Double
bessel_Jnu = gsl_sf_bessel_Jnu
foreign import ccall "gsl_sf_bessel_Jnu" gsl_sf_bessel_Jnu :: Double -> Double -> Double

bessel_Ynu_e :: Double -> Double -> (Double,Double)
bessel_Ynu_e nu x = createSFR "bessel_Ynu_e" $ gsl_sf_bessel_Ynu_e nu x
foreign import ccall "gsl_sf_bessel_Ynu_e" gsl_sf_bessel_Ynu_e :: Double -> Double -> Ptr () -> IO CInt

bessel_Ynu :: Double -> Double -> Double
bessel_Ynu = gsl_sf_bessel_Ynu
foreign import ccall "gsl_sf_bessel_Ynu" gsl_sf_bessel_Ynu :: Double -> Double -> Double

bessel_sequence_Jnu_e :: Double -> Precision -> Size_t -> Ptr Double -> CInt
bessel_sequence_Jnu_e nu mode size v = gsl_sf_bessel_sequence_Jnu_e nu  (precCode mode) size v
foreign import ccall "gsl_sf_bessel_sequence_Jnu_e" gsl_sf_bessel_sequence_Jnu_e :: Double -> Gsl_mode_t -> Size_t -> Ptr Double -> CInt

bessel_Inu_scaled_e :: Double -> Double -> (Double,Double)
bessel_Inu_scaled_e nu x = createSFR "bessel_Inu_scaled_e" $ gsl_sf_bessel_Inu_scaled_e nu x
foreign import ccall "gsl_sf_bessel_Inu_scaled_e" gsl_sf_bessel_Inu_scaled_e :: Double -> Double -> Ptr () -> IO CInt

bessel_Inu_scaled :: Double -> Double -> Double
bessel_Inu_scaled = gsl_sf_bessel_Inu_scaled
foreign import ccall "gsl_sf_bessel_Inu_scaled" gsl_sf_bessel_Inu_scaled :: Double -> Double -> Double

bessel_Inu_e :: Double -> Double -> (Double,Double)
bessel_Inu_e nu x = createSFR "bessel_Inu_e" $ gsl_sf_bessel_Inu_e nu x
foreign import ccall "gsl_sf_bessel_Inu_e" gsl_sf_bessel_Inu_e :: Double -> Double -> Ptr () -> IO CInt

bessel_Inu :: Double -> Double -> Double
bessel_Inu = gsl_sf_bessel_Inu
foreign import ccall "gsl_sf_bessel_Inu" gsl_sf_bessel_Inu :: Double -> Double -> Double

bessel_Knu_scaled_e :: Double -> Double -> (Double,Double)
bessel_Knu_scaled_e nu x = createSFR "bessel_Knu_scaled_e" $ gsl_sf_bessel_Knu_scaled_e nu x
foreign import ccall "gsl_sf_bessel_Knu_scaled_e" gsl_sf_bessel_Knu_scaled_e :: Double -> Double -> Ptr () -> IO CInt

bessel_Knu_scaled :: Double -> Double -> Double
bessel_Knu_scaled = gsl_sf_bessel_Knu_scaled
foreign import ccall "gsl_sf_bessel_Knu_scaled" gsl_sf_bessel_Knu_scaled :: Double -> Double -> Double

bessel_Knu_e :: Double -> Double -> (Double,Double)
bessel_Knu_e nu x = createSFR "bessel_Knu_e" $ gsl_sf_bessel_Knu_e nu x
foreign import ccall "gsl_sf_bessel_Knu_e" gsl_sf_bessel_Knu_e :: Double -> Double -> Ptr () -> IO CInt

bessel_Knu :: Double -> Double -> Double
bessel_Knu = gsl_sf_bessel_Knu
foreign import ccall "gsl_sf_bessel_Knu" gsl_sf_bessel_Knu :: Double -> Double -> Double

bessel_lnKnu_e :: Double -> Double -> (Double,Double)
bessel_lnKnu_e nu x = createSFR "bessel_lnKnu_e" $ gsl_sf_bessel_lnKnu_e nu x
foreign import ccall "gsl_sf_bessel_lnKnu_e" gsl_sf_bessel_lnKnu_e :: Double -> Double -> Ptr () -> IO CInt

bessel_lnKnu :: Double -> Double -> Double
bessel_lnKnu = gsl_sf_bessel_lnKnu
foreign import ccall "gsl_sf_bessel_lnKnu" gsl_sf_bessel_lnKnu :: Double -> Double -> Double

bessel_zero_J0_e :: CInt -> (Double,Double)
bessel_zero_J0_e s = createSFR "bessel_zero_J0_e" $ gsl_sf_bessel_zero_J0_e s
foreign import ccall "gsl_sf_bessel_zero_J0_e" gsl_sf_bessel_zero_J0_e :: CInt -> Ptr () -> IO CInt

bessel_zero_J0 :: CInt -> Double
bessel_zero_J0 = gsl_sf_bessel_zero_J0
foreign import ccall "gsl_sf_bessel_zero_J0" gsl_sf_bessel_zero_J0 :: CInt -> Double

bessel_zero_J1_e :: CInt -> (Double,Double)
bessel_zero_J1_e s = createSFR "bessel_zero_J1_e" $ gsl_sf_bessel_zero_J1_e s
foreign import ccall "gsl_sf_bessel_zero_J1_e" gsl_sf_bessel_zero_J1_e :: CInt -> Ptr () -> IO CInt

bessel_zero_J1 :: CInt -> Double
bessel_zero_J1 = gsl_sf_bessel_zero_J1
foreign import ccall "gsl_sf_bessel_zero_J1" gsl_sf_bessel_zero_J1 :: CInt -> Double

bessel_zero_Jnu_e :: Double -> CInt -> (Double,Double)
bessel_zero_Jnu_e nu s = createSFR "bessel_zero_Jnu_e" $ gsl_sf_bessel_zero_Jnu_e nu s
foreign import ccall "gsl_sf_bessel_zero_Jnu_e" gsl_sf_bessel_zero_Jnu_e :: Double -> CInt -> Ptr () -> IO CInt

bessel_zero_Jnu :: Double -> CInt -> Double
bessel_zero_Jnu = gsl_sf_bessel_zero_Jnu
foreign import ccall "gsl_sf_bessel_zero_Jnu" gsl_sf_bessel_zero_Jnu :: Double -> CInt -> Double
