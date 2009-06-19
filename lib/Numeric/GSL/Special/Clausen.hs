------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Special.Clausen
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- Wrappers for selected functions described at:
--
-- <http://www.google.com/search?q=gsl_sf_clausen.h&as_sitesearch=www.gnu.org/software/gsl/manual&btnI=Lucky>
------------------------------------------------------------

module Numeric.GSL.Special.Clausen(
  clausen_e
, clausen
) where

import Foreign(Ptr)
import Foreign.C.Types(CInt)
import Numeric.GSL.Special.Internal

clausen_e :: Double -> (Double,Double)
clausen_e x = createSFR "clausen_e" $ gsl_sf_clausen_e x
foreign import ccall "gsl_sf_clausen_e" gsl_sf_clausen_e :: Double -> Ptr () -> IO CInt

clausen :: Double -> Double
clausen = gsl_sf_clausen
foreign import ccall "gsl_sf_clausen" gsl_sf_clausen :: Double -> Double
