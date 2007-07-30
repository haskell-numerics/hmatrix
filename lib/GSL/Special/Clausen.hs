------------------------------------------------------------
{- |
Module      :  GSL.Special.Clausen
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style
Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected functions described at:

<http://www.gnu.org/software/gsl/manual/html_node/Clausen-Functions.html>

-}
------------------------------------------------------------

module GSL.Special.Clausen(
  clausen_e
, clausen
) where

import Foreign(Ptr)
import GSL.Special.Internal

-- | wrapper for int gsl_sf_clausen_e(double x,gsl_sf_result* result);
clausen_e :: Double -> (Double,Double)
clausen_e x = createSFR "clausen_e" $ gsl_sf_clausen_e x
foreign import ccall "clausen.h gsl_sf_clausen_e" gsl_sf_clausen_e :: Double -> Ptr Double -> IO(Int)

-- | wrapper for double gsl_sf_clausen(double x);
clausen :: Double -> Double
clausen = gsl_sf_clausen
foreign import ccall "clausen.h gsl_sf_clausen" gsl_sf_clausen :: Double -> Double
