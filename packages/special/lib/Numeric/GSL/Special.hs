-----------------------------------------------------------------------------
{- |
Module      :  Numeric.GSL.Special
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected special functions.

<http://www.gnu.org/software/gsl/manual/html_node/Special-Functions.html#Special-Functions>
-}
-----------------------------------------------------------------------------

module Numeric.GSL.Special (
  -- * Functions
  module Numeric.GSL.Special.Airy
, module Numeric.GSL.Special.Bessel
, module Numeric.GSL.Special.Clausen
, module Numeric.GSL.Special.Coulomb
, module Numeric.GSL.Special.Coupling
, module Numeric.GSL.Special.Dawson
, module Numeric.GSL.Special.Debye
, module Numeric.GSL.Special.Dilog
, module Numeric.GSL.Special.Elementary
, module Numeric.GSL.Special.Ellint
, module Numeric.GSL.Special.Elljac
, module Numeric.GSL.Special.Erf
, module Numeric.GSL.Special.Exp
, module Numeric.GSL.Special.Expint
, module Numeric.GSL.Special.Fermi_dirac
, module Numeric.GSL.Special.Gamma
, module Numeric.GSL.Special.Gegenbauer
, module Numeric.GSL.Special.Hyperg
, module Numeric.GSL.Special.Laguerre
, module Numeric.GSL.Special.Lambert
, module Numeric.GSL.Special.Legendre
, module Numeric.GSL.Special.Log
, module Numeric.GSL.Special.Pow_int
, module Numeric.GSL.Special.Psi
, module Numeric.GSL.Special.Synchrotron
, module Numeric.GSL.Special.Transport
, module Numeric.GSL.Special.Trig
, module Numeric.GSL.Special.Zeta
-- * Util
, mkComplex_e
)
where


import Numeric.GSL.Special.Airy
import Numeric.GSL.Special.Bessel
import Numeric.GSL.Special.Clausen
import Numeric.GSL.Special.Coulomb
import Numeric.GSL.Special.Coupling
import Numeric.GSL.Special.Dawson
import Numeric.GSL.Special.Debye
import Numeric.GSL.Special.Dilog
import Numeric.GSL.Special.Elementary
import Numeric.GSL.Special.Ellint
import Numeric.GSL.Special.Elljac
import Numeric.GSL.Special.Erf
import Numeric.GSL.Special.Exp
import Numeric.GSL.Special.Expint
import Numeric.GSL.Special.Fermi_dirac
import Numeric.GSL.Special.Gamma
import Numeric.GSL.Special.Gegenbauer
import Numeric.GSL.Special.Hyperg
import Numeric.GSL.Special.Laguerre
import Numeric.GSL.Special.Lambert
import Numeric.GSL.Special.Legendre
import Numeric.GSL.Special.Log
import Numeric.GSL.Special.Pow_int
import Numeric.GSL.Special.Psi
import Numeric.GSL.Special.Synchrotron
import Numeric.GSL.Special.Transport
import Numeric.GSL.Special.Trig
import Numeric.GSL.Special.Zeta

import Data.Complex

----------------------------------------------------------------

{- | Some GSL complex functions work with separate real and imaginary parts stored in real variables, obtaining tuples (value, error) for the real and imaginary parts of the result:

> > import Numeric.GSL.Special.Dilog

> > complex_dilog_xy_e 1 1
> ((0.6168502750680847,1.1097853812294034e-14),(1.4603621167531193,1.1855504863267322e-14))

We can use @mkComplex_e@ to work with \"normal\" complex numbers:

> > import Numeric.GSL.Special(mkComplex_e)
> > import Data.Complex

> > let dilogC = fst . mkComplex_e complex_dilog_xy_e

> > dilogC (1 :+ 1)
> 0.6168502750680847 :+ 1.4603621167531193

-}
mkComplex_e :: (Double -> Double -> ((Double, Double), (Double, Double)))
            -> Complex Double -> (Complex Double, Complex Double)
mkComplex_e f (x :+ y) = (zr :+ zi, er :+ ei)
    where ((zr,er),(zi,ei)) = f x y

