-----------------------------------------------------------------------------
{- |
Module      :  GSL.Special
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Wrappers for selected special functions.

<http://www.gnu.org/software/gsl/manual/html_node/Special-Functions.html#Special-Functions>
-}
-----------------------------------------------------------------------------

module GSL.Special (
  module GSL.Special.Airy
, module GSL.Special.Bessel
, module GSL.Special.Clausen
, module GSL.Special.Coulomb
, module GSL.Special.Coupling
, module GSL.Special.Dawson
, module GSL.Special.Debye
, module GSL.Special.Dilog
, module GSL.Special.Elementary
, module GSL.Special.Ellint
, module GSL.Special.Erf
, module GSL.Special.Exp
, module GSL.Special.Expint
, module GSL.Special.Fermi_dirac
, module GSL.Special.Gamma
, module GSL.Special.Gegenbauer
, module GSL.Special.Hyperg
, module GSL.Special.Laguerre
, module GSL.Special.Lambert
, module GSL.Special.Legendre
, module GSL.Special.Log
, module GSL.Special.Pow_int
, module GSL.Special.Psi
, module GSL.Special.Synchrotron
, module GSL.Special.Trig
, module GSL.Special.Zeta
)
where

import Foreign
import GSL.Special.Internal
import GSL.Special.Airy
import GSL.Special.Bessel
import GSL.Special.Clausen
import GSL.Special.Coulomb
import GSL.Special.Coupling
import GSL.Special.Dawson
import GSL.Special.Debye
import GSL.Special.Dilog
import GSL.Special.Elementary
import GSL.Special.Ellint
import GSL.Special.Erf
import GSL.Special.Exp
import GSL.Special.Expint
import GSL.Special.Fermi_dirac
import GSL.Special.Gamma
import GSL.Special.Gegenbauer
import GSL.Special.Hyperg
import GSL.Special.Laguerre
import GSL.Special.Lambert
import GSL.Special.Legendre
import GSL.Special.Log
import GSL.Special.Pow_int
import GSL.Special.Psi
import GSL.Special.Synchrotron
import GSL.Special.Trig
import GSL.Special.Zeta
