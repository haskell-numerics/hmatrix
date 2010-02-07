{- |

Module      :  Numeric.GSL
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

This module reexports all the available GSL functions (except those in "Numeric.LinearAlgebra").

-}

module Numeric.GSL (
  module Numeric.GSL.Integration
, module Numeric.GSL.Differentiation
, module Numeric.GSL.Fourier
, module Numeric.GSL.Polynomials
, module Numeric.GSL.Minimization
, module Numeric.GSL.Root
, module Numeric.GSL.ODE
, module Numeric.GSL.Special
, module Complex
, setErrorHandlerOff
) where

import Numeric.GSL.Integration
import Numeric.GSL.Differentiation
import Numeric.GSL.Special
import Numeric.GSL.Fourier
import Numeric.GSL.Polynomials
import Numeric.GSL.Minimization
import Numeric.GSL.Root
import Numeric.GSL.ODE
import Complex


-- | This action removes the GSL default error handler (which aborts the program), so that
-- GSL errors can be handled by Haskell (using Control.Exception) and ghci doesn't abort.
foreign import ccall "GSL/gsl-aux.h no_abort_on_error" setErrorHandlerOff :: IO ()
