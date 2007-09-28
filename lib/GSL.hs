{- |

Module      :  GSL
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

This module reexports all the available GSL functions (except those in "LinearAlgebra").

-}

module GSL (
  module GSL.Integration
, module GSL.Differentiation
, module GSL.Fourier
, module GSL.Polynomials
, module GSL.Minimization
, module GSL.Special
, module Complex
, setErrorHandlerOff
) where

import GSL.Integration
import GSL.Differentiation
import GSL.Special
import GSL.Fourier
import GSL.Polynomials
import GSL.Minimization
import Complex
import GSL.Special


-- | This action removes the GSL default error handler (which aborts the program), so that
-- GSL errors can be handled by Haskell (using Control.Exception) and ghci doesn't abort.
foreign import ccall "GSL/gsl-aux.h no_abort_on_error" setErrorHandlerOff :: IO ()
