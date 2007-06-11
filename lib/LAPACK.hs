-----------------------------------------------------------------------------
-- |
-- Module      :  LAPACK
-- Copyright   :  (c) Alberto Ruiz 2006-7
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Wrappers for a few LAPACK functions (<http://www.netlib.org/lapack>).
--
-----------------------------------------------------------------------------

module LAPACK (
    svdR, svdR', svdC, svdC',
    eigC, eigR, eigS, eigH,
    linearSolveR, linearSolveC,
    linearSolveLSR, linearSolveLSC,
    linearSolveSVDR, linearSolveSVDC,
) where

import LAPACK.Internal
