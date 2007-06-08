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
    --module LAPACK.Internal
    svdR, svdR', svdC, svdC',
    eigC,
    linearSolveLSR
) where

import LAPACK.Internal
