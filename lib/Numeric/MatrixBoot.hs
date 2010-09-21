{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.MatrixBoot
-- Copyright   :  (c) Alberto Ruiz 2010
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable
--
-- Module to avoid cyclic dependency
--
-----------------------------------------------------------------------------

module Numeric.MatrixBoot (
                     ctrans, diag, ident,
                    ) where

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.Internal.Matrix
import Numeric.Container

-------------------------------------------------------------------

-- | conjugate transpose
ctrans :: (Container Vector e, Element e) => Matrix e -> Matrix e
ctrans = liftMatrix conj . trans

-- | Creates a square matrix with a given diagonal.
diag :: (Num a, Element a) => Vector a -> Matrix a
diag v = diagRect 0 v n n where n = dim v

-- | creates the identity matrix of given dimension
ident :: (Num a, Element a) => Int -> Matrix a
ident n = diag (constantD 1 n)

----------------------------------------------------


