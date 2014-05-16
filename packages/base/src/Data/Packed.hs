-----------------------------------------------------------------------------
{- |
Module      :  Data.Packed
Copyright   :  (c) Alberto Ruiz 2006-2014
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Types for dense 'Vector' and 'Matrix' of 'Storable' elements.

-}
-----------------------------------------------------------------------------
{-# OPTIONS_HADDOCK hide #-}

module Data.Packed (
    -- * Vector
    --
    -- | Vectors are @Data.Vector.Storable.Vector@ from the \"vector\" package.
    module Data.Packed.Vector,
    -- * Matrix
    module Data.Packed.Matrix,
) where

import Data.Packed.Vector
import Data.Packed.Matrix

