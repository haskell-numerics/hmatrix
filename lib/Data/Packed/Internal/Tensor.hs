{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Internal.Tensor
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Fundamental types
--
-----------------------------------------------------------------------------

module Data.Packed.Internal.Tensor where

import Data.Packed.Internal.Vector
import Data.Packed.Internal.Matrix


data IdxTp = Covariant | Contravariant deriving Show

data Tensor t = T { dims   :: [(Int,(IdxTp,String))]
                  , ten    :: Vector t
                  } deriving Show

rank = length . dims

outer u v = dat (multiply RowMajor r c)
    where r = matrixFromVector RowMajor 1 u
          c = matrixFromVector RowMajor (dim v) v
