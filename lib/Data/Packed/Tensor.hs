-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Tensor
-- Copyright   :  (c) Alberto Ruiz 2007
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic tensor operations
--
-----------------------------------------------------------------------------

module Data.Packed.Tensor (
    -- * Construction
    Tensor, tensor, scalar,
    -- * Manipulation
    IdxName, IdxType(..), IdxDesc(..), structure, dims, coords, parts,
    tridx, withIdx, raise,
    -- * Operations
    addT, mulT,
    -- * Exterior Algebra
    wedge, dual, leviCivita, innerLevi, innerAT, niceAS,
    -- * Misc
    liftTensor, liftTensor2
) where


import Data.Packed.Internal.Tensor

