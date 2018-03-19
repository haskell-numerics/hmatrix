{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Foreign.C.Types
import           Foreign.Ptr (Ptr)

import           Foreign.Storable (Storable(..))

import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as CT
import qualified Data.Map as Map
import           Language.C.Inline.Context

data BarType

instance Storable BarType where
    sizeOf _ = sizeOf (undefined :: BarType)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "peek not implemented for BarType"
    poke _ _ = error "poke not implemented for BarType"

-- This is a lie!!!
type SunIndexType = CLong

sunTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
sunTypesTable = Map.fromList
  [
    (CT.TypeName "sunindextype", [t| SunIndexType |] )
  , (CT.TypeName "BarType", [t| BarType |] )
  ]

sunCtx :: Context
sunCtx = mempty {ctxTypesTable = sunTypesTable}

