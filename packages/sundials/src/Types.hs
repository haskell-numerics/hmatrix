{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

module Types where

import           Foreign.C.Types

import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as CT
import qualified Data.Map as Map
import           Language.C.Inline.Context


data SunVector

-- FIXME: Is this true?
type SunIndexType = CLong

sunTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
sunTypesTable = Map.fromList
  [
    (CT.TypeName "sunindextype", [t| SunIndexType |] )
  , (CT.TypeName "SunVector", [t| SunVector |] )
  ]

sunCtx :: Context
sunCtx = mempty {ctxTypesTable = sunTypesTable}

