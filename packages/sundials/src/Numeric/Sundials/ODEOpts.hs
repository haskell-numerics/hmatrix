module Numeric.Sundials.ODEOpts where

import           Data.Word (Word32)
import qualified Data.Vector.Storable as VS

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix)


type Jacobian = Double -> Vector Double -> Matrix Double

data ODEOpts = ODEOpts {
    maxNumSteps :: Word32
  , minStep     :: Double
  , relTol      :: Double
  , absTols     :: VS.Vector Double
  , initStep    :: Maybe Double
  , maxFail     :: Word32
  } deriving (Read, Show, Eq, Ord)

data SundialsDiagnostics = SundialsDiagnostics {
    aRKodeGetNumSteps               :: Int
  , aRKodeGetNumStepAttempts        :: Int
  , aRKodeGetNumRhsEvals_fe         :: Int
  , aRKodeGetNumRhsEvals_fi         :: Int
  , aRKodeGetNumLinSolvSetups       :: Int
  , aRKodeGetNumErrTestFails        :: Int
  , aRKodeGetNumNonlinSolvIters     :: Int
  , aRKodeGetNumNonlinSolvConvFails :: Int
  , aRKDlsGetNumJacEvals            :: Int
  , aRKDlsGetNumRhsEvals            :: Int
  } deriving Show

