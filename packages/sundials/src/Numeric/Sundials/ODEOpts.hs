module Numeric.Sundials.ODEOpts where

import           Data.Int (Int32)
import qualified Data.Vector.Storable as VS

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix)


type Jacobian = Double -> Vector Double -> Matrix Double

data ODEOpts = ODEOpts {
    maxNumSteps :: Int32
  , minStep     :: Double
  , relTol      :: Double
  , absTols     :: VS.Vector Double
  , initStep    :: Double
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

