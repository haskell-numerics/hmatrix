module Numeric.Sundials.ODEOpts where

import           Data.Int (Int32)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable as FS
import           Foreign.ForeignPtr as FF
import           Foreign.C.Types
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix)

import qualified Numeric.Sundials.CLangToHaskellTypes as T
import qualified Numeric.Sundials.Arkode as B

type Jacobian = Double -> Vector Double -> Matrix Double

data ODEOpts = ODEOpts {
    maxNumSteps :: Int32
  , minStep     :: Double
  , relTol      :: Double
  , absTols     :: VS.Vector Double
  , initStep    :: Double
  } deriving (Read, Show, Eq, Ord)

-- FIXME: Potentially an instance of Storable
_getMatrixDataFromContents :: Ptr T.SunMatrix -> IO T.SunMatrix
_getMatrixDataFromContents ptr = do
  qtr <- B.getContentMatrixPtr ptr
  rs  <- B.getNRows qtr
  cs  <- B.getNCols qtr
  rtr <- B.getMatrixData qtr
  vs  <- vectorFromC (fromIntegral $ rs * cs) rtr
  return $ T.SunMatrix { T.rows = rs, T.cols = cs, T.vals = vs }

putMatrixDataFromContents :: T.SunMatrix -> Ptr T.SunMatrix -> IO ()
putMatrixDataFromContents mat ptr = do
  let rs = T.rows mat
      cs = T.cols mat
      vs = T.vals mat
  qtr <- B.getContentMatrixPtr ptr
  B.putNRows rs qtr
  B.putNCols cs qtr
  rtr <- B.getMatrixData qtr
  vectorToC vs (fromIntegral $ rs * cs) rtr
-- FIXME: END

vectorFromC :: Storable a => Int -> Ptr a -> IO (VS.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  VS.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => VS.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  VS.copy (VM.unsafeFromForeignPtr0 ptr' len) vec

getDataFromContents :: Int -> Ptr T.SunVector -> IO (VS.Vector CDouble)
getDataFromContents len ptr = do
  qtr <- B.getContentPtr ptr
  rtr <- B.getData qtr
  vectorFromC len rtr

putDataInContents :: Storable a => VS.Vector a -> Int -> Ptr b -> IO ()
putDataInContents vec len ptr = do
  qtr <- B.getContentPtr ptr
  rtr <- B.getData qtr
  vectorToC vec len rtr

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

