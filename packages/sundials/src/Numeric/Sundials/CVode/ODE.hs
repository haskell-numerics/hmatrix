{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Sundials.CVode.ODE
-- Copyright   :  Dominic Steinitz 2018,
--                Novadiscovery 2018
-- License     :  BSD
-- Maintainer  :  Dominic Steinitz
-- Stability   :  provisional
--
-- Solution of ordinary differential equation (ODE) initial value problems.
--
-- <https://computation.llnl.gov/projects/sundials/sundials-software>
--
-- A simple example:
--
-- <<diagrams/brusselator.png#diagram=brusselator&height=400&width=500>>
--
-- @
-- import           Numeric.Sundials.CVode.ODE
-- import           Numeric.LinearAlgebra
--
-- import           Plots as P
-- import qualified Diagrams.Prelude as D
-- import           Diagrams.Backend.Rasterific
--
-- brusselator :: Double -> [Double] -> [Double]
-- brusselator _t x = [ a - (w + 1) * u + v * u * u
--                    , w * u - v * u * u
--                    , (b - w) / eps - w * u
--                    ]
--   where
--     a = 1.0
--     b = 3.5
--     eps = 5.0e-6
--     u = x !! 0
--     v = x !! 1
--     w = x !! 2
--
-- lSaxis :: [[Double]] -> P.Axis B D.V2 Double
-- lSaxis xs = P.r2Axis &~ do
--   let ts = xs!!0
--       us = xs!!1
--       vs = xs!!2
--       ws = xs!!3
--   P.linePlot' $ zip ts us
--   P.linePlot' $ zip ts vs
--   P.linePlot' $ zip ts ws
--
-- main = do
--   let res1 = odeSolve brusselator [1.2, 3.1, 3.0] (fromList [0.0, 0.1 .. 10.0])
--   renderRasterific "diagrams/brusselator.png"
--                    (D.dims2D 500.0 500.0)
--                    (renderAxis $ lSaxis $ [0.0, 0.1 .. 10.0]:(toLists $ tr res1))
-- @
--
-- KVAERNO_4_2_3
--
-- \[
-- \begin{array}{c|cccc}
-- 0.0 & 0.0 & 0.0 & 0.0 & 0.0 \\
-- 0.871733043 & 0.4358665215 & 0.4358665215 & 0.0 & 0.0 \\
-- 1.0 & 0.490563388419108 & 7.3570090080892e-2 & 0.4358665215 & 0.0 \\
-- 1.0 & 0.308809969973036 & 1.490563388254106 & -1.235239879727145 & 0.4358665215 \\
-- \hline
--  & 0.308809969973036 & 1.490563388254106 & -1.235239879727145 & 0.4358665215 \\
--  & 0.490563388419108 & 7.3570090080892e-2 & 0.4358665215 & 0.0 \\
-- \end{array}
-- \]
--
-- SDIRK_2_1_2
--
-- \[
-- \begin{array}{c|cc}
-- 1.0 & 1.0 & 0.0 \\
-- 0.0 & -1.0 & 1.0 \\
-- \hline
--  & 0.5 & 0.5 \\
--  & 1.0 & 0.0 \\
-- \end{array}
-- \]
--
-- SDIRK_5_3_4
--
-- \[
-- \begin{array}{c|ccccc}
-- 0.25 & 0.25 & 0.0 & 0.0 & 0.0 & 0.0 \\
-- 0.75 & 0.5 & 0.25 & 0.0 & 0.0 & 0.0 \\
-- 0.55 & 0.34 & -4.0e-2 & 0.25 & 0.0 & 0.0 \\
-- 0.5 & 0.2727941176470588 & -5.036764705882353e-2 & 2.7573529411764705e-2 & 0.25 & 0.0 \\
-- 1.0 & 1.0416666666666667 & -1.0208333333333333 & 7.8125 & -7.083333333333333 & 0.25 \\
-- \hline
--  & 1.0416666666666667 & -1.0208333333333333 & 7.8125 & -7.083333333333333 & 0.25 \\
--  & 1.2291666666666667 & -0.17708333333333334 & 7.03125 & -7.083333333333333 & 0.0 \\
-- \end{array}
-- \]
-----------------------------------------------------------------------------
module Numeric.Sundials.CVode.ODE ( odeSolve
                                   , odeSolveV
                                   , odeSolveVWith
                                   , odeSolveVWith'
                                   , ODEMethod(..)
                                   , StepControl(..)
                                   , Jacobian
                                   , SundialsDiagnostics(..)
                                   ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           Data.Monoid ((<>))
import           Data.Maybe (isJust)

import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (Storable)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import           Data.Coerce (coerce)
import           System.IO.Unsafe (unsafePerformIO)

import           Numeric.LinearAlgebra.Devel (createVector)

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix, toList, (><),
                                                subMatrix, rows, cols, toLists,
                                                size, subVector)

import qualified Types as T
import           Arkode (cV_ADAMS, cV_BDF)
import qualified Arkode as B


C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> T.sunCtx)

C.include "<stdlib.h>"
C.include "<stdio.h>"
C.include "<math.h>"
C.include "<cvode/cvode.h>"               -- prototypes for CVODE fcts., consts.
C.include "<nvector/nvector_serial.h>"    -- serial N_Vector types, fcts., macros
C.include "<sunmatrix/sunmatrix_dense.h>" -- access to dense SUNMatrix
C.include "<sunlinsol/sunlinsol_dense.h>" -- access to dense SUNLinearSolver
C.include "<cvode/cvode_direct.h>"        -- access to CVDls interface
C.include "<sundials/sundials_types.h>"   -- definition of type realtype
C.include "<sundials/sundials_math.h>"
C.include "../../../helpers.h"
C.include "Arkode_hsc.h"


getDataFromContents :: Int -> Ptr T.SunVector -> IO (V.Vector CDouble)
getDataFromContents len ptr = do
  qtr <- B.getContentPtr ptr
  rtr <- B.getData qtr
  vectorFromC len rtr

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

putDataInContents :: Storable a => V.Vector a -> Int -> Ptr b -> IO ()
putDataInContents vec len ptr = do
  qtr <- B.getContentPtr ptr
  rtr <- B.getData qtr
  vectorToC vec len rtr

-- Utils

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec

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

type Jacobian = Double -> Vector Double -> Matrix Double

-- | Stepping functions
data ODEMethod = ADAMS
               | BDF

getMethod :: ODEMethod -> Int
getMethod (ADAMS) = cV_ADAMS
getMethod (BDF)   = cV_BDF

getJacobian :: ODEMethod -> Maybe Jacobian
getJacobian _ = Nothing

-- | A version of 'odeSolveVWith' with reasonable default step control.
odeSolveV
    :: ODEMethod
    -> Maybe Double      -- ^ initial step size - by default, ARKode
                         -- estimates the initial step size to be the
                         -- solution \(h\) of the equation
                         -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                         -- \(\ddot{y}\) is an estimated value of the
                         -- second derivative of the solution at \(t_0\)
    -> Double            -- ^ absolute tolerance for the state vector
    -> Double            -- ^ relative tolerance for the state vector
    -> (Double -> Vector Double -> Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
    -> Vector Double     -- ^ initial conditions
    -> Vector Double     -- ^ desired solution times
    -> Matrix Double     -- ^ solution
odeSolveV meth hi epsAbs epsRel f y0 ts =
  case odeSolveVWith meth (X epsAbs epsRel) hi g y0 ts of
    Left c -> error $ show c -- FIXME
    -- FIXME: Can we do better than using lists?
    Right (v, d) -> (nR >< nC) (V.toList v)
  where
    us = toList ts
    nR = length us
    nC = size y0
    g t x0 = coerce $ f t x0

-- | A version of 'odeSolveV' with reasonable default parameters and
-- system of equations defined using lists. FIXME: we should say
-- something about the fact we could use the Jacobian but don't for
-- compatibility with hmatrix-gsl.
odeSolve :: (Double -> [Double] -> [Double]) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
         -> [Double]                         -- ^ initial conditions
         -> Vector Double                    -- ^ desired solution times
         -> Matrix Double                    -- ^ solution
odeSolve f y0 ts =
  -- FIXME: These tolerances are different from the ones in GSL
  case odeSolveVWith BDF (XX' 1.0e-6 1.0e-10 1 1)  Nothing g (V.fromList y0) (V.fromList $ toList ts) of
    Left c -> error $ show c -- FIXME
    Right (v, d) -> (nR >< nC) (V.toList v)
  where
    us = toList ts
    nR = length us
    nC = length y0
    g t x0 = V.fromList $ f t (V.toList x0)

odeSolveVWith' ::
  ODEMethod
  -> StepControl
  -> Maybe Double -- ^ initial step size - by default, ARKode
                  -- estimates the initial step size to be the
                  -- solution \(h\) of the equation
                  -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                  -- \(\ddot{y}\) is an estimated value of the second
                  -- derivative of the solution at \(t_0\)
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Matrix Double                       -- ^ Error code or solution
odeSolveVWith' method control initStepSize f y0 tt =
  case odeSolveVWith method control initStepSize f y0 tt of
    Left c        -> error $ show c -- FIXME
    Right (v, _d) -> (nR >< nC) (V.toList v)
  where
    nR = V.length tt
    nC = V.length y0

odeSolveVWith ::
  ODEMethod
  -> StepControl
  -> Maybe Double -- ^ initial step size - by default, ARKode
                  -- estimates the initial step size to be the
                  -- solution \(h\) of the equation
                  -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                  -- \(\ddot{y}\) is an estimated value of the second
                  -- derivative of the solution at \(t_0\)
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Either Int ((V.Vector Double), SundialsDiagnostics) -- ^ Error code or solution
odeSolveVWith method control initStepSize f y0 tt =
  case solveOdeC (fromIntegral $ getMethod method) (coerce initStepSize) jacH (scise control)
                 (coerce f) (coerce y0) (coerce tt) of
    Left c -> Left $ fromIntegral c
    Right (v, d) -> Right (coerce v, d)
  where
    l = size y0
    scise (X absTol relTol)                          = coerce (V.replicate l absTol, relTol)
    scise (X' absTol relTol)                         = coerce (V.replicate l absTol, relTol)
    scise (XX' absTol relTol yScale _yDotScale)      = coerce (V.replicate l absTol, yScale * relTol)
    -- FIXME; Should we check that the length of ss is correct?
    scise (ScXX' absTol relTol yScale _yDotScale ss) = coerce (V.map (* absTol) ss, yScale * relTol)
    jacH = fmap (\g t v -> matrixToSunMatrix $ g (coerce t) (coerce v)) $
           getJacobian method
    matrixToSunMatrix m = T.SunMatrix { T.rows = nr, T.cols = nc, T.vals = vs }
      where
        nr = fromIntegral $ rows m
        nc = fromIntegral $ cols m
        -- FIXME: efficiency
        vs = V.fromList $ map coerce $ concat $ toLists m

solveOdeC ::
  CInt ->
  Maybe CDouble ->
  (Maybe (CDouble -> V.Vector CDouble -> T.SunMatrix)) ->
  (V.Vector CDouble, CDouble) ->
  (CDouble -> V.Vector CDouble -> V.Vector CDouble) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector CDouble -- ^ Initial conditions
  -> V.Vector CDouble -- ^ Desired solution times
  -> Either CInt ((V.Vector CDouble), SundialsDiagnostics) -- ^ Error code or solution
solveOdeC method initStepSize jacH (absTols, relTol) fun f0 ts = unsafePerformIO $ do

  let isInitStepSize :: CInt
      isInitStepSize = fromIntegral $ fromEnum $ isJust initStepSize
      ss :: CDouble
      ss = case initStepSize of
             -- It would be better to put an error message here but
             -- inline-c seems to evaluate this even if it is never
             -- used :(
             Nothing -> 0.0
             Just x  -> x
  let dim = V.length f0
      nEq :: CLong
      nEq = fromIntegral dim
      nTs :: CInt
      nTs = fromIntegral $ V.length ts
  -- FIXME: fMut is not actually mutatated
  fMut <- V.thaw f0
  tMut <- V.thaw ts
  -- FIXME: I believe this gets taken from the ghc heap and so should
  -- be subject to garbage collection.
  -- quasiMatrixRes <- createVector ((fromIntegral dim) * (fromIntegral nTs))
  -- qMatMut <- V.thaw quasiMatrixRes
  diagnostics :: V.Vector CLong <- createVector 10 -- FIXME
  diagMut <- V.thaw diagnostics
  -- We need the types that sundials expects. These are tied together
  -- in 'Types'. FIXME: The Haskell type is currently empty!
  let funIO :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr () -> IO CInt
      funIO x y f _ptr = do
        -- Convert the pointer we get from C (y) to a vector, and then
        -- apply the user-supplied function.
        fImm <- fun x <$> getDataFromContents dim y
        -- Fill in the provided pointer with the resulting vector.
        putDataInContents fImm dim f
        -- FIXME: I don't understand what this comment means
        -- Unsafe since the function will be called many times.
        [CU.exp| int{ 0 } |]
  let isJac :: CInt
      isJac = fromIntegral $ fromEnum $ isJust jacH
      jacIO :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr T.SunMatrix ->
               Ptr () -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr T.SunVector ->
               IO CInt
      jacIO t y _fy jacS _ptr _tmp1 _tmp2 _tmp3 = do
        case jacH of
          Nothing   -> error "Numeric.Sundials.ARKode.ODE: Jacobian not defined"
          Just jacI -> do j <- jacI t <$> getDataFromContents dim y
                          putMatrixDataFromContents j jacS
                          -- FIXME: I don't understand what this comment means
                          -- Unsafe since the function will be called many times.
                          [CU.exp| int{ 0 } |]

  res <- [C.block| int {
                         /* general problem variables */

                         int flag;                  /* reusable error-checking flag                 */
                         int i, j;                  /* reusable loop indices                        */
                         N_Vector y = NULL;         /* empty vector for storing solution            */
                         void *cvode_mem = NULL;    /* empty CVODE memory structure                 */

                         /* general problem parameters */

                         realtype T0 = RCONST(($vec-ptr:(double *ts))[0]); /* initial time              */
                         sunindextype NEQ = $(sunindextype nEq);           /* number of dependent vars. */

                         /* Initialize data structures */

                         y = N_VNew_Serial(NEQ); /* Create serial vector for solution */
                         if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
                         /* Specify initial condition */
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(y,i) = ($vec-ptr:(double *f0))[i];
                         };

                         cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
                         if (check_flag((void *)cvode_mem, "CVodeCreate", 0)) return(1);

                         /* Call CVodeInit to initialize the integrator memory and specify the
                          * user's right hand side function in y'=f(t,y), the inital time T0, and
                          * the initial dependent variable vector y. */
                         flag = CVodeInit(cvode_mem,   $fun:(int (* funIO) (double t, SunVector y[], SunVector dydt[], void * params)), T0, y);
                         if (check_flag(&flag, "CVodeInit", 1)) return(1);

                         /* Clean up and return */

                         N_VDestroy(y);          /* Free y vector          */
                         CVodeFree(&cvode_mem);  /* Free integrator memory */

                         return flag;
                       } |]
  if res == 0
    then do
      return $ Left res
    else do
      return $ Left res

-- | Adaptive step-size control
-- functions.
--
-- [GSL](https://www.gnu.org/software/gsl/doc/html/ode-initval.html#adaptive-step-size-control)
-- allows the user to control the step size adjustment using
-- \(D_i = \epsilon^{abs}s_i + \epsilon^{rel}(a_{y} |y_i| + a_{dy/dt} h |\dot{y}_i|)\) where
-- \(\epsilon^{abs}\) is the required absolute error, \(\epsilon^{rel}\)
-- is the required relative error, \(s_i\) is a vector of scaling
-- factors, \(a_{y}\) is a scaling factor for the solution \(y\) and
-- \(a_{dydt}\) is a scaling factor for the derivative of the solution \(dy/dt\).
--
-- [ARKode](https://computation.llnl.gov/projects/sundials/arkode)
-- allows the user to control the step size adjustment using
-- \(\eta^{rel}|y_i| + \eta^{abs}_i\). For compatibility with
-- [hmatrix-gsl](https://hackage.haskell.org/package/hmatrix-gsl),
-- tolerances for \(y\) and \(\dot{y}\) can be specified but the latter have no
-- effect.
data StepControl = X     Double Double -- ^ absolute and relative tolerance for \(y\); in GSL terms, \(a_{y} = 1\) and \(a_{dy/dt} = 0\); in ARKode terms, the \(\eta^{abs}_i\) are identical
                 | X'    Double Double -- ^ absolute and relative tolerance for \(\dot{y}\); in GSL terms, \(a_{y} = 0\) and \(a_{dy/dt} = 1\); in ARKode terms, the latter is treated as the relative tolerance for \(y\) so this is the same as specifying 'X' which may be entirely incorrect for the given problem
                 | XX'   Double Double Double Double -- ^ include both via relative tolerance
                                                     -- scaling factors \(a_y\), \(a_{{dy}/{dt}}\); in ARKode terms, the latter is ignored and \(\eta^{rel} = a_{y}\epsilon^{rel}\)
                 | ScXX' Double Double Double Double (Vector Double) -- ^ scale absolute tolerance of \(y_i\); in ARKode terms, \(a_{{dy}/{dt}}\) is ignored, \(\eta^{abs}_i = s_i \epsilon^{abs}\) and \(\eta^{rel} = a_{y}\epsilon^{rel}\)
