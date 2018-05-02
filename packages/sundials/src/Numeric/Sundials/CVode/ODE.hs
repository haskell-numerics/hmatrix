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
-----------------------------------------------------------------------------
module Numeric.Sundials.CVode.ODE ( odeSolve
                                   , odeSolveV
                                   , odeSolveVWith
                                   , odeSolveVWith'
                                   , ODEMethod(..)
                                   , StepControl(..)
                                   ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           Data.Monoid ((<>))
import           Data.Maybe (isJust)

import           Foreign.C.Types (CDouble, CInt, CLong)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (poke)

import qualified Data.Vector.Storable as V

import           Data.Coerce (coerce)
import           System.IO.Unsafe (unsafePerformIO)

import           Numeric.LinearAlgebra.Devel (createVector)

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix, toList, rows,
                                                cols, toLists, size, reshape)

import           Numeric.Sundials.Arkode (cV_ADAMS, cV_BDF,
                                          getDataFromContents, putDataInContents)
import qualified Numeric.Sundials.Arkode as T
import           Numeric.Sundials.ODEOpts (ODEOpts(..), Jacobian, SundialsDiagnostics(..))


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
C.include "Numeric/Sundials/Arkode_hsc.h"


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
    -> Maybe Double      -- ^ initial step size - by default, CVode
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
  odeSolveVWith meth (X epsAbs epsRel) hi g y0 ts
  where
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
  odeSolveVWith BDF (XX' 1.0e-6 1.0e-10 1 1)  Nothing g (V.fromList y0) (V.fromList $ toList ts)
  where
    g t x0 = V.fromList $ f t (V.toList x0)

odeSolveVWith ::
  ODEMethod
  -> StepControl
  -> Maybe Double -- ^ initial step size - by default, CVode
                  -- estimates the initial step size to be the
                  -- solution \(h\) of the equation
                  -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                  -- \(\ddot{y}\) is an estimated value of the second
                  -- derivative of the solution at \(t_0\)
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Matrix Double                       -- ^ Error code or solution
odeSolveVWith method control initStepSize f y0 tt =
  case odeSolveVWith' opts method control initStepSize f y0 tt of
    Left c        -> error $ show c -- FIXME
    Right (v, _d) -> v
  where
    opts = ODEOpts { maxNumSteps = 10000
                   , minStep     = 1.0e-12
                   , relTol      = error "relTol"
                   , absTols     = error "absTol"
                   , initStep    = error "initStep"
                   , maxFail     = 10
                   }

odeSolveVWith' ::
  ODEOpts
  -> ODEMethod
  -> StepControl
  -> Maybe Double -- ^ initial step size - by default, CVode
                  -- estimates the initial step size to be the
                  -- solution \(h\) of the equation
                  -- \(\|\frac{h^2\ddot{y}}{2}\| = 1\), where
                  -- \(\ddot{y}\) is an estimated value of the second
                  -- derivative of the solution at \(t_0\)
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Either Int (Matrix Double, SundialsDiagnostics) -- ^ Error code or solution
odeSolveVWith' opts method control initStepSize f y0 tt =
  case solveOdeC (fromIntegral $ maxFail opts)
                 (fromIntegral $ maxNumSteps opts) (coerce $ minStep opts)
                 (fromIntegral $ getMethod method) (coerce initStepSize) jacH (scise control)
                 (coerce f) (coerce y0) (coerce tt) of
    Left c -> Left $ fromIntegral c
    Right (v, d) -> Right (reshape l (coerce v), d)
  where
    l = size y0
    scise (X aTol rTol)                          = coerce (V.replicate l aTol, rTol)
    scise (X' aTol rTol)                         = coerce (V.replicate l aTol, rTol)
    scise (XX' aTol rTol yScale _yDotScale)      = coerce (V.replicate l aTol, yScale * rTol)
    -- FIXME; Should we check that the length of ss is correct?
    scise (ScXX' aTol rTol yScale _yDotScale ss) = coerce (V.map (* aTol) ss, yScale * rTol)
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
  CLong ->
  CDouble ->
  CInt ->
  Maybe CDouble ->
  (Maybe (CDouble -> V.Vector CDouble -> T.SunMatrix)) ->
  (V.Vector CDouble, CDouble) ->
  (CDouble -> V.Vector CDouble -> V.Vector CDouble) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector CDouble -- ^ Initial conditions
  -> V.Vector CDouble -- ^ Desired solution times
  -> Either CInt ((V.Vector CDouble), SundialsDiagnostics) -- ^ Error code or solution
solveOdeC maxErrTestFails maxNumSteps_ minStep_ method initStepSize
          jacH (aTols, rTol) fun f0 ts =
  unsafePerformIO $ do

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
  quasiMatrixRes <- createVector ((fromIntegral dim) * (fromIntegral nTs))
  qMatMut <- V.thaw quasiMatrixRes
  diagnostics :: V.Vector CLong <- createVector 10 -- FIXME
  diagMut <- V.thaw diagnostics
  -- We need the types that sundials expects. These are tied together
  -- in 'CLangToHaskellTypes'. FIXME: The Haskell type is currently empty!
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
          Nothing   -> error "Numeric.Sundials.CVode.ODE: Jacobian not defined"
          Just jacI -> do j <- jacI t <$> getDataFromContents dim y
                          poke jacS j
                          -- FIXME: I don't understand what this comment means
                          -- Unsafe since the function will be called many times.
                          [CU.exp| int{ 0 } |]

  res <- [C.block| int {
                         /* general problem variables */

                         int flag;                  /* reusable error-checking flag                 */
                         int i, j;                  /* reusable loop indices                        */
                         N_Vector y = NULL;         /* empty vector for storing solution            */
                         N_Vector tv = NULL;        /* empty vector for storing absolute tolerances */

                         SUNMatrix A = NULL;        /* empty matrix for linear solver               */
                         SUNLinearSolver LS = NULL; /* empty linear solver object                   */
                         void *cvode_mem = NULL;    /* empty CVODE memory structure                 */
                         realtype t;
                         long int nst, nfe, nsetups, nje, nfeLS, nni, ncfn, netf, nge;

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

                         cvode_mem = CVodeCreate($(int method), CV_NEWTON);
                         if (check_flag((void *)cvode_mem, "CVodeCreate", 0)) return(1);

                         /* Call CVodeInit to initialize the integrator memory and specify the
                          * user's right hand side function in y'=f(t,y), the inital time T0, and
                          * the initial dependent variable vector y. */
                         flag = CVodeInit(cvode_mem,   $fun:(int (* funIO) (double t, SunVector y[], SunVector dydt[], void * params)), T0, y);
                         if (check_flag(&flag, "CVodeInit", 1)) return(1);

                         tv = N_VNew_Serial(NEQ); /* Create serial vector for absolute tolerances */
                         if (check_flag((void *)tv, "N_VNew_Serial", 0)) return 1;
                         /* Specify tolerances */
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(tv,i) = ($vec-ptr:(double *aTols))[i];
                         };

                         flag = CVodeSetMinStep(cvode_mem, $(double minStep_));
                         if (check_flag(&flag, "CVodeSetMinStep", 1)) return 1;
                         flag = CVodeSetMaxNumSteps(cvode_mem, $(long int maxNumSteps_));
                         if (check_flag(&flag, "CVodeSetMaxNumSteps", 1)) return 1;
                         flag = CVodeSetMaxErrTestFails(cvode_mem, $(int maxErrTestFails));
                         if (check_flag(&flag, "CVodeSetMaxErrTestFails", 1)) return 1;

                         /* Call CVodeSVtolerances to specify the scalar relative tolerance
                          * and vector absolute tolerances */
                         flag = CVodeSVtolerances(cvode_mem, $(double rTol), tv);
                         if (check_flag(&flag, "CVodeSVtolerances", 1)) return(1);

                         /* Initialize dense matrix data structure and solver */
                         A = SUNDenseMatrix(NEQ, NEQ);
                         if (check_flag((void *)A, "SUNDenseMatrix", 0)) return 1;
                         LS = SUNDenseLinearSolver(y, A);
                         if (check_flag((void *)LS, "SUNDenseLinearSolver", 0)) return 1;

                         /* Attach matrix and linear solver */
                         flag = CVDlsSetLinearSolver(cvode_mem, LS, A);
                         if (check_flag(&flag, "CVDlsSetLinearSolver", 1)) return 1;

                         /* Set the initial step size if there is one */
                         if ($(int isInitStepSize)) {
                           /* FIXME: We could check if the initial step size is 0 */
                           /* or even NaN and then throw an error                 */
                           flag = CVodeSetInitStep(cvode_mem, $(double ss));
                           if (check_flag(&flag, "CVodeSetInitStep", 1)) return 1;
                         }

                         /* Set the Jacobian if there is one */
                         if ($(int isJac)) {
                           flag = CVDlsSetJacFn(cvode_mem, $fun:(int (* jacIO) (double t, SunVector y[], SunVector fy[], SunMatrix Jac[], void * params, SunVector tmp1[], SunVector tmp2[], SunVector tmp3[])));
                           if (check_flag(&flag, "CVDlsSetJacFn", 1)) return 1;
                         }

                         /* Store initial conditions */
                         for (j = 0; j < NEQ; j++) {
                           ($vec-ptr:(double *qMatMut))[0 * $(int nTs) + j] = NV_Ith_S(y,j);
                         }

                         /* Main time-stepping loop: calls CVode to perform the integration */
                         /* Stops when the final time has been reached                       */
                         for (i = 1; i < $(int nTs); i++) {

                           flag = CVode(cvode_mem, ($vec-ptr:(double *ts))[i], y, &t, CV_NORMAL); /* call integrator */
                           if (check_flag(&flag, "CVode", 1)) break;

                           /* Store the results for Haskell */
                           for (j = 0; j < NEQ; j++) {
                             ($vec-ptr:(double *qMatMut))[i * NEQ + j] = NV_Ith_S(y,j);
                           }

                           /* unsuccessful solve: break */
                           if (flag < 0) {
                             fprintf(stderr,"Solver failure, stopping integration\n");
                             break;
                           }
                         }

                         /* Get some final statistics on how the solve progressed */

                         flag = CVodeGetNumSteps(cvode_mem, &nst);
                         check_flag(&flag, "CVodeGetNumSteps", 1);
                         ($vec-ptr:(long int *diagMut))[0] = nst;

                         /* FIXME */
                         ($vec-ptr:(long int *diagMut))[1] = 0;

                         flag = CVodeGetNumRhsEvals(cvode_mem, &nfe);
                         check_flag(&flag, "CVodeGetNumRhsEvals", 1);
                         ($vec-ptr:(long int *diagMut))[2] = nfe;
                         /* FIXME */
                         ($vec-ptr:(long int *diagMut))[3] = 0;

                         flag = CVodeGetNumLinSolvSetups(cvode_mem, &nsetups);
                         check_flag(&flag, "CVodeGetNumLinSolvSetups", 1);
                         ($vec-ptr:(long int *diagMut))[4] = nsetups;

                         flag = CVodeGetNumErrTestFails(cvode_mem, &netf);
                         check_flag(&flag, "CVodeGetNumErrTestFails", 1);
                         ($vec-ptr:(long int *diagMut))[5] = netf;

                         flag = CVodeGetNumNonlinSolvIters(cvode_mem, &nni);
                         check_flag(&flag, "CVodeGetNumNonlinSolvIters", 1);
                         ($vec-ptr:(long int *diagMut))[6] = nni;

                         flag = CVodeGetNumNonlinSolvConvFails(cvode_mem, &ncfn);
                         check_flag(&flag, "CVodeGetNumNonlinSolvConvFails", 1);
                         ($vec-ptr:(long int *diagMut))[7] = ncfn;

                         flag = CVDlsGetNumJacEvals(cvode_mem, &nje);
                         check_flag(&flag, "CVDlsGetNumJacEvals", 1);
                         ($vec-ptr:(long int *diagMut))[8] = ncfn;

                         flag = CVDlsGetNumRhsEvals(cvode_mem, &nfeLS);
                         check_flag(&flag, "CVDlsGetNumRhsEvals", 1);
                         ($vec-ptr:(long int *diagMut))[9] = ncfn;

                         /* Clean up and return */

                         N_VDestroy(y);          /* Free y vector          */
                         N_VDestroy(tv);         /* Free tv vector         */
                         CVodeFree(&cvode_mem);  /* Free integrator memory */
                         SUNLinSolFree(LS);      /* Free linear solver     */
                         SUNMatDestroy(A);       /* Free A matrix          */

                         return flag;
                       } |]
  if res == 0
    then do
      preD <- V.freeze diagMut
      let d = SundialsDiagnostics (fromIntegral $ preD V.!0)
                                  (fromIntegral $ preD V.!1)
                                  (fromIntegral $ preD V.!2)
                                  (fromIntegral $ preD V.!3)
                                  (fromIntegral $ preD V.!4)
                                  (fromIntegral $ preD V.!5)
                                  (fromIntegral $ preD V.!6)
                                  (fromIntegral $ preD V.!7)
                                  (fromIntegral $ preD V.!8)
                                  (fromIntegral $ preD V.!9)
      m <- V.freeze qMatMut
      return $ Right (m, d)
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
