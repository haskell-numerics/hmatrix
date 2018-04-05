{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Sundials.ARKode
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
-- @
-- import Numeric.Sundials.ARKode
-- import Numeric.LinearAlgebra
-- import Graphics.Plot(mplot)
--
-- xdot t [x,v] = [v, -0.95*x - 0.1*v]
--
-- ts = linspace 100 (0,20 :: Double)
--
-- sol = odeSolve xdot [10,0] ts
--
-- main = mplot (ts : toColumns sol)
-- @
--
-- KVAERNO_4_2_3
--
-- \[
-- \begin{array}{c|cccc}
-- c_1 & 0.0 & 0.0 & 0.0 & 0.0 \\
-- c_2 & 0.4358665215 & 0.4358665215 & 0.0 & 0.0 \\
-- c_3 & 0.490563388419108 & 7.3570090080892e-2 & 0.4358665215 & 0.0 \\
-- c_4 & 0.308809969973036 & 1.490563388254106 & -1.235239879727145 & 0.4358665215 \\
-- \end{array}
-- \]
--
-- SDIRK_2_1_2
--
-- \[
-- \begin{array}{c|cc}
-- c_1 & 1.0 & 0.0 \\
-- c_2 & -1.0 & 1.0 \\
-- \end{array}
-- \]
--
-- SDIRK_5_3_4
--
-- \[
-- \begin{array}{c|ccccc}
-- c_1 & 0.25 & 0.0 & 0.0 & 0.0 & 0.0 \\
-- c_2 & 0.5 & 0.25 & 0.0 & 0.0 & 0.0 \\
-- c_3 & 0.34 & -4.0e-2 & 0.25 & 0.0 & 0.0 \\
-- c_4 & 0.2727941176470588 & -5.036764705882353e-2 & 2.7573529411764705e-2 & 0.25 & 0.0 \\
-- c_5 & 1.0416666666666667 & -1.0208333333333333 & 7.8125 & -7.083333333333333 & 0.25 \\
-- \end{array}
-- \]
-----------------------------------------------------------------------------
module Numeric.Sundials.ARKode.ODE ( odeSolve
                                   , odeSolveV
                                   , odeSolveVWith
                                   , odeSolve'
                                   , getButcherTable
                                   , getBT
                                   , btGet
                                   , ODEMethod(..)
                                   , StepControl(..)
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
                                                subMatrix, rows, cols, toLists)

import qualified Types as T
import           Arkode (sDIRK_2_1_2, kVAERNO_4_2_3, sDIRK_5_3_4)
import qualified Arkode as B

import Debug.Trace


C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> T.sunCtx)

C.include "<stdlib.h>"
C.include "<stdio.h>"
C.include "<math.h>"
C.include "<arkode/arkode.h>"                 -- prototypes for ARKODE fcts., consts.
C.include "<nvector/nvector_serial.h>"        -- serial N_Vector types, fcts., macros
C.include "<sunmatrix/sunmatrix_dense.h>"     -- access to dense SUNMatrix
C.include "<sunlinsol/sunlinsol_dense.h>"     -- access to dense SUNLinearSolver
C.include "<arkode/arkode_direct.h>"          -- access to ARKDls interface
C.include "<sundials/sundials_types.h>"       -- definition of type realtype
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
    _aRKodeGetNumSteps               :: Int
  , _aRKodeGetNumStepAttempts        :: Int
  , _aRKodeGetNumRhsEvals_fe         :: Int
  , _aRKodeGetNumRhsEvals_fi         :: Int
  , _aRKodeGetNumLinSolvSetups       :: Int
  , _aRKodeGetNumErrTestFails        :: Int
  , _aRKodeGetNumNonlinSolvIters     :: Int
  , _aRKodeGetNumNonlinSolvConvFails :: Int
  , _aRKDlsGetNumJacEvals            :: Int
  , _aRKDlsGetNumRhsEvals            :: Int
  } deriving Show

type Jacobian = Double -> Vector Double -> Matrix Double

-- | Stepping functions
data ODEMethod = SDIRK_2_1_2 Jacobian
               | KVAERNO_4_2_3 Jacobian
               | SDIRK_5_3_4 Jacobian

getMethod :: ODEMethod -> Int
getMethod (SDIRK_2_1_2 _)   = sDIRK_2_1_2
getMethod (KVAERNO_4_2_3 _) = kVAERNO_4_2_3
getMethod (SDIRK_5_3_4 _)   = sDIRK_5_3_4

-- | A version of 'odeSolveVWith' with reasonable default step control.
odeSolveV
    :: ODEMethod
    -> Double            -- ^ initial step size
    -> Double            -- ^ absolute tolerance for the state vector
    -> Double            -- ^ relative tolerance for the state vector
    -> (Double -> Vector Double -> Vector Double)   -- ^ x'(t,x)
    -> Vector Double     -- ^ initial conditions
    -> Vector Double     -- ^ desired solution times
    -> Matrix Double     -- ^ solution
odeSolveV _meth _hi _epsAbs _epsRel = undefined

-- | A version of 'odeSolveV' with reasonable default parameters and
-- system of equations defined using lists. FIXME: we should say
-- something about the fact we could use the Jacobian but don't for
-- compatibility with hmatrix-gsl.
odeSolve :: (Double -> [Double] -> [Double]) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
         -> [Double]                         -- ^ initial conditions
         -> Vector Double                    -- ^ desired solution times
         -> Matrix Double                    -- ^ solution
odeSolve f y0 ts =
  case odeSolveVWith (SDIRK_5_3_4 undefined) Nothing 1.0e-6 1.0e-10 g (V.fromList y0) (V.fromList $ toList ts) of
    Left c -> error $ show c -- FIXME
    Right (v, d) -> trace (show d) $ (nR >< nC) (V.toList v)
  where
    us = toList ts
    nR = length us
    nC = length y0
    g t x0 = V.fromList $ f t (V.toList x0)

odeSolve' :: ODEMethod
         -> (Double -> Vector Double -> Matrix Double)
         -> (Double -> [Double] -> [Double]) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
         -> [Double]                         -- ^ initial conditions
         -> Vector Double                    -- ^ desired solution times
         -> Matrix Double                    -- ^ solution
odeSolve' method jac f y0 ts =
  case odeSolveVWith method (pure jac') 1.0e-6 1.0e-10 g (V.fromList y0) (V.fromList $ toList ts) of
    Left c -> error $ show c -- FIXME
    Right (v, d) -> trace (show d) $ (nR >< nC) (V.toList v)
  where
    us = toList ts
    nR = length us
    nC = length y0
    g t x0 = V.fromList $ f t (V.toList x0)
    jac' t v = foo $ jac t (V.fromList $ toList v)
    foo m = T.SunMatrix { T.rows = nr, T.cols = nc, T.vals = vs }
      where
        nr = fromIntegral $ rows m
        nc = fromIntegral $ cols m
        vs = V.fromList $ map coerce $ concat $ toLists m

odeSolveVWith ::
  ODEMethod
  -> (Maybe (Double -> V.Vector Double -> T.SunMatrix))
  -> Double
  -> Double
  -> (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector Double                     -- ^ Initial conditions
  -> V.Vector Double                     -- ^ Desired solution times
  -> Either Int ((V.Vector Double), SundialsDiagnostics) -- ^ Error code or solution
odeSolveVWith method jac relTol absTol f y0 tt =
  case solveOdeC (fromIntegral $ getMethod method) jacH (CDouble relTol) (CDouble absTol)
                 (coerce f) (coerce y0) (coerce tt) of
    Left c -> Left $ fromIntegral c
    Right (v, d) -> Right (coerce v, d)
  where
    jacH :: Maybe (CDouble -> V.Vector CDouble -> T.SunMatrix)
    jacH = fmap (\g -> (\t v -> g (coerce t) (coerce v))) jac

solveOdeC ::
  CInt ->
  (Maybe (CDouble -> V.Vector CDouble -> T.SunMatrix)) ->
  CDouble ->
  CDouble ->
  (CDouble -> V.Vector CDouble -> V.Vector CDouble) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
  -> V.Vector CDouble -- ^ Initial conditions
  -> V.Vector CDouble -- ^ Desired solution times
  -> Either CInt ((V.Vector CDouble), SundialsDiagnostics) -- ^ Error code or solution
solveOdeC method jacH relTol absTol fun f0 ts = unsafePerformIO $ do
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
  quasiMatrixRes <- createVector ((fromIntegral dim) * (fromIntegral nTs))
  qMatMut <- V.thaw quasiMatrixRes
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
                         int flag;                       /* reusable error-checking flag */
                         N_Vector y = NULL;              /* empty vector for storing solution */
                         SUNMatrix A = NULL;             /* empty matrix for linear solver */
                         SUNLinearSolver LS = NULL;      /* empty linear solver object */
                         void *arkode_mem = NULL;        /* empty ARKode memory structure */
                         realtype t;
                         long int nst, nst_a, nfe, nfi, nsetups, nje, nfeLS, nni, ncfn, netf;

                         /* general problem parameters */
                         realtype T0 = RCONST(($vec-ptr:(double *tMut))[0]);    /* initial time */
                         sunindextype NEQ = $(sunindextype nEq); /* number of dependent vars. */

                         /* Initialize data structures */
                         y = N_VNew_Serial(NEQ);      /* Create serial vector for solution */
                         if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
                         int i, j;
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(y,i) = ($vec-ptr:(double *fMut))[i];
                         }; /* Specify initial condition */
                         arkode_mem = ARKodeCreate(); /* Create the solver memory */
                         if (check_flag((void *)arkode_mem, "ARKodeCreate", 0)) return 1;

                         /* Call ARKodeInit to initialize the integrator memory and specify the */
                         /*    right-hand side function in y'=f(t,y), the inital time T0, and */
                         /*    the initial dependent variable vector y.  Note: since this */
                         /*    problem is fully implicit, we set f_E to NULL and f_I to f. */

                         /* Here we use the C types defined in helpers.h which tie up with */
                         /* the Haskell types defined in Types                             */
                         flag = ARKodeInit(arkode_mem, NULL, $fun:(int (* funIO) (double t, SunVector y[], SunVector dydt[], void * params)), T0, y);
                         if (check_flag(&flag, "ARKodeInit", 1)) return 1;

                         /* Set routines */
                         flag = ARKodeSStolerances(arkode_mem, $(double relTol), $(double absTol));
                         if (check_flag(&flag, "ARKodeSStolerances", 1)) return 1;

                         /* Initialize dense matrix data structure and solver */
                         A = SUNDenseMatrix(NEQ, NEQ);
                         if (check_flag((void *)A, "SUNDenseMatrix", 0)) return 1;
                         LS = SUNDenseLinearSolver(y, A);
                         if (check_flag((void *)LS, "SUNDenseLinearSolver", 0)) return 1;

                         /* Linear solver interface */
                         flag = ARKDlsSetLinearSolver(arkode_mem, LS, A);        /* Attach matrix and linear solver */

                         if ($(int isJac)) {
                           flag = ARKDlsSetJacFn(arkode_mem, $fun:(int (* jacIO) (double t, SunVector y[], SunVector fy[], SunMatrix Jac[], void * params, SunVector tmp1[], SunVector tmp2[], SunVector tmp3[])));
                           if (check_flag(&flag, "ARKDlsSetJacFn", 1)) return 1;
                         }

                         /* Store initial conditions */
                         for (j = 0; j < NEQ; j++) {
                           ($vec-ptr:(double *qMatMut))[0 * $(int nTs) + j] = NV_Ith_S(y,j);
                         }

                         flag = ARKodeSetIRKTableNum(arkode_mem, $(int method));
                         if (check_flag(&flag, "ARKode", 1)) return 1;

                         /* Main time-stepping loop: calls ARKode to perform the integration */
                         /* Stops when the final time has been reached */
                         for (i = 1; i < $(int nTs); i++) {

                           flag = ARKode(arkode_mem, ($vec-ptr:(double *tMut))[i], y, &t, ARK_NORMAL); /* call integrator */
                           if (check_flag(&flag, "ARKode", 1)) break;

                           /* Store the results for Haskell */
                           for (j = 0; j < NEQ; j++) {
                             ($vec-ptr:(double *qMatMut))[i * NEQ + j] = NV_Ith_S(y,j);
                           }

                           if (flag < 0) {                                                 /* unsuccessful solve: break */
                             fprintf(stderr,"Solver failure, stopping integration\n");
                             break;
                           }
                         }

                         /* Get some final statistics on how the solve progressed */
                         flag = ARKodeGetNumSteps(arkode_mem, &nst);
                         check_flag(&flag, "ARKodeGetNumSteps", 1);
                         ($vec-ptr:(long int *diagMut))[0] = nst;

                         flag = ARKodeGetNumStepAttempts(arkode_mem, &nst_a);
                         check_flag(&flag, "ARKodeGetNumStepAttempts", 1);
                         ($vec-ptr:(long int *diagMut))[1] = nst_a;

                         flag = ARKodeGetNumRhsEvals(arkode_mem, &nfe, &nfi);
                         check_flag(&flag, "ARKodeGetNumRhsEvals", 1);
                         ($vec-ptr:(long int *diagMut))[2] = nfe;
                         ($vec-ptr:(long int *diagMut))[3] = nfi;

                         flag = ARKodeGetNumLinSolvSetups(arkode_mem, &nsetups);
                         check_flag(&flag, "ARKodeGetNumLinSolvSetups", 1);
                         ($vec-ptr:(long int *diagMut))[4] = nsetups;

                         flag = ARKodeGetNumErrTestFails(arkode_mem, &netf);
                         check_flag(&flag, "ARKodeGetNumErrTestFails", 1);
                         ($vec-ptr:(long int *diagMut))[5] = netf;

                         flag = ARKodeGetNumNonlinSolvIters(arkode_mem, &nni);
                         check_flag(&flag, "ARKodeGetNumNonlinSolvIters", 1);
                         ($vec-ptr:(long int *diagMut))[6] = nni;

                         flag = ARKodeGetNumNonlinSolvConvFails(arkode_mem, &ncfn);
                         check_flag(&flag, "ARKodeGetNumNonlinSolvConvFails", 1);
                         ($vec-ptr:(long int *diagMut))[7] = ncfn;

                         flag = ARKDlsGetNumJacEvals(arkode_mem, &nje);
                         check_flag(&flag, "ARKDlsGetNumJacEvals", 1);
                         ($vec-ptr:(long int *diagMut))[8] = ncfn;

                         flag = ARKDlsGetNumRhsEvals(arkode_mem, &nfeLS);
                         check_flag(&flag, "ARKDlsGetNumRhsEvals", 1);
                         ($vec-ptr:(long int *diagMut))[9] = ncfn;

                         /* Clean up and return */
                         N_VDestroy(y);            /* Free y vector */
                         ARKodeFree(&arkode_mem);  /* Free integrator memory */
                         SUNLinSolFree(LS);        /* Free linear solver */
                         SUNMatDestroy(A);         /* Free A matrix */

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

btGet :: ODEMethod -> Matrix Double
btGet method = case getBT method of
                 Left c -> error $ show c -- FIXME
                 Right (v, sqp) -> subMatrix (0, 0) (s, s) $
                                    (B.arkSMax >< B.arkSMax) (V.toList v)
                   where
                     s = fromIntegral $ sqp V.! 0

getBT :: ODEMethod -> Either Int (V.Vector Double, V.Vector Int)
getBT method = case getButcherTable method of
                 Left c -> Left $ fromIntegral c
                 Right (v, sqp) -> Right $ (coerce v, V.map fromIntegral sqp)

getButcherTable :: ODEMethod -> Either CInt ((V.Vector CDouble), V.Vector CInt)
getButcherTable method = unsafePerformIO $ do
  -- arkode seems to want an ODE in order to set and then get the
  -- Butcher tableau so here's one to keep it happy
  let fun :: CDouble -> V.Vector CDouble -> V.Vector CDouble
      fun _t ys = V.fromList [ ys V.! 0 ]
      f0        = V.fromList [ 1.0 ]
      ts        = V.fromList [ 0.0 ]
      dim = V.length f0
      nEq :: CLong
      nEq = fromIntegral dim
      mN :: CInt
      mN = fromIntegral $ getMethod method

  -- FIXME: I believe these gets taken from the ghc heap and so should
  -- be subject to garbage collection.
  btSQP :: V.Vector CInt <- createVector 3
  btSQPMut <- V.thaw btSQP
  btAs :: V.Vector CDouble <- createVector (B.arkSMax * B.arkSMax)
  btAsMut <- V.thaw btAs
  -- We need the types that sundials expects. These are tied together
  -- in 'Types'. FIXME: The Haskell type is currently empty!
  let funIO :: CDouble -> Ptr T.SunVector -> Ptr T.SunVector -> Ptr () -> IO CInt
      funIO x y f _ptr = do
        -- Convert the pointer we get from C (y) to a vector, and then
        -- apply the user-supplied function.
        fImm <- fun x <$> getDataFromContents dim y
        -- Fill in the provided pointer with the resulting vector.
        putDataInContents fImm dim f
        -- I don't understand what this comment means
        -- Unsafe since the function will be called many times.
        [CU.exp| int{ 0 } |]
  res <- [C.block| int {
                         /* general problem variables */
                         int flag;                       /* reusable error-checking flag */
                         N_Vector y = NULL;              /* empty vector for storing solution */
                         void *arkode_mem = NULL;        /* empty ARKode memory structure */

                         /* general problem parameters */
                         /* initial time */
                         realtype T0 = RCONST(($vec-ptr:(double *ts))[0]);
                         /* number of dependent vars. */
                         sunindextype NEQ = $(sunindextype nEq);

                         /* Initialize data structures */
                         y = N_VNew_Serial(NEQ);         /* Create serial vector for solution */
                         if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
                         /* Specify initial condition */
                         int i, j;
                         for (i = 0; i < NEQ; i++) {
                           NV_Ith_S(y,i) = ($vec-ptr:(double *f0))[i];
                         };
                         arkode_mem = ARKodeCreate(); /* Create the solver memory */
                         if (check_flag((void *)arkode_mem, "ARKodeCreate", 0)) return 1;

                         flag = ARKodeInit(arkode_mem, NULL, $fun:(int (* funIO) (double t, SunVector y[], SunVector dydt[], void * params)), T0, y);
                         if (check_flag(&flag, "ARKodeInit", 1)) return 1;

                         flag = ARKodeSetIRKTableNum(arkode_mem, $(int mN));
                         if (check_flag(&flag, "ARKode", 1)) return 1;

                         int s, q, p;
                         realtype *ai = (realtype *)malloc(ARK_S_MAX * ARK_S_MAX * sizeof(realtype));
                         realtype *ae = (realtype *)malloc(ARK_S_MAX * ARK_S_MAX * sizeof(realtype));
                         realtype *ci = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *ce = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *bi = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *be = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *b2i = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         realtype *b2e = (realtype *)malloc(ARK_S_MAX * sizeof(realtype));
                         flag = ARKodeGetCurrentButcherTables(arkode_mem, &s, &q, &p, ai, ae, ci, ce, bi, be, b2i, b2e);
                         if (check_flag(&flag, "ARKode", 1)) return 1;
                         $vec-ptr:(int *btSQPMut)[0] = s;
                         $vec-ptr:(int *btSQPMut)[1] = q;
                         $vec-ptr:(int *btSQPMut)[2] = p;
                         for (i = 0; i < s; i++) {
                           for (j = 0; j < s; j++) {
                             /* FIXME: double should be realtype */
                             ($vec-ptr:(double *btAsMut))[i * ARK_S_MAX + j] = ai[i * ARK_S_MAX + j];
                           }
                         }

                         /* Clean up and return */
                         N_VDestroy(y);            /* Free y vector */
                         ARKodeFree(&arkode_mem);  /* Free integrator memory */

                         return flag;
                       } |]
  if res == 0
    then do
      x <- V.freeze btAsMut
      y <- V.freeze btSQPMut
      return $ Right (x, y)
    else do
      return $ Left res

-- | Adaptive step-size control functions. FIXME: It may not be
-- possible to scale the tolerances for the derivatives in sundials so
-- for now we ignore them and emit a warning.
data StepControl = X     Double Double -- ^ abs. and rel. tolerance for x(t)
                 | X'    Double Double -- ^ abs. and rel. tolerance for x'(t)
                 | XX'   Double Double Double Double -- ^ include both via rel. tolerance scaling factors a_x, a_x'
                 | ScXX' Double Double Double Double (Vector Double) -- ^ scale abs. tolerance of x(t) components
