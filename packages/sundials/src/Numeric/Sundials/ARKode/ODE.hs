{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Sundials.Arkode.ODE ( solveOde ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           Data.Monoid ((<>))

import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (Storable, peekByteOff)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import           Data.Coerce (coerce)
import           System.IO.Unsafe (unsafePerformIO)

import           Numeric.LinearAlgebra.Devel (createVector)

import           Numeric.LinearAlgebra.HMatrix (Vector, Matrix)

import qualified Types as T

C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> T.sunCtx)

-- C includes
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


-- These were semi-generated using hsc2hs with Bar.hsc as the
-- template. They are probably very fragile and could easily break on
-- different architectures and / or changes in the sundials package.

getContentPtr :: Storable a => Ptr b -> IO a
getContentPtr ptr = ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr

getData :: Storable a => Ptr b -> IO a
getData ptr = ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr

getDataFromContents :: Storable b => Int -> Ptr a -> IO (V.Vector b)
getDataFromContents len ptr = do
  qtr <- getContentPtr ptr
  rtr <- getData qtr
  vectorFromC len rtr

putDataInContents :: Storable a => V.Vector a -> Int -> Ptr b -> IO ()
putDataInContents vec len ptr = do
  qtr <- getContentPtr ptr
  rtr <- getData qtr
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

odeSolve :: (Double -> [Double] -> [Double]) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
         -> [Double]                         -- ^ initial conditions
         -> Vector Double                    -- ^ desired solution times
         -> Matrix Double                    -- ^ solution
odeSolve = undefined

solveOde :: 
  (Double -> V.Vector Double -> V.Vector Double) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
          -> V.Vector Double                     -- ^ Initial conditions
          -> V.Vector Double                     -- ^ Desired solution times
          -> Either Int (V.Vector Double)        -- ^ Error code or solution
solveOde f y0 tt = case solveOdeC (coerce f) (coerce y0) (coerce tt) of
                     Left c -> Left $ fromIntegral c
                     Right v -> Right $ coerce v
                        
solveOdeC ::
  (CDouble -> V.Vector CDouble -> V.Vector CDouble) -- ^ The RHS of the system \(\dot{y} = f(t,y)\)
          -> V.Vector CDouble                       -- ^ Initial conditions
          -> V.Vector CDouble                       -- ^ Desired solution times
          -> Either CInt (V.Vector CDouble)         -- ^ Error code or solution
solveOdeC fun f0 ts = unsafePerformIO $ do
  let dim = V.length f0
      nEq :: CLong
      nEq = fromIntegral dim
      nTs :: CInt
      nTs = fromIntegral $ V.length ts
  fMut <- V.thaw f0
  tMut <- V.thaw ts
  -- FIXME: I believe this gets taken from the ghc heap and so should
  -- be subject to garbage collection.
  quasiMatrixRes <- createVector ((fromIntegral dim) * (fromIntegral nTs))
  qMatMut <- V.thaw quasiMatrixRes
  -- We need the types that sundials expects. These are tied together
  -- in 'Types'. FIXME: The Haskell type is currently empty!
  let funIO :: CDouble -> Ptr T.BarType -> Ptr T.BarType -> Ptr () -> IO CInt
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
                         SUNMatrix A = NULL;             /* empty matrix for linear solver */
                         SUNLinearSolver LS = NULL;      /* empty linear solver object */
                         void *arkode_mem = NULL;        /* empty ARKode memory structure */
                         FILE *UFID;
                         realtype t;
                         long int nst, nst_a, nfe, nfi, nsetups, nje, nfeLS, nni, ncfn, netf;

                         /* general problem parameters */
                         realtype T0 = RCONST(($vec-ptr:(double *tMut))[0]);    /* initial time */
                         realtype Tf = RCONST(($vec-ptr:(double *tMut))[$(int nTs) - 1]);   /* final time */
                         sunindextype NEQ = $(sunindextype nEq); /* number of dependent vars. */
                         realtype reltol = 1.0e-6;     /* tolerances */
                         realtype abstol = 1.0e-10;

                         /* Initial diagnostics output */
                         printf("\nAnalytical ODE test problem:\n");
                         printf("   reltol = %.1"ESYM"\n",  reltol);
                         printf("   abstol = %.1"ESYM"\n\n",abstol);

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
                         flag = ARKodeInit(arkode_mem, NULL, $fun:(int (* funIO) (double t, BarType y[], BarType dydt[], void * params)), T0, y);
                         if (check_flag(&flag, "ARKodeInit", 1)) return 1;

                         /* Set routines */
                         flag = ARKodeSStolerances(arkode_mem, reltol, abstol);  /* Specify tolerances */
                         if (check_flag(&flag, "ARKodeSStolerances", 1)) return 1;

                         /* Initialize dense matrix data structure and solver */
                         A = SUNDenseMatrix(NEQ, NEQ);
                         if (check_flag((void *)A, "SUNDenseMatrix", 0)) return 1;
                         LS = SUNDenseLinearSolver(y, A);
                         if (check_flag((void *)LS, "SUNDenseLinearSolver", 0)) return 1;

                         /* Linear solver interface */
                         flag = ARKDlsSetLinearSolver(arkode_mem, LS, A);        /* Attach matrix and linear solver */
                         /* Output initial conditions */
                         for (j = 0; j < NEQ; j++) {
                           ($vec-ptr:(double *qMatMut))[0 * $(int nTs) + j] = NV_Ith_S(y,j);
                         }
                         
                         /* Main time-stepping loop: calls ARKode to perform the integration, then
                            prints results.  Stops when the final time has been reached */
                         printf("        t           u\n");
                         printf("   ---------------------\n");
                         for (i = 1; i < $(int nTs); i++) {

                           flag = ARKode(arkode_mem, ($vec-ptr:(double *tMut))[i], y, &t, ARK_NORMAL);      /* call integrator */
                           if (check_flag(&flag, "ARKode", 1)) break;
                           printf("  %10.6"FSYM"  %10.6"FSYM"\n", t, NV_Ith_S(y,0));       /* access/print solution */
                           for (j = 0; j < NEQ; j++) {
                             ($vec-ptr:(double *qMatMut))[i * NEQ + j] = NV_Ith_S(y,j);
                           }

                           if (flag < 0) {                                                 /* unsuccessful solve: break */
                             fprintf(stderr,"Solver failure, stopping integration\n");
                             break;
                           }
                         }
                         printf("   ---------------------\n");

                         for (i = 0; i < NEQ; i++) {
                           ($vec-ptr:(double *fMut))[i] = NV_Ith_S(y,i);
                         };

                         /* Get/print some final statistics on how the solve progressed */
                         flag = ARKodeGetNumSteps(arkode_mem, &nst);
                         check_flag(&flag, "ARKodeGetNumSteps", 1);
                         flag = ARKodeGetNumStepAttempts(arkode_mem, &nst_a);
                         check_flag(&flag, "ARKodeGetNumStepAttempts", 1);
                         flag = ARKodeGetNumRhsEvals(arkode_mem, &nfe, &nfi);
                         check_flag(&flag, "ARKodeGetNumRhsEvals", 1);
                         flag = ARKodeGetNumLinSolvSetups(arkode_mem, &nsetups);
                         check_flag(&flag, "ARKodeGetNumLinSolvSetups", 1);
                         flag = ARKodeGetNumErrTestFails(arkode_mem, &netf);
                         check_flag(&flag, "ARKodeGetNumErrTestFails", 1);
                         flag = ARKodeGetNumNonlinSolvIters(arkode_mem, &nni);
                         check_flag(&flag, "ARKodeGetNumNonlinSolvIters", 1);
                         flag = ARKodeGetNumNonlinSolvConvFails(arkode_mem, &ncfn);
                         check_flag(&flag, "ARKodeGetNumNonlinSolvConvFails", 1);
                         flag = ARKDlsGetNumJacEvals(arkode_mem, &nje);
                         check_flag(&flag, "ARKDlsGetNumJacEvals", 1);
                         flag = ARKDlsGetNumRhsEvals(arkode_mem, &nfeLS);
                         check_flag(&flag, "ARKDlsGetNumRhsEvals", 1);
                       
                         printf("\nFinal Solver Statistics:\n");
                         printf("   Internal solver steps = %li (attempted = %li)\n", nst, nst_a);
                         printf("   Total RHS evals:  Fe = %li,  Fi = %li\n", nfe, nfi);
                         printf("   Total linear solver setups = %li\n", nsetups);
                         printf("   Total RHS evals for setting up the linear system = %li\n", nfeLS);
                         printf("   Total number of Jacobian evaluations = %li\n", nje);
                         printf("   Total number of Newton iterations = %li\n", nni);
                         printf("   Total number of linear solver convergence failures = %li\n", ncfn);
                         printf("   Total number of error test failures = %li\n\n", netf);

                         /* Clean up and return */
                         N_VDestroy(y);            /* Free y vector */
                         ARKodeFree(&arkode_mem);  /* Free integrator memory */
                         SUNLinSolFree(LS);        /* Free linear solver */
                         SUNMatDestroy(A);         /* Free A matrix */
 
                         return flag;
                       } |]
  if res == 0
    then do
      m <- V.freeze qMatMut
      return $ Right m
    else do
      return $ Left res
