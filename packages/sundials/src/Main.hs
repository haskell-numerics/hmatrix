{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import qualified Data.Vector.Storable as V

import           Data.Coerce (coerce)
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (Storable)
import           System.IO.Unsafe (unsafePerformIO)

import           Foreign.Storable (peekByteOff)

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
C.include "helpers.h"


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

stiffish :: Double -> V.Vector Double -> V.Vector Double
stiffish t v = V.fromList [ lamda * u + 1.0 / (1.0 + t * t) - lamda * atan t ]
  where
    u = v V.! 0
    lamda = -100.0

solveOdeC :: (CDouble -> V.Vector CDouble -> V.Vector CDouble) ->
             V.Vector Double ->
             CInt
solveOdeC fun f0 = unsafePerformIO $ do
  let dim = V.length f0
  -- We need the types that sundials expects. These are tied together
  -- in 'Types'. The Haskell type is currently empty!
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
                         realtype t, tout;
                         long int nst, nst_a, nfe, nfi, nsetups, nje, nfeLS, nni, ncfn, netf;

                         /* general problem parameters */
                         realtype T0 = RCONST(0.0);    /* initial time */
                         realtype Tf = RCONST(10.0);   /* final time */
                         realtype dTout = RCONST(1.0); /* time between outputs */
                         sunindextype NEQ = 1;         /* number of dependent vars. */
                         realtype reltol = 1.0e-6;     /* tolerances */
                         realtype abstol = 1.0e-10;
                         realtype lamda  = -100.0;     /* stiffness parameter */

                         /* Initial diagnostics output */
                         printf("\nAnalytical ODE test problem:\n");
                         printf("    lamda = %"GSYM"\n",    lamda);
                         printf("   reltol = %.1"ESYM"\n",  reltol);
                         printf("   abstol = %.1"ESYM"\n\n",abstol);

                         /* Initialize data structures */
                         y = N_VNew_Serial(NEQ);      /* Create serial vector for solution */
                         if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
                         N_VConst(0.0, y);            /* Specify initial condition */
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
                         flag = ARKodeSetUserData(arkode_mem, (void *) &lamda);  /* Pass lamda to user functions */
                         if (check_flag(&flag, "ARKodeSetUserData", 1)) return 1;
                         flag = ARKodeSStolerances(arkode_mem, reltol, abstol);  /* Specify tolerances */
                         if (check_flag(&flag, "ARKodeSStolerances", 1)) return 1;

                         /* Initialize dense matrix data structure and solver */
                         A = SUNDenseMatrix(NEQ, NEQ);
                         if (check_flag((void *)A, "SUNDenseMatrix", 0)) return 1;
                         LS = SUNDenseLinearSolver(y, A);
                         if (check_flag((void *)LS, "SUNDenseLinearSolver", 0)) return 1;

                         /* Linear solver interface */
                         flag = ARKDlsSetLinearSolver(arkode_mem, LS, A);        /* Attach matrix and linear solver */
                         /* Open output stream for results, output comment line */
                         UFID = fopen("solution.txt","w");
                         fprintf(UFID,"# t u\n");

                         /* output initial condition to disk */
                         fprintf(UFID," %.16"ESYM" %.16"ESYM"\n", T0, NV_Ith_S(y,0));  

                         /* Main time-stepping loop: calls ARKode to perform the integration, then
                            prints results.  Stops when the final time has been reached */
                         t = T0;
                         tout = T0+dTout;
                         printf("        t           u\n");
                         printf("   ---------------------\n");
                         while (Tf - t > 1.0e-15) {

                           flag = ARKode(arkode_mem, tout, y, &t, ARK_NORMAL);      /* call integrator */
                           if (check_flag(&flag, "ARKode", 1)) break;
                           printf("  %10.6"FSYM"  %10.6"FSYM"\n", t, NV_Ith_S(y,0));          /* access/print solution */
                           fprintf(UFID," %.16"ESYM" %.16"ESYM"\n", t, NV_Ith_S(y,0));  
                           if (flag >= 0) {                                         /* successful solve: update time */
                             tout += dTout;
                             tout = (tout > Tf) ? Tf : tout;
                           } else {                                                 /* unsuccessful solve: break */
                             fprintf(stderr,"Solver failure, stopping integration\n");
                             break;
                           }
                         }
                         printf("   ---------------------\n");
                         fclose(UFID);

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

                         /* check the solution error */
                         flag = check_ans(y, t, reltol, abstol);

                         /* Clean up and return */
                         N_VDestroy(y);            /* Free y vector */
                         ARKodeFree(&arkode_mem);  /* Free integrator memory */
                         SUNLinSolFree(LS);        /* Free linear solver */
                         SUNMatDestroy(A);         /* Free A matrix */
 
                         return flag;
                       } |]
  return res

main :: IO ()
main = do
  let res = solveOdeC (coerce stiffish) (V.fromList [1.0])
  putStrLn $ show res
