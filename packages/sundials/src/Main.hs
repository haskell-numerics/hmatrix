{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import           Data.Monoid ((<>))
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.Marshal.Array
import qualified Data.Vector.Storable as V

import           Data.Coerce (coerce)
import           Data.Monoid ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import           System.IO.Unsafe (unsafePerformIO)

import qualified Language.Haskell.TH as TH
import qualified Language.C.Types as CT
import qualified Data.Map as Map
import           Language.C.Inline.Context

C.context (C.baseCtx <> C.vecCtx <> C.funCtx)

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

-- | Solves a system of ODEs.  Every 'V.Vector' involved must be of the
-- same size.
-- {-# NOINLINE solveOdeC #-}
-- solveOdeC
--   :: (CDouble -> V.Vector CDouble -> V.Vector CDouble)
--   -- ^ ODE to Solve
--   -> CDouble
--   -- ^ Start
--   -> V.Vector CDouble
--   -- ^ Solution at start point
--   -> CDouble
--   -- ^ End
--   -> Either String (V.Vector CDouble)
--   -- ^ Solution at end point, or error.
-- solveOdeC fun x0 f0 xend = unsafePerformIO $ do
--   let dim = V.length f0
--   let dim_c = fromIntegral dim -- This is in CInt
--   -- Convert the function to something of the right type to C.
--   let funIO x y f _ptr = do
--         -- Convert the pointer we get from C (y) to a vector, and then
--         -- apply the user-supplied function.
--         fImm <- fun x <$> vectorFromC dim y
--         -- Fill in the provided pointer with the resulting vector.
--         vectorToC fImm dim f
--         -- Unsafe since the function will be called many times.
--         [CU.exp| int{ GSL_SUCCESS } |]
--   -- Create a mutable vector from the initial solution.  This will be
--   -- passed to the ODE solving function provided by GSL, and will
--   -- contain the final solution.
--   fMut <- V.thaw f0
--   res <- [C.block| int {
--       gsl_odeiv2_system sys = {
--         $fun:(int (* funIO) (double t, const double y[], double dydt[], void * params)),
--         // The ODE to solve, converted to function pointer using the `fun`
--         // anti-quoter
--         NULL,                   // We don't provide a Jacobian
--         $(int dim_c),           // The dimension
--         NULL                    // We don't need the parameter pointer
--       };
--       // Create the driver, using some sensible values for the stepping
--       // function and the tolerances
--       gsl_odeiv2_driver *d = gsl_odeiv2_driver_alloc_y_new (
--         &sys, gsl_odeiv2_step_rk8pd, 1e-6, 1e-6, 0.0);
--       // Finally, apply the driver.
--       int status = gsl_odeiv2_driver_apply(
--         d, &$(double x0), $(double xend), $vec-ptr:(double *fMut));
--       // Free the driver
--       gsl_odeiv2_driver_free(d);
--       return status;
--     } |]
--   -- Check the error code
--   maxSteps <- [C.exp| int{ GSL_EMAXITER } |]
--   smallStep <- [C.exp| int{ GSL_ENOPROG } |]
--   good <- [C.exp| int{ GSL_SUCCESS } |]
--   if | res == good -> Right <$> V.freeze fMut
--      | res == maxSteps -> return $ Left "Too many steps"
--      | res == smallStep -> return $ Left "Step size dropped below minimum allowed size"
--      | otherwise -> return $ Left $ "Unknown error code " ++ show res

-- -- Utils

-- vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
-- vectorFromC len ptr = do
--   ptr' <- newForeignPtr_ ptr
--   V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

-- vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
-- vectorToC vec len ptr = do
--   ptr' <- newForeignPtr_ ptr
--   V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec


-- /* Check function return value...
--     opt == 0 means SUNDIALS function allocates memory so check if
--              returned NULL pointer
--     opt == 1 means SUNDIALS function returns a flag so check if
--              flag >= 0
--     opt == 2 means function allocates memory so check if returned
--              NULL pointer  
-- */
-- static int check_flag(void *flagvalue, const char *funcname, int opt)
-- {
--   int *errflag;

--   /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
--   if (opt == 0 && flagvalue == NULL) {
--     fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n",
-- 	    funcname);
--     return 1; }

--   /* Check if flag < 0 */
--   else if (opt == 1) {
--     errflag = (int *) flagvalue;
--     if (*errflag < 0) {
--       fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n",
-- 	      funcname, *errflag);
--       return 1; }}

--   /* Check if function returned NULL pointer - no memory allocated */
--   else if (opt == 2 && flagvalue == NULL) {
--     fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
-- 	    funcname);
--     return 1; }

--   return 0;
-- }

main = do
  res <- [C.block| int { /* general problem variables */
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
                         flag = ARKodeInit(arkode_mem, NULL, f, T0, y);
                         if (check_flag(&flag, "ARKodeInit", 1)) return 1;

                         return 0;
                       } |]
  putStrLn $ show res
