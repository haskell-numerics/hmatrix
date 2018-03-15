#include <stdio.h>
#include <math.h>
#include <arkode/arkode.h>                 /* prototypes for ARKODE fcts., consts. */
#include <nvector/nvector_serial.h>        /* serial N_Vector types, fcts., macros */
#include <sunmatrix/sunmatrix_dense.h>     /* access to dense SUNMatrix            */
#include <sunlinsol/sunlinsol_dense.h>     /* access to dense SUNLinearSolver      */
#include <arkode/arkode_direct.h>          /* access to ARKDls interface           */
#include <sundials/sundials_types.h>       /* definition of type realtype          */
#include <sundials/sundials_math.h>

#include "farkode.h"

#include <HsFFI.h>
#include "Main_stub.h"


/* Check function return value...
    opt == 0 means SUNDIALS function allocates memory so check if
             returned NULL pointer
    opt == 1 means SUNDIALS function returns a flag so check if
             flag >= 0
    opt == 2 means function allocates memory so check if returned
             NULL pointer  
*/
int check_flag(void *flagvalue, const char *funcname, int opt)
{
  int *errflag;

  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && flagvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return 1; }

  /* Check if flag < 0 */
  else if (opt == 1) {
    errflag = (int *) flagvalue;
    if (*errflag < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n",
	      funcname, *errflag);
      return 1; }}

  /* Check if function returned NULL pointer - no memory allocated */
  else if (opt == 2 && flagvalue == NULL) {
    fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return 1; }

  return 0;
}

/* f routine to compute the ODE RHS function f(t,y). */
int f(realtype t, N_Vector y, N_Vector ydot, void *user_data)
{
  realtype *rdata = (realtype *) user_data;   /* cast user_data to realtype */
  realtype lamda = rdata[0];                  /* set shortcut for stiffness parameter */
  realtype u = NV_Ith_S(y,0);                 /* access current solution value */

  /* fill in the RHS function: "NV_Ith_S" accesses the 0th entry of ydot */
  NV_Ith_S(ydot,0) = lamda*u + 1.0/(1.0+t*t) - lamda*atan(t);

  return 0;                                   /* return with success */
}

int FARK_IMP_FUN(realtype *T, realtype *Y, realtype *YDOT,
		 long int *IPAR, realtype *RPAR, int *IER) {
  realtype t = *T;
  realtype u = Y[0];
  realtype lamda = -100.0;
  YDOT[0] = singleEq(t, u);
  return 0;
}

/* C interface to user-supplied FORTRAN function FARKIFUN; see
   farkode.h for further details */
int FARKfi(realtype t, N_Vector y, N_Vector ydot, void *user_data) {

  int ier;
  realtype *ydata, *dydata;
  FARKUserData ARK_userdata;
  ydata  = N_VGetArrayPointer(y);
  dydata = N_VGetArrayPointer(ydot);
  ARK_userdata = (FARKUserData) user_data;

  FARK_IMP_FUN(&t, ydata, dydata, ARK_userdata->ipar,
	       ARK_userdata->rpar, &ier);
  return(ier);
}

/* Jacobian routine to compute J(t,y) = df/dy. */
int Jac(realtype t, N_Vector y, N_Vector fy, SUNMatrix J,
        void *user_data, N_Vector tmp1, N_Vector tmp2, N_Vector tmp3)
{
  realtype *rdata = (realtype *) user_data;   /* cast user_data to realtype */
  realtype lamda = rdata[0];                  /* set shortcut for stiffness parameter */
  realtype *Jdata = SUNDenseMatrix_Data(J);
  
  /* Fill in Jacobian of f: set the first entry of the data array to set the (0,0) entry */
  Jdata[0] = lamda;

  return 0;                                   /* return with success */
}


/* check the computed solution */
int check_ans(N_Vector y, realtype t, realtype rtol, realtype atol)
{
  int      passfail=0;     /* answer pass (0) or fail (1) flag     */  
  realtype ans, err, ewt;  /* answer data, error, and error weight */
  realtype ONE=RCONST(1.0);

  /* compute solution error */
  ans  = atan(t);
  ewt = ONE / (rtol * SUNRabs(ans) + atol);
  err = ewt * SUNRabs(NV_Ith_S(y,0) - ans);

  /* is the solution within the tolerances? */
  passfail = (err < ONE) ? 0 : 1; 

  if (passfail) {
    fprintf(stdout, "\nSUNDIALS_WARNING: check_ans error=%g \n\n", err);
  }

  return(passfail);
}
