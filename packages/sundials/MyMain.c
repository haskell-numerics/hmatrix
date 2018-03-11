#include <stdio.h>
#include <math.h>
#include <arkode/arkode.h>                 /* prototypes for ARKODE fcts., consts. */
#include <nvector/nvector_serial.h>        /* serial N_Vector types, fcts., macros */
#include <sunmatrix/sunmatrix_dense.h>     /* access to dense SUNMatrix            */
#include <sunlinsol/sunlinsol_dense.h>     /* access to dense SUNLinearSolver      */
#include <arkode/arkode_direct.h>          /* access to ARKDls interface           */
#include <sundials/sundials_types.h>       /* definition of type realtype          */
#include <sundials/sundials_math.h>

#include "src/helpers.h"

int main () {
  sunindextype NEQ = 1;   /* number of dependent vars. */
  N_Vector y = NULL;      /* empty vector for storing solution */
  y = N_VNew_Serial(NEQ); /* Create serial vector for solution */
  if (check_flag((void *)y, "N_VNew_Serial", 0)) return 1;
  return 0;
}
