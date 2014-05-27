
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mkl_dss.h"
#include "mkl_types.h"
#include "mkl_spblas.h"

#define KIVEC(A) int A##n, const int*A##p
#define KDVEC(A) int A##n, const double*A##p
#define DVEC(A) int A##n, double*A##p
#define OK return 0;


void check_error(int error)
{
	if(error != MKL_DSS_SUCCESS) {
		printf ("Solver returned error code %d\n", error);
		exit (1);
	}
}

int dss(KDVEC(vals),KIVEC(cols),KIVEC(rows),KDVEC(x),DVEC(r)) {
    MKL_INT nRows = rowsn-1, nCols = rn, nNonZeros = valsn, nRhs = 1;
    MKL_INT *rowIndex = (MKL_INT*) rowsp;
    MKL_INT *columns = (MKL_INT*) colsp;
    double *values = (double*) valsp;
    _DOUBLE_PRECISION_t *rhs = (_DOUBLE_PRECISION_t*) xp;
//  _DOUBLE_PRECISION_t *obtrhs = (_DOUBLE_PRECISION_t*) malloc((nCols)*sizeof(_DOUBLE_PRECISION_t));
    _DOUBLE_PRECISION_t *solValues = (_DOUBLE_PRECISION_t*) rp;

    _MKL_DSS_HANDLE_t handle;
    _INTEGER_t error;
//	_CHARACTER_t *uplo;
    MKL_INT opt;

    opt = MKL_DSS_DEFAULTS;
    error = dss_create(handle, opt);
    check_error(error);

    opt = MKL_DSS_NON_SYMMETRIC;
    error = dss_define_structure(handle, opt, rowIndex, nRows, nCols, columns, nNonZeros);
    check_error(error);

    opt = MKL_DSS_DEFAULTS;
    error = dss_reorder(handle, opt, 0);
    check_error(error);

    opt = MKL_DSS_INDEFINITE;
    error = dss_factor_real(handle, opt, values);
    check_error(error);

    int j;
    for (j = 0; j < nCols; j++) {
	    solValues[j] = 0.0;
    }

    // Solve system
    opt = MKL_DSS_REFINEMENT_ON;
    error = dss_solve_real(handle, opt, rhs, nRhs, solValues);
    check_error(error);

    OK
}
