#include <gsl/gsl_complex.h>

#define RVEC(A) int A##n, double*A##p
#define RMAT(A) int A##r, int A##c, double* A##p
#define KRVEC(A) int A##n, const double*A##p
#define KRMAT(A) int A##r, int A##c, const double* A##p

#define CVEC(A) int A##n, gsl_complex*A##p
#define CMAT(A) int A##r, int A##c, gsl_complex* A##p
#define KCVEC(A) int A##n, const gsl_complex*A##p
#define KCMAT(A) int A##r, int A##c, const gsl_complex* A##p

int submatrixR(int r1, int r2, int c1, int c2, KRMAT(x),RMAT(r));

const char * gsl_strerror (const int gsl_errno);

int matrix_fscanf(char*filename, RMAT(a));

int conjugate(KCVEC(x),CVEC(t));

void asm_finit();
