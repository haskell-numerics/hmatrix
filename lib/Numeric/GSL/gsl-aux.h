#include <gsl/gsl_complex.h>

#define RVEC(A) int A##n, double*A##p
#define RMAT(A) int A##r, int A##c, double* A##p
#define KRVEC(A) int A##n, const double*A##p
#define KRMAT(A) int A##r, int A##c, const double* A##p

#define CVEC(A) int A##n, gsl_complex*A##p
#define CMAT(A) int A##r, int A##c, gsl_complex* A##p
#define KCVEC(A) int A##n, const gsl_complex*A##p
#define KCMAT(A) int A##r, int A##c, const gsl_complex* A##p

void no_abort_on_error();

int toScalarR(int code, KRVEC(x), RVEC(r));
/* norm2, absdif, maximum, posmax, etc. */

int mapR(int code, KRVEC(x), RVEC(r));
int mapC(int code, KCVEC(x), CVEC(r));
/* sin cos tan etc. */

int mapValR(int code, double*, KRVEC(x), RVEC(r));
int mapValC(int code, gsl_complex*, KCVEC(x), CVEC(r));

int zipR(int code, KRVEC(a), KRVEC(b), RVEC(r));
int zipC(int code, KCVEC(a), KCVEC(b), CVEC(r));


int fft(int code, KCVEC(a), CVEC(b));

int deriv(int code, double f(double, void*), double x, double h, double * result, double * abserr);

int integrate_qng(double f(double, void*), double a, double b, double prec,
                   double *result, double*error);

int integrate_qags(double f(double,void*), double a, double b, double prec, int w,
               double *result, double* error);

int polySolve(KRVEC(a), CVEC(z));

