#include <gsl/gsl_complex.h>

#define RVEC(A) int A##n, double*A##p
#define RMAT(A) int A##r, int A##c, double* A##p
#define KRVEC(A) int A##n, const double*A##p
#define KRMAT(A) int A##r, int A##c, const double* A##p

#define CVEC(A) int A##n, gsl_complex*A##p
#define CMAT(A) int A##r, int A##c, gsl_complex* A##p
#define KCVEC(A) int A##n, const gsl_complex*A##p
#define KCMAT(A) int A##r, int A##c, const gsl_complex* A##p

int toScalarR(int code, KRVEC(x), RVEC(r));
/* norm2, absdif, maximum, posmax, etc. */

int mapR(int code, KRVEC(x), RVEC(r));
int mapC(int code, KCVEC(x), CVEC(r));
/* sin cos tan etc. */

int mapValR(int code, double*, KRVEC(x), RVEC(r));
int mapValC(int code, gsl_complex*, KCVEC(x), CVEC(r));

int zipR(int code, KRVEC(a), KRVEC(b), RVEC(r));
int zipC(int code, KCVEC(a), KCVEC(b), CVEC(r));


int luSolveR(KRMAT(a),KRMAT(b),RMAT(r)); 
int luSolveC(KCMAT(a),KCMAT(b),CMAT(r));
int luRaux(KRMAT(a),RVEC(b));
int luCaux(KCMAT(a),CVEC(b));

int svd(KRMAT(x),RMAT(u), RVEC(s),RMAT(v));

int eigensystemR(KRMAT(x),RVEC(l),RMAT(v));
int eigensystemC(KCMAT(x),RVEC(l),CMAT(v));

int QR(KRMAT(x),RMAT(q),RMAT(r));

int chol(KRMAT(x),RMAT(l));

int fft(int code, KCVEC(a), CVEC(b));

int integrate_qng(double f(double, void*), double a, double b, double prec,
                   double *result, double*error);

int integrate_qags(double f(double,void*), double a, double b, double prec, int w, 
               double *result, double* error);

int polySolve(KRVEC(a), CVEC(z));

int matrix_fscanf(char*filename, RMAT(a));

int minimize(double f(int, double*), double tolsize, int maxit, 
                 KRVEC(xi), KRVEC(sz), RMAT(sol));

int minimizeWithDeriv(double f(int, double*), void df(int, double*, double*),
                      double initstep, double minimpar, double tolgrad, int maxit, 
                      KRVEC(xi), RMAT(sol));

int deriv(int code, double f(double, void*), double x, double h, double * result, double * abserr);

double gsl_sf_erf(double);
double gsl_sf_erf_Z(double);

int gsl_sf_bessel_J0_e(double, double*); // hmmm...
int gsl_sf_exp_e10_e(double, double*);   // HMMMMM...
