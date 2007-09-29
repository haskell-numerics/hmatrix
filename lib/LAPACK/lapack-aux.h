#include "f2c.h"
#include "clapack.h"

#define DVEC(A) int A##n, double*A##p
#define CVEC(A) int A##n, double*A##p
#define DMAT(A) int A##r, int A##c, double* A##p
#define CMAT(A) int A##r, int A##c, double* A##p

// const pointer versions for the parameters 
#define KDVEC(A) int A##n, const double*A##p
#define KCVEC(A) int A##n, const double*A##p
#define KDMAT(A) int A##r, int A##c, const double* A##p
#define KCMAT(A) int A##r, int A##c, const double* A##p 

int svd_l_R(KDMAT(x),DMAT(u),DVEC(s),DMAT(v));
int svd_l_Rdd(KDMAT(x),DMAT(u),DVEC(s),DMAT(v));

int svd_l_C(KCMAT(a),CMAT(u),DVEC(s),CMAT(v));

int eig_l_C(KCMAT(a),CMAT(u),CVEC(s),CMAT(v));

int eig_l_R(KDMAT(a),DMAT(u),CVEC(s),DMAT(v));

int eig_l_S(KDMAT(a),DVEC(s),DMAT(v));

int eig_l_H(KCMAT(a),DVEC(s),CMAT(v));

int linearSolveR_l(KDMAT(a),KDMAT(b),DMAT(x));

int linearSolveC_l(KCMAT(a),KCMAT(b),CMAT(x));

int linearSolveLSR_l(KDMAT(a),KDMAT(b),DMAT(x));

int linearSolveLSC_l(KCMAT(a),KCMAT(b),CMAT(x));

int linearSolveSVDR_l(double,KDMAT(a),KDMAT(b),DMAT(x));

int linearSolveSVDC_l(double,KCMAT(a),KCMAT(b),CMAT(x));

int chol_l_H(KCMAT(a),CMAT(r));

int chol_l_S(KDMAT(a),DMAT(r));

int qr_l_R(KDMAT(a), DVEC(tau), DMAT(r));

int qr_l_C(KCMAT(a), CVEC(tau), CMAT(r));
