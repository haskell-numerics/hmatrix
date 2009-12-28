/*
 * We have copied the definitions in f2c.h required
 * to compile clapack.h, modified to support both
 * 32 and 64 bit

      http://opengrok.creo.hu/dragonfly/xref/src/contrib/gcc-3.4/libf2c/readme.netlib
      http://www.ibm.com/developerworks/library/l-port64.html
 */

#ifdef _LP64
typedef int integer;
typedef unsigned int uinteger;
typedef int logical;
typedef long longint;		/* system-dependent */
typedef unsigned long ulongint;	/* system-dependent */
#else
typedef long int integer;
typedef unsigned long int uinteger;
typedef long int logical;
typedef long long longint;		/* system-dependent */
typedef unsigned long long ulongint;	/* system-dependent */
#endif

typedef char *address;
typedef short int shortint;
typedef float real;
typedef double doublereal;
typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;
typedef short int shortlogical;
typedef char logical1;
typedef char integer1;

typedef logical (*L_fp)();
typedef short ftnlen;

/********************************************************/

#include "clapack.h"

/********************************************************/

#define DVEC(A) int A##n, double*A##p
#define CVEC(A) int A##n, double*A##p
#define DMAT(A) int A##r, int A##c, double* A##p
#define CMAT(A) int A##r, int A##c, double* A##p

#define KDVEC(A) int A##n, const double*A##p
#define KCVEC(A) int A##n, const double*A##p
#define KDMAT(A) int A##r, int A##c, const double* A##p
#define KCMAT(A) int A##r, int A##c, const double* A##p

/********************************************************/

int multiplyR(int ta, int tb, KDMAT(a),KDMAT(b),DMAT(r));
int multiplyC(int ta, int tb, KCMAT(a),KCMAT(b),CMAT(r));

int transR(KDMAT(x),DMAT(t));
int transC(KCMAT(x),CMAT(t));

int constantR(double * pval, DVEC(r));
int constantC(doublecomplex* pval, CVEC(r));

int svd_l_R(KDMAT(x),DMAT(u),DVEC(s),DMAT(v));
int svd_l_Rdd(KDMAT(x),DMAT(u),DVEC(s),DMAT(v));
int svd_l_C(KCMAT(a),CMAT(u),DVEC(s),CMAT(v));

int eig_l_C(KCMAT(a),CMAT(u),CVEC(s),CMAT(v));
int eig_l_R(KDMAT(a),DMAT(u),CVEC(s),DMAT(v));

int eig_l_S(int,KDMAT(a),DVEC(s),DMAT(v));
int eig_l_H(int,KCMAT(a),DVEC(s),CMAT(v));

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

int hess_l_R(KDMAT(a), DVEC(tau), DMAT(r));
int hess_l_C(KCMAT(a), CVEC(tau), CMAT(r));

int schur_l_R(KDMAT(a), DMAT(u), DMAT(s));
int schur_l_C(KCMAT(a), CMAT(u), CMAT(s));

int lu_l_R(KDMAT(a), DVEC(ipiv), DMAT(r));
int lu_l_C(KCMAT(a), DVEC(ipiv), CMAT(r));

int luS_l_R(KDMAT(a), KDVEC(ipiv), KDMAT(b), DMAT(x));
int luS_l_C(KCMAT(a), KDVEC(ipiv), KCMAT(b), CMAT(x));
