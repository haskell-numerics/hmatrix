#include <gsl/gsl_complex.h>

#define RVEC(A) int A##n, double*A##p
#define RMAT(A) int A##r, int A##c, double* A##p
#define KRVEC(A) int A##n, const double*A##p
#define KRMAT(A) int A##r, int A##c, const double* A##p

#define CVEC(A) int A##n, gsl_complex*A##p
#define CMAT(A) int A##r, int A##c, gsl_complex* A##p
#define KCVEC(A) int A##n, const gsl_complex*A##p
#define KCMAT(A) int A##r, int A##c, const gsl_complex* A##p

#define FVEC(A) int A##n, float*A##p
#define FMAT(A) int A##r, int A##c, float* A##p
#define KFVEC(A) int A##n, const float*A##p
#define KFMAT(A) int A##r, int A##c, const float* A##p

#define QVEC(A) int A##n, gsl_complex_float*A##p
#define QMAT(A) int A##r, int A##c, gsl_complex_float* A##p
#define KQVEC(A) int A##n, const gsl_complex_float*A##p
#define KQMAT(A) int A##r, int A##c, const gsl_complex_float* A##p

#include <gsl/gsl_blas.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_fft_complex.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_deriv.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit_nlin.h>
#include <string.h>
#include <stdio.h>

#define MACRO(B) do {B} while (0)
#define ERROR(CODE) MACRO(return CODE;)
#define REQUIRES(COND, CODE) MACRO(if(!(COND)) {ERROR(CODE);})
#define OK return 0;

#define MIN(A,B) ((A)<(B)?(A):(B))
#define MAX(A,B) ((A)>(B)?(A):(B))

#ifdef DBG
#define DEBUGMSG(M) printf("*** calling aux C function: %s\n",M);
#else
#define DEBUGMSG(M)
#endif

#define CHECK(RES,CODE) MACRO(if(RES) return CODE;)

#ifdef DBG
#define DEBUGMAT(MSG,X) printf(MSG" = \n"); gsl_matrix_fprintf(stdout,X,"%f"); printf("\n");
#else
#define DEBUGMAT(MSG,X)
#endif

#ifdef DBG
#define DEBUGVEC(MSG,X) printf(MSG" = \n"); gsl_vector_fprintf(stdout,X,"%f"); printf("\n");
#else
#define DEBUGVEC(MSG,X)
#endif

#define DVVIEW(A) gsl_vector_view A = gsl_vector_view_array(A##p,A##n)
#define DMVIEW(A) gsl_matrix_view A = gsl_matrix_view_array(A##p,A##r,A##c)
#define CVVIEW(A) gsl_vector_complex_view A = gsl_vector_complex_view_array((double*)A##p,A##n)
#define CMVIEW(A) gsl_matrix_complex_view A = gsl_matrix_complex_view_array((double*)A##p,A##r,A##c)
#define KDVVIEW(A) gsl_vector_const_view A = gsl_vector_const_view_array(A##p,A##n)
#define KDMVIEW(A) gsl_matrix_const_view A = gsl_matrix_const_view_array(A##p,A##r,A##c)
#define KCVVIEW(A) gsl_vector_complex_const_view A = gsl_vector_complex_const_view_array((double*)A##p,A##n)
#define KCMVIEW(A) gsl_matrix_complex_const_view A = gsl_matrix_complex_const_view_array((double*)A##p,A##r,A##c)

#define FVVIEW(A) gsl_vector_float_view A = gsl_vector_float_view_array(A##p,A##n)
#define FMVIEW(A) gsl_matrix_float_view A = gsl_matrix_float_view_array(A##p,A##r,A##c)
#define QVVIEW(A) gsl_vector_complex_float_view A = gsl_vector_float_complex_view_array((float*)A##p,A##n)
#define QMVIEW(A) gsl_matrix_complex_float_view A = gsl_matrix_float_complex_view_array((float*)A##p,A##r,A##c)
#define KFVVIEW(A) gsl_vector_float_const_view A = gsl_vector_float_const_view_array(A##p,A##n)
#define KFMVIEW(A) gsl_matrix_float_const_view A = gsl_matrix_float_const_view_array(A##p,A##r,A##c)
#define KQVVIEW(A) gsl_vector_complex_float_const_view A = gsl_vector_complex_float_const_view_array((float*)A##p,A##n)
#define KQMVIEW(A) gsl_matrix_complex_float_const_view A = gsl_matrix_complex_float_const_view_array((float*)A##p,A##r,A##c)

#define V(a) (&a.vector)
#define M(a) (&a.matrix)

#define GCVEC(A) int A##n, gsl_complex*A##p
#define KGCVEC(A) int A##n, const gsl_complex*A##p

#define GQVEC(A) int A##n, gsl_complex_float*A##p
#define KGQVEC(A) int A##n, const gsl_complex_float*A##p

#define BAD_SIZE 2000
#define BAD_CODE 2001
#define MEM      2002
#define BAD_FILE 2003


int sumF(KFVEC(x),FVEC(r)) {
    DEBUGMSG("sumF");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    float res = 0;
    for (i = 0; i < xn; i++) res += xp[i];
    rp[0] = res;
    OK
}
    
int sumR(KRVEC(x),RVEC(r)) {
    DEBUGMSG("sumR");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    double res = 0;
    for (i = 0; i < xn; i++) res += xp[i];
    rp[0] = res;
    OK
}
    
int sumQ(KQVEC(x),QVEC(r)) {
    DEBUGMSG("sumQ");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    gsl_complex_float res;
    res.dat[0] = 0;
    res.dat[1] = 0;
    for (i = 0; i < xn; i++) {
      res.dat[0] += xp[i].dat[0];
      res.dat[1] += xp[i].dat[1];
    }
    rp[0] = res;
    OK
}
    
int sumC(KCVEC(x),CVEC(r)) {
    DEBUGMSG("sumC");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    gsl_complex res;
    res.dat[0] = 0;
    res.dat[1] = 0;
    for (i = 0; i < xn; i++)  {
      res.dat[0] += xp[i].dat[0];
      res.dat[1] += xp[i].dat[1];
    }
    rp[0] = res;
    OK
}

int prodF(KFVEC(x),FVEC(r)) {
    DEBUGMSG("prodF");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    float res = 1;
    for (i = 0; i < xn; i++) res *= xp[i];
    rp[0] = res;
    OK
}
    
int prodR(KRVEC(x),RVEC(r)) {
    DEBUGMSG("prodR");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    double res = 1;
    for (i = 0; i < xn; i++) res *= xp[i];
    rp[0] = res;
    OK
}
    
int prodQ(KQVEC(x),QVEC(r)) {
    DEBUGMSG("prodQ");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    gsl_complex_float res;
    float temp;
    res.dat[0] = 1;
    res.dat[1] = 0;
    for (i = 0; i < xn; i++) {
      temp       = res.dat[0] * xp[i].dat[0] - res.dat[1] * xp[i].dat[1];
      res.dat[1] = res.dat[0] * xp[i].dat[1] + res.dat[1] * xp[i].dat[0];
      res.dat[0] = temp;
    }
    rp[0] = res;
    OK
}
    
int prodC(KCVEC(x),CVEC(r)) {
    DEBUGMSG("prodC");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    gsl_complex res;
    double temp;
    res.dat[0] = 1;
    res.dat[1] = 0;
    for (i = 0; i < xn; i++)  {
      temp       = res.dat[0] * xp[i].dat[0] - res.dat[1] * xp[i].dat[1];
      res.dat[1] = res.dat[0] * xp[i].dat[1] + res.dat[1] * xp[i].dat[0];
      res.dat[0] = temp;
    }
    rp[0] = res;
    OK
}

int dotF(KFVEC(x), KFVEC(y), FVEC(r)) {
    DEBUGMSG("dotF");
    REQUIRES(xn==yn,BAD_SIZE); 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("dotF");
    KFVVIEW(x);
    KFVVIEW(y);
    gsl_blas_sdot(V(x),V(y),rp);
    OK
}
    
int dotR(KRVEC(x), KRVEC(y), RVEC(r)) {
    DEBUGMSG("dotR");
    REQUIRES(xn==yn,BAD_SIZE); 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("dotR");
    KDVVIEW(x);
    KDVVIEW(y);
    gsl_blas_ddot(V(x),V(y),rp);
    OK
}
    
int dotQ(KQVEC(x), KQVEC(y), QVEC(r)) {
    DEBUGMSG("dotQ");
    REQUIRES(xn==yn,BAD_SIZE); 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("dotQ");
    KQVVIEW(x);
    KQVVIEW(y);
    gsl_blas_cdotu(V(x),V(y),rp);
    OK
}
    
int dotC(KCVEC(x), KCVEC(y), CVEC(r)) {
    DEBUGMSG("dotC");
    REQUIRES(xn==yn,BAD_SIZE); 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("dotC");
    KCVVIEW(x);
    KCVVIEW(y);
    gsl_blas_zdotu(V(x),V(y),rp);
    OK
}
    
int toScalarR(int code, KRVEC(x), RVEC(r)) { 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarR");
    KDVVIEW(x);
    double res;
    switch(code) {
        case 0: { res = gsl_blas_dnrm2(V(x)); break; } 
        case 1: { res = gsl_blas_dasum(V(x));  break; }
        case 2: { res = gsl_vector_max_index(V(x));  break; }
        case 3: { res = gsl_vector_max(V(x));  break; }
        case 4: { res = gsl_vector_min_index(V(x)); break; }
        case 5: { res = gsl_vector_min(V(x)); break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}

int toScalarF(int code, KFVEC(x), FVEC(r)) { 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarF");
    KFVVIEW(x);
    float res;
    switch(code) {
        case 0: { res = gsl_blas_snrm2(V(x)); break; } 
        case 1: { res = gsl_blas_sasum(V(x));  break; }
        case 2: { res = gsl_vector_float_max_index(V(x));  break; }
        case 3: { res = gsl_vector_float_max(V(x));  break; }
        case 4: { res = gsl_vector_float_min_index(V(x)); break; }
        case 5: { res = gsl_vector_float_min(V(x)); break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}


int toScalarC(int code, KCVEC(x), RVEC(r)) { 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarC");
    KCVVIEW(x);
    double res;
    switch(code) {
        case 0: { res = gsl_blas_dznrm2(V(x)); break; } 
        case 1: { res = gsl_blas_dzasum(V(x));  break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}

int toScalarQ(int code, KQVEC(x), FVEC(r)) { 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarQ");
    KQVVIEW(x);
    float res;
    switch(code) {
        case 0: { res = gsl_blas_scnrm2(V(x)); break; } 
        case 1: { res = gsl_blas_scasum(V(x));  break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}


inline double sign(double x) {
    if(x>0) {
        return +1.0;
    } else if (x<0) {
        return -1.0;
    } else {
        return 0.0;
    }
}

inline float float_sign(float x) {
    if(x>0) {
        return +1.0;
    } else if (x<0) {
        return -1.0;
    } else {
        return 0.0;
    }
}

inline gsl_complex complex_abs(gsl_complex z) {
    gsl_complex r;
    r.dat[0] = gsl_complex_abs(z);
    r.dat[1] = 0;
    return r;
}

inline gsl_complex complex_signum(gsl_complex z) {
    gsl_complex r;
    double mag;
    if (z.dat[0] == 0 && z.dat[1] == 0) {
        r.dat[0] = 0;
        r.dat[1] = 0;
    } else {
        mag = gsl_complex_abs(z);
        r.dat[0] = z.dat[0]/mag;
        r.dat[1] = z.dat[1]/mag;
    }
    return r;
}

#define OP(C,F) case C: { for(k=0;k<xn;k++) rp[k] = F(xp[k]); OK }
#define OPV(C,E) case C: { for(k=0;k<xn;k++) rp[k] = E; OK }
int mapR(int code, KRVEC(x), RVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapR");
    switch (code) {
        OP(0,sin)
        OP(1,cos)
        OP(2,tan)
        OP(3,fabs)
        OP(4,asin)
        OP(5,acos)
        OP(6,atan) /* atan2 mediante vectorZip */
        OP(7,sinh)
        OP(8,cosh)
        OP(9,tanh)
        OP(10,gsl_asinh)
        OP(11,gsl_acosh)
        OP(12,gsl_atanh)
        OP(13,exp)
        OP(14,log)
        OP(15,sign)
        OP(16,sqrt)
        default: ERROR(BAD_CODE);
    }
}

int mapF(int code, KFVEC(x), FVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapF");
    switch (code) {
        OP(0,sin)
        OP(1,cos)
        OP(2,tan)
        OP(3,fabs)
        OP(4,asin)
        OP(5,acos)
        OP(6,atan) /* atan2 mediante vectorZip */
        OP(7,sinh)
        OP(8,cosh)
        OP(9,tanh)
        OP(10,gsl_asinh)
        OP(11,gsl_acosh)
        OP(12,gsl_atanh)
        OP(13,exp)
        OP(14,log)
        OP(15,sign)
        OP(16,sqrt)
        default: ERROR(BAD_CODE);
    }
}


int mapCAux(int code, KGCVEC(x), GCVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapC");
    switch (code) {
        OP(0,gsl_complex_sin)
        OP(1,gsl_complex_cos)
        OP(2,gsl_complex_tan)
        OP(3,complex_abs)
        OP(4,gsl_complex_arcsin)
        OP(5,gsl_complex_arccos)
        OP(6,gsl_complex_arctan)
        OP(7,gsl_complex_sinh)
        OP(8,gsl_complex_cosh)
        OP(9,gsl_complex_tanh)
        OP(10,gsl_complex_arcsinh)
        OP(11,gsl_complex_arccosh)
        OP(12,gsl_complex_arctanh)
        OP(13,gsl_complex_exp)
        OP(14,gsl_complex_log)
        OP(15,complex_signum)
        OP(16,gsl_complex_sqrt)

        // gsl_complex_arg
        // gsl_complex_abs
        default: ERROR(BAD_CODE);
    }
}

int mapC(int code, KCVEC(x), CVEC(r)) {
    return mapCAux(code, xn, (gsl_complex*)xp, rn, (gsl_complex*)rp);
}


gsl_complex_float complex_float_math_fun(gsl_complex (*cf)(gsl_complex), gsl_complex_float a)
{
  gsl_complex c;
  gsl_complex r;

  gsl_complex_float float_r;

  c.dat[0] = a.dat[0];
  c.dat[1] = a.dat[1];

  r = (*cf)(c);

  float_r.dat[0] = r.dat[0];
  float_r.dat[1] = r.dat[1];

  return float_r;
}

gsl_complex_float complex_float_math_op(gsl_complex (*cf)(gsl_complex,gsl_complex), 
					gsl_complex_float a,gsl_complex_float b)
{
  gsl_complex c1;
  gsl_complex c2;
  gsl_complex r;

  gsl_complex_float float_r;

  c1.dat[0] = a.dat[0];
  c1.dat[1] = a.dat[1];

  c2.dat[0] = b.dat[0];
  c2.dat[1] = b.dat[1];

  r = (*cf)(c1,c2);

  float_r.dat[0] = r.dat[0];
  float_r.dat[1] = r.dat[1];

  return float_r;
}

#define OPC(C,F) case C: { for(k=0;k<xn;k++) rp[k] = complex_float_math_fun(&F,xp[k]); OK }
#define OPCA(C,F,A,B) case C: { for(k=0;k<xn;k++) rp[k] = complex_float_math_op(&F,A,B); OK }
int mapQAux(int code, KGQVEC(x), GQVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapQ");
    switch (code) {
        OPC(0,gsl_complex_sin)
        OPC(1,gsl_complex_cos)
        OPC(2,gsl_complex_tan)
        OPC(3,complex_abs)
        OPC(4,gsl_complex_arcsin)
        OPC(5,gsl_complex_arccos)
        OPC(6,gsl_complex_arctan)
        OPC(7,gsl_complex_sinh)
        OPC(8,gsl_complex_cosh)
        OPC(9,gsl_complex_tanh)
        OPC(10,gsl_complex_arcsinh)
        OPC(11,gsl_complex_arccosh)
        OPC(12,gsl_complex_arctanh)
        OPC(13,gsl_complex_exp)
        OPC(14,gsl_complex_log)
        OPC(15,complex_signum)
        OPC(16,gsl_complex_sqrt)

        // gsl_complex_arg
        // gsl_complex_abs
        default: ERROR(BAD_CODE);
    }
}

int mapQ(int code, KQVEC(x), QVEC(r)) {
    return mapQAux(code, xn, (gsl_complex_float*)xp, rn, (gsl_complex_float*)rp);
}


int mapValR(int code, double* pval, KRVEC(x), RVEC(r)) {
    int k;
    double val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValR");
    switch (code) {
        OPV(0,val*xp[k])
        OPV(1,val/xp[k])
        OPV(2,val+xp[k])
        OPV(3,val-xp[k])
        OPV(4,pow(val,xp[k]))
        OPV(5,pow(xp[k],val))
        default: ERROR(BAD_CODE);
    }
}

int mapValF(int code, float* pval, KFVEC(x), FVEC(r)) {
    int k;
    float val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValF");
    switch (code) {
        OPV(0,val*xp[k])
        OPV(1,val/xp[k])
        OPV(2,val+xp[k])
        OPV(3,val-xp[k])
        OPV(4,pow(val,xp[k]))
        OPV(5,pow(xp[k],val))
        default: ERROR(BAD_CODE);
    }
}

int mapValCAux(int code, gsl_complex* pval, KGCVEC(x), GCVEC(r)) {
    int k;
    gsl_complex val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValC");
    switch (code) {
        OPV(0,gsl_complex_mul(val,xp[k]))
        OPV(1,gsl_complex_div(val,xp[k]))
        OPV(2,gsl_complex_add(val,xp[k]))
        OPV(3,gsl_complex_sub(val,xp[k]))
        OPV(4,gsl_complex_pow(val,xp[k]))
        OPV(5,gsl_complex_pow(xp[k],val))
        default: ERROR(BAD_CODE);
    }
}

int mapValC(int code, gsl_complex* val, KCVEC(x), CVEC(r)) {
    return mapValCAux(code, val, xn, (gsl_complex*)xp, rn, (gsl_complex*)rp);
}


int mapValQAux(int code, gsl_complex_float* pval, KQVEC(x), GQVEC(r)) {
    int k;
    gsl_complex_float val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValQ");
    switch (code) {
        OPCA(0,gsl_complex_mul,val,xp[k])
	OPCA(1,gsl_complex_div,val,xp[k])
	OPCA(2,gsl_complex_add,val,xp[k])
	OPCA(3,gsl_complex_sub,val,xp[k])
	OPCA(4,gsl_complex_pow,val,xp[k])
	OPCA(5,gsl_complex_pow,xp[k],val)
        default: ERROR(BAD_CODE);
    }
}

int mapValQ(int code, gsl_complex_float* val, KQVEC(x), QVEC(r)) {
    return mapValQAux(code, val, xn, (gsl_complex_float*)xp, rn, (gsl_complex_float*)rp);
}


#define OPZE(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = E(ap[k],bp[k]); OK }
#define OPZV(C,msg,E) case C: {DEBUGMSG(msg) res = E(V(r),V(b)); CHECK(res,res); OK }
int zipR(int code, KRVEC(a), KRVEC(b), RVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZE(4,"zipR Pow",pow)
        OPZE(5,"zipR ATan2",atan2)
    }
    KDVVIEW(a);
    KDVVIEW(b);
    DVVIEW(r);
    gsl_vector_memcpy(V(r),V(a));
    int res;
    switch(code) {
        OPZV(0,"zipR Add",gsl_vector_add)
        OPZV(1,"zipR Sub",gsl_vector_sub)
        OPZV(2,"zipR Mul",gsl_vector_mul)
        OPZV(3,"zipR Div",gsl_vector_div)
        default: ERROR(BAD_CODE);
    }
}


int zipF(int code, KFVEC(a), KFVEC(b), FVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZE(4,"zipF Pow",pow)
        OPZE(5,"zipF ATan2",atan2)
    }
    KFVVIEW(a);
    KFVVIEW(b);
    FVVIEW(r);
    gsl_vector_float_memcpy(V(r),V(a));
    int res;
    switch(code) {
        OPZV(0,"zipF Add",gsl_vector_float_add)
        OPZV(1,"zipF Sub",gsl_vector_float_sub)
        OPZV(2,"zipF Mul",gsl_vector_float_mul)
        OPZV(3,"zipF Div",gsl_vector_float_div)
        default: ERROR(BAD_CODE);
    }
}


int zipCAux(int code, KGCVEC(a), KGCVEC(b), GCVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZE(0,"zipC Add",gsl_complex_add)
        OPZE(1,"zipC Sub",gsl_complex_sub)
        OPZE(2,"zipC Mul",gsl_complex_mul)
        OPZE(3,"zipC Div",gsl_complex_div)
        OPZE(4,"zipC Pow",gsl_complex_pow)
        //OPZE(5,"zipR ATan2",atan2)
    }
    //KCVVIEW(a);
    //KCVVIEW(b);
    //CVVIEW(r);
    //gsl_vector_memcpy(V(r),V(a));
    //int res;
    switch(code) {
        default: ERROR(BAD_CODE);
    }
}


int zipC(int code, KCVEC(a), KCVEC(b), CVEC(r)) {
    return zipCAux(code, an, (gsl_complex*)ap, bn, (gsl_complex*)bp, rn, (gsl_complex*)rp);
}


#define OPCZE(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = complex_float_math_op(&E,ap[k],bp[k]); OK }
int zipQAux(int code, KGQVEC(a), KGQVEC(b), GQVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPCZE(0,"zipQ Add",gsl_complex_add)
        OPCZE(1,"zipQ Sub",gsl_complex_sub)
        OPCZE(2,"zipQ Mul",gsl_complex_mul)
        OPCZE(3,"zipQ Div",gsl_complex_div)
        OPCZE(4,"zipQ Pow",gsl_complex_pow)
        //OPZE(5,"zipR ATan2",atan2)
    }
    //KCVVIEW(a);
    //KCVVIEW(b);
    //CVVIEW(r);
    //gsl_vector_memcpy(V(r),V(a));
    //int res;
    switch(code) {
        default: ERROR(BAD_CODE);
    }
}


int zipQ(int code, KQVEC(a), KQVEC(b), QVEC(r)) {
    return zipQAux(code, an, (gsl_complex_float*)ap, bn, (gsl_complex_float*)bp, rn, (gsl_complex_float*)rp);
}


int vector_fscanf(char*filename, RVEC(a)) {
    DEBUGMSG("gsl_vector_fscanf");
    DVVIEW(a);
    FILE * f = fopen(filename,"r");
    CHECK(!f,BAD_FILE);
    int res = gsl_vector_fscanf(f,V(a));
    CHECK(res,res);
    fclose (f);
    OK
}

int vector_fprintf(char*filename, char*fmt, RVEC(a)) {
    DEBUGMSG("gsl_vector_fprintf");
    DVVIEW(a);
    FILE * f = fopen(filename,"w");
    CHECK(!f,BAD_FILE);
    int res = gsl_vector_fprintf(f,V(a),fmt);
    CHECK(res,res);
    fclose (f);
    OK
}

int vector_fread(char*filename, RVEC(a)) {
    DEBUGMSG("gsl_vector_fread");
    DVVIEW(a);
    FILE * f = fopen(filename,"r");
    CHECK(!f,BAD_FILE);
    int res = gsl_vector_fread(f,V(a));
    CHECK(res,res);
    fclose (f);
    OK
}

int vector_fwrite(char*filename, RVEC(a)) {
    DEBUGMSG("gsl_vector_fwrite");
    DVVIEW(a);
    FILE * f = fopen(filename,"w");
    CHECK(!f,BAD_FILE);
    int res = gsl_vector_fwrite(f,V(a));
    CHECK(res,res);
    fclose (f);
    OK
}

int matrix_fprintf(char*filename, char*fmt, int ro, RMAT(m)) {
    DEBUGMSG("matrix_fprintf");
    FILE * f = fopen(filename,"w");
    CHECK(!f,BAD_FILE);
    int i,j,sr,sc;
    if (ro==1) { sr = mc; sc = 1;} else { sr = 1; sc = mr;}
    #define AT(M,r,c) (M##p[(r)*sr+(c)*sc])
    for (i=0; i<mr; i++) {
        for (j=0; j<mc-1; j++) {
            fprintf(f,fmt,AT(m,i,j));
            fprintf(f," ");
        }
        fprintf(f,fmt,AT(m,i,j));
        fprintf(f,"\n");
    }
    fclose (f);
    OK
}

//---------------------------------------------------------------

#define RAN(C,F) case C: { for(k=0;k<rn;k++) { rp[k]= F(gen); }; OK }

int random_vector(int seed, int code, RVEC(r)) {
    DEBUGMSG("random_vector")
    static gsl_rng * gen = NULL;
    if (!gen) { gen = gsl_rng_alloc (gsl_rng_mt19937);}
    gsl_rng_set (gen, seed);
    int k;
    switch (code) {
        RAN(0,gsl_rng_uniform)
        RAN(1,gsl_ran_ugaussian)
        default: ERROR(BAD_CODE);
    }
}
#undef RAN

