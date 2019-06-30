#include <complex.h>
#include <inttypes.h>

typedef double complex TCD;
typedef float  complex TCF;

#undef complex

#include "lapack-aux.h"

#define V(x) x##n,x##p

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

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

int sumR(KDVEC(x),DVEC(r)) {
    DEBUGMSG("sumR");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    double res = 0;
    for (i = 0; i < xn; i++) res += xp[i];
    rp[0] = res;
    OK
}

int sumI(int m, KIVEC(x),IVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    int res = 0;
    if (m==1) {
        for (i = 0; i < xn; i++) res += xp[i];
    } else {
        for (i = 0; i < xn; i++) res = (res + xp[i]) % m;
    }
    rp[0] = res;
    OK
}

int sumL(int64_t m, KLVEC(x),LVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    int res = 0;
    if (m==1) {
        for (i = 0; i < xn; i++) res += xp[i];
    } else {
        for (i = 0; i < xn; i++) res = (res + xp[i]) % m;
    }
    rp[0] = res;
    OK
}

int sumQ(KQVEC(x),QVEC(r)) {
    DEBUGMSG("sumQ");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    complex res;
    res.r = 0;
    res.i = 0;
    for (i = 0; i < xn; i++) {
      res.r += xp[i].r;
      res.i += xp[i].i;
    }
    rp[0] = res;
    OK
}

int sumC(KCVEC(x),CVEC(r)) {
    DEBUGMSG("sumC");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    doublecomplex res;
    res.r = 0;
    res.i = 0;
    for (i = 0; i < xn; i++)  {
      res.r += xp[i].r;
      res.i += xp[i].i;
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

int prodR(KDVEC(x),DVEC(r)) {
    DEBUGMSG("prodR");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    double res = 1;
    for (i = 0; i < xn; i++) res *= xp[i];
    rp[0] = res;
    OK
}

int prodI(int m, KIVEC(x),IVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    int res = 1;
    if (m==1) {
        for (i = 0; i < xn; i++) res *= xp[i];
    } else {
        for (i = 0; i < xn; i++) res = (res * xp[i]) % m;
    }
    rp[0] = res;
    OK
}

int prodL(int64_t m, KLVEC(x),LVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    int res = 1;
    if (m==1) {
        for (i = 0; i < xn; i++) res *= xp[i];
    } else {
        for (i = 0; i < xn; i++) res = (res * xp[i]) % m;
    }
    rp[0] = res;
    OK
}

int prodQ(KQVEC(x),QVEC(r)) {
    DEBUGMSG("prodQ");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    complex res;
    float temp;
    res.r = 1;
    res.i = 0;
    for (i = 0; i < xn; i++) {
      temp  = res.r * xp[i].r - res.i * xp[i].i;
      res.i = res.r * xp[i].i + res.i * xp[i].r;
      res.r = temp;
    }
    rp[0] = res;
    OK
}

int prodC(KCVEC(x),CVEC(r)) {
    DEBUGMSG("prodC");
    REQUIRES(rn==1,BAD_SIZE);
    int i;
    doublecomplex res;
    double temp;
    res.r = 1;
    res.i = 0;
    for (i = 0; i < xn; i++)  {
      temp  = res.r * xp[i].r - res.i * xp[i].i;
      res.i = res.r * xp[i].i + res.i * xp[i].r;
      res.r = temp;
    }
    rp[0] = res;
    OK
}


double dnrm2_(integer*, const double*, integer*);
double dasum_(integer*, const double*, integer*);

double vector_max(KDVEC(x)) {
    double r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]>r) {
            r = xp[k];
        }
    }
    return r;
}

double vector_min(KDVEC(x)) {
    double r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]<r) {
            r = xp[k];
        }
    }
    return r;
}

int vector_max_index(KDVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[r]) {
            r = k;
        }
    }
    return r;
}

int vector_min_index(KDVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[r]) {
            r = k;
        }
    }
    return r;
}

int toScalarR(int code, KDVEC(x), DVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarR");
    double res;
    integer one = 1;
    integer n = xn;
    switch(code) {
        case 0: { res = dnrm2_(&n,xp,&one); break; }
        case 1: { res = dasum_(&n,xp,&one);  break; }
        case 2: { res = vector_max_index(V(x));  break; }
        case 3: { res = vector_max(V(x));  break; }
        case 4: { res = vector_min_index(V(x)); break; }
        case 5: { res = vector_min(V(x)); break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}


float snrm2_(integer*, const float*, integer*);
float sasum_(integer*, const float*, integer*);

float vector_max_f(KFVEC(x)) {
    float r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]>r) {
            r = xp[k];
        }
    }
    return r;
}

float vector_min_f(KFVEC(x)) {
    float r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]<r) {
            r = xp[k];
        }
    }
    return r;
}

int vector_max_index_f(KFVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[r]) {
            r = k;
        }
    }
    return r;
}

int vector_min_index_f(KFVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[r]) {
            r = k;
        }
    }
    return r;
}


int toScalarF(int code, KFVEC(x), FVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarF");
    float res;
    integer one = 1;
    integer n = xn;
    switch(code) {
        case 0: { res = snrm2_(&n,xp,&one); break; }
        case 1: { res = sasum_(&n,xp,&one);  break; }
        case 2: { res = vector_max_index_f(V(x));  break; }
        case 3: { res = vector_max_f(V(x));  break; }
        case 4: { res = vector_min_index_f(V(x)); break; }
        case 5: { res = vector_min_f(V(x)); break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}

int vector_max_i(KIVEC(x)) {
    int r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]>r) {
            r = xp[k];
        }
    }
    return r;
}

int vector_min_i(KIVEC(x)) {
    int r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]<r) {
            r = xp[k];
        }
    }
    return r;
}

int vector_max_index_i(KIVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[r]) {
            r = k;
        }
    }
    return r;
}

int vector_min_index_i(KIVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[r]) {
            r = k;
        }
    }
    return r;
}


int toScalarI(int code, KIVEC(x), IVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    int res;
    switch(code) {
        case 2: { res = vector_max_index_i(V(x));  break; }
        case 3: { res = vector_max_i(V(x));  break; }
        case 4: { res = vector_min_index_i(V(x)); break; }
        case 5: { res = vector_min_i(V(x)); break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}


int64_t vector_max_l(KLVEC(x)) {
    int64_t r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]>r) {
            r = xp[k];
        }
    }
    return r;
}

int64_t vector_min_l(KLVEC(x)) {
    int64_t r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]<r) {
            r = xp[k];
        }
    }
    return r;
}

int vector_max_index_l(KLVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[r]) {
            r = k;
        }
    }
    return r;
}

int vector_min_index_l(KLVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[r]) {
            r = k;
        }
    }
    return r;
}


int toScalarL(int code, KLVEC(x), LVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    int64_t res;
    switch(code) {
        case 2: { res = vector_max_index_l(V(x));  break; }
        case 3: { res = vector_max_l(V(x));  break; }
        case 4: { res = vector_min_index_l(V(x)); break; }
        case 5: { res = vector_min_l(V(x)); break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}


double dznrm2_(integer*, const doublecomplex*, integer*);
double dzasum_(integer*, const doublecomplex*, integer*);

int toScalarC(int code, KCVEC(x), DVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarC");
    double res;
    integer one = 1;
    integer n = xn;
    switch(code) {
        case 0: { res = dznrm2_(&n,xp,&one); break; }
        case 1: { res = dzasum_(&n,xp,&one);  break; }
        default: ERROR(BAD_CODE);
    }
    rp[0] = res;
    OK
}


double scnrm2_(integer*, const complex*, integer*);
double scasum_(integer*, const complex*, integer*);

int toScalarQ(int code, KQVEC(x), FVEC(r)) {
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarQ");
    float res;
    integer one = 1;
    integer n = xn;
    switch(code) {
        case 0: { res = scnrm2_(&n,xp,&one); break; }
        case 1: { res = scasum_(&n,xp,&one);  break; }
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


#define OP(C,F) case C: { for(k=0;k<xn;k++) rp[k] = F(xp[k]); OK }
#define OPV(C,E) case C: { for(k=0;k<xn;k++) rp[k] = E; OK }
int mapR(int code, KDVEC(x), DVEC(r)) {
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
        OP(6,atan)
        OP(7,sinh)
        OP(8,cosh)
        OP(9,tanh)
        OP(10,asinh)
        OP(11,acosh)
        OP(12,atanh)
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
        OP(6,atan)
        OP(7,sinh)
        OP(8,cosh)
        OP(9,tanh)
        OP(10,asinh)
        OP(11,acosh)
        OP(12,atanh)
        OP(13,exp)
        OP(14,log)
        OP(15,sign)
        OP(16,sqrt)
        default: ERROR(BAD_CODE);
    }
}


int mapI(int code, KIVEC(x), IVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    switch (code) {
        OP(3,abs)
        OP(15,sign)
        default: ERROR(BAD_CODE);
    }
}


int mapL(int code, KLVEC(x), LVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    switch (code) {
        OP(3,abs)
        OP(15,sign)
        default: ERROR(BAD_CODE);
    }
}



inline double abs_complex(doublecomplex z) {
    return sqrt(z.r*z.r + z.i*z.i);
}

inline doublecomplex complex_abs_complex(doublecomplex z) {
    doublecomplex r;
    r.r = abs_complex(z);
    r.i = 0;
    return r;
}

inline doublecomplex complex_signum_complex(doublecomplex z) {
    doublecomplex r;
    double mag;
    if (z.r == 0 && z.i == 0) {
        r.r = 0;
        r.i = 0;
    } else {
        mag = abs_complex(z);
        r.r = z.r/mag;
        r.i = z.i/mag;
    }
    return r;
}

#define OPb(C,F) case C: { for(k=0;k<xn;k++) r2p[k] = F(x2p[k]); OK }
int mapC(int code, KCVEC(x), CVEC(r)) {
    TCD* x2p = (TCD*)xp;
    TCD* r2p = (TCD*)rp;
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapC");
    switch (code) {
        OPb(0,csin)
        OPb(1,ccos)
        OPb(2,ctan)
        OP(3,complex_abs_complex)
        OPb(4,casin)
        OPb(5,cacos)
        OPb(6,catan)
        OPb(7,csinh)
        OPb(8,ccosh)
        OPb(9,ctanh)
        OPb(10,casinh)
        OPb(11,cacosh)
        OPb(12,catanh)
        OPb(13,cexp)
        OPb(14,clog)
        OP(15,complex_signum_complex)
        OPb(16,csqrt)
        default: ERROR(BAD_CODE);
    }
}



inline complex complex_f_math_fun(doublecomplex (*cf)(doublecomplex), complex a)
{
  doublecomplex c;
  doublecomplex r;

  complex float_r;

  c.r = a.r;
  c.i = a.i;

  r = (*cf)(c);

  float_r.r = r.r;
  float_r.i = r.i;

  return float_r;
}


#define OPC(C,F) case C: { for(k=0;k<xn;k++) rp[k] = complex_f_math_fun(&F,xp[k]); OK }
int mapQ(int code, KQVEC(x), QVEC(r)) {
    TCF* x2p = (TCF*)xp;
    TCF* r2p = (TCF*)rp;
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapQ");
    switch (code) {
        OPb(0,csinf)
        OPb(1,ccosf)
        OPb(2,ctanf)
        OPC(3,complex_abs_complex)
        OPb(4,casinf)
        OPb(5,cacosf)
        OPb(6,catanf)
        OPb(7,csinhf)
        OPb(8,ccoshf)
        OPb(9,ctanhf)
        OPb(10,casinhf)
        OPb(11,cacoshf)
        OPb(12,catanhf)
        OPb(13,cexpf)
        OPb(14,clogf)
        OPC(15,complex_signum_complex)
        OPb(16,csqrtf)
        default: ERROR(BAD_CODE);
    }
}


int mapValR(int code, double* pval, KDVEC(x), DVEC(r)) {
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

int mapValI(int code, int* pval, KIVEC(x), IVEC(r)) {
    int k;
    int val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValI");
    switch (code) {
        OPV(0,val*xp[k])
        OPV(1,val/xp[k])
        OPV(2,val+xp[k])
        OPV(3,val-xp[k])
        OPV(6,mod(val,xp[k]))
        OPV(7,mod(xp[k],val))
        default: ERROR(BAD_CODE);
    }
}

int mapValL(int code, int64_t* pval, KLVEC(x), LVEC(r)) {
    int k;
    int64_t val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValL");
    switch (code) {
        OPV(0,val*xp[k])
        OPV(1,val/xp[k])
        OPV(2,val+xp[k])
        OPV(3,val-xp[k])
        OPV(6,mod_l(val,xp[k]))
        OPV(7,mod_l(xp[k],val))
        default: ERROR(BAD_CODE);
    }
}



inline doublecomplex complex_add(doublecomplex a, doublecomplex b) {
    doublecomplex r;
    r.r = a.r+b.r;
    r.i = a.i+b.i;
    return r;
}

#define OPVb(C,E) case C: { for(k=0;k<xn;k++) r2p[k] = E; OK }
int mapValC(int code, doublecomplex* pval, KCVEC(x), CVEC(r)) {
    TCD* x2p = (TCD*)xp;
    TCD* r2p = (TCD*)rp;
    int k;
    TCD val = * (TCD*)pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValC");
    switch (code) {
        OPVb(0,val*x2p[k])
        OPVb(1,val/x2p[k])
        OPVb(2,val+x2p[k])
        OPVb(3,val-x2p[k])
        OPVb(4,cpow(val,x2p[k]))
        OPVb(5,cpow(x2p[k],val))
        default: ERROR(BAD_CODE);
    }
}


int mapValQ(int code, complex* pval, KQVEC(x), QVEC(r)) {
    TCF* x2p = (TCF*)xp;
    TCF* r2p = (TCF*)rp;
    int k;
    TCF val = *(TCF*)pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValQ");
    switch (code) {
        OPVb(0,val*x2p[k])
        OPVb(1,val/x2p[k])
        OPVb(2,val+x2p[k])
        OPVb(3,val-x2p[k])
        OPVb(4,cpow(val,x2p[k]))
        OPVb(5,cpow(x2p[k],val))
        default: ERROR(BAD_CODE);
    }
}



#define OPZE(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = E(ap[k],bp[k]); OK }
#define OPZV(C,msg,E) case C: {DEBUGMSG(msg) res = E(V(r),V(b)); CHECK(res,res); OK }
#define OPZO(C,msg,O) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = ap[k] O bp[k]; OK }

int zipR(int code, KDVEC(a), KDVEC(b), DVEC(r)) {
REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZO(0,"zipR Add",+)
        OPZO(1,"zipR Sub",-)
        OPZO(2,"zipR Mul",*)
        OPZO(3,"zipR Div",/)
        OPZE(4,"zipR Pow",  pow)
        OPZE(5,"zipR ATan2",atan2)
        default: ERROR(BAD_CODE);
    }
}

int zipF(int code, KFVEC(a), KFVEC(b), FVEC(r)) {
REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZO(0,"zipR Add",+)
        OPZO(1,"zipR Sub",-)
        OPZO(2,"zipR Mul",*)
        OPZO(3,"zipR Div",/)
        OPZE(4,"zipR Pow",  pow)
        OPZE(5,"zipR ATan2",atan2)
        default: ERROR(BAD_CODE);
    }
}


int zipI(int code, KIVEC(a), KIVEC(b), IVEC(r)) {
REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZO(0,"zipI Add",+)
        OPZO(1,"zipI Sub",-)
        OPZO(2,"zipI Mul",*)
        OPZO(3,"zipI Div",/)
        OPZO(6,"zipI Mod",%)
        default: ERROR(BAD_CODE);
    }
}


int zipL(int code, KLVEC(a), KLVEC(b), LVEC(r)) {
REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZO(0,"zipI Add",+)
        OPZO(1,"zipI Sub",-)
        OPZO(2,"zipI Mul",*)
        OPZO(3,"zipI Div",/)
        OPZO(6,"zipI Mod",%)
        default: ERROR(BAD_CODE);
    }
}


#define OPZOb(C,msg,O) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) r2p[k] = a2p[k] O b2p[k]; OK }
#define OPZEb(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) r2p[k] = E(a2p[k],b2p[k]); OK }
int zipC(int code, KCVEC(a), KCVEC(b), CVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    TCD* a2p = (TCD*)ap;
    TCD* b2p = (TCD*)bp;
    TCD* r2p = (TCD*)rp;
    int k;
    switch(code) {
        OPZOb(0,"zipC Add",+)
        OPZOb(1,"zipC Sub",-)
        OPZOb(2,"zipC Mul",*)
        OPZOb(3,"zipC Div",/)
        OPZEb(4,"zipC Pow",cpow)
        default: ERROR(BAD_CODE);
    }
}





#define OPCZE(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = complex_f_math_op(&E,ap[k],bp[k]); OK }

int zipQ(int code, KQVEC(a), KQVEC(b), QVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    TCF* a2p = (TCF*)ap;
    TCF* b2p = (TCF*)bp;
    TCF* r2p = (TCF*)rp;

    int k;
    switch(code) {
        OPZOb(0,"zipC Add",+)
        OPZOb(1,"zipC Sub",-)
        OPZOb(2,"zipC Mul",*)
        OPZOb(3,"zipC Div",/)
        OPZEb(4,"zipC Pow",cpowf)
        default: ERROR(BAD_CODE);
    }
}

////////////////////////////////////////////////////////////////////////////////

int vectorScan(char * file, int* n, double**pp){
    FILE * fp;
    fp = fopen (file, "r");
    if(!fp) {
        ERROR(BAD_FILE);
    }
    int nbuf = 100*100;
    double * p = (double*)malloc(nbuf*sizeof(double));
    int k=0;
    double d;
    int ok;
    for (;;) {
        ok = fscanf(fp,"%lf",&d);
        if (ok<1) {
            break;
        }
        if (k==nbuf) {
            nbuf = nbuf * 2;
            p = (double*)realloc(p,nbuf*sizeof(double));
            // printf("R\n");
        }
        p[k++] = d;
    }
    *n = k;
    *pp = p;
    fclose(fp);
    OK
}

////////////////////////////////////////////////////////////////////////////////

#if defined (__APPLE__) || (__FreeBSD__) || defined(NO_RANDOM_R) || defined(_WIN32) || defined(WIN32)
/* Windows use thread-safe random
   See: http://stackoverflow.com/questions/143108/is-windows-rand-s-thread-safe
*/
#if defined (__APPLE__) || (__FreeBSD__) || defined(NO_RANDOM_R)

/* For FreeBSD, Mac OS X, and other libcs (like `musl`) that do not provide
   random_r(), or if the use of random_r() is explicitly disabled, thread safety
   cannot be guaranteed.
   As per current understanding, this should at worst lead to less "random"
   numbers being generated, in particular
     * if another thread somebody calls lcong48() at the same time as nrand48()
       is called
     * in addition to that, for glibc with NO_RANDOM_R enabled when ndrand48()
       is called for the first time by multiple threads in parallel due to the
       initialisation function placed within it
   See: http://www.evanjones.ca/random-thread-safe.html

   For FreeBSD and Mac OS X, nrand48() is much better than random().
   See: http://www.evanjones.ca/random-thread-safe.html

   TODO: As mentioned in the linked article, this could be fixed:
         "the best solution for truly portable applications is to include
          your own random number generator implementation,
          and not rely on the system's C library".
*/
#pragma message "randomVector is not thread-safe in OSX and FreeBSD or with NO_RANDOM_R; this likely leads to less random numbers at worst; see http://www.evanjones.ca/random-thread-safe.html"

inline double urandom() {
    /* the probalility of matching will be theoretically p^3(in fact, it is not)
       p is matching probalility of random().
       using the test there, only 3 matches, using random(), 13783 matches
    */
    unsigned short state[3];
    state[0] = random();
    state[1] = random();
    state[2] = random();

    const long max_random = 2147483647; // 2**31 - 1
    return (double)nrand48(state) / (double)max_random;
}

#else

#define _CRT_RAND_S
inline double urandom() {
    unsigned int number;
    errno_t err;
    err = rand_s(&number);
    if (err!=0) {
        printf("something wrong\n");
        return -1;
    }
    return (double)number / (double)UINT_MAX;
}

#endif

double gaussrand(int *phase, double *pV1, double *pV2, double *pS)
{
	double V1=*pV1, V2=*pV2, S=*pS;
	double X;

	if(*phase == 0) {
		do {
            double U1 = urandom();
			double U2 = urandom();

			V1 = 2 * U1 - 1;
			V2 = 2 * U2 - 1;
			S = V1 * V1 + V2 * V2;
			} while(S >= 1 || S == 0);

		X = V1 * sqrt(-2 * log(S) / S);
	} else
		X = V2 * sqrt(-2 * log(S) / S);

	*phase = 1 - *phase;
    *pV1=V1; *pV2=V2; *pS=S;

	return X;

}

#if defined(_WIN32) || defined(WIN32)

int random_vector(unsigned int seed, int code, DVEC(r)) {
    int phase = 0;
    double V1,V2,S;

    srand(seed);

    int k;
    switch (code) {
      case 0: { // uniform
        for (k=0; k<rn; k++) {
            rp[k] = urandom();
        }
        OK
      }
      case 1: { // gaussian
        for (k=0; k<rn; k++) {
            rp[k] = gaussrand(&phase,&V1,&V2,&S);
        }
        OK
      }

      default: ERROR(BAD_CODE);
    }
}

#else

int random_vector(unsigned int seed, int code, DVEC(r)) {
    int phase = 0;
    double V1,V2,S;

    srandom(seed);

    int k;
    switch (code) {
      case 0: { // uniform
        for (k=0; k<rn; k++) {
            rp[k] = urandom();
        }
        OK
      }
      case 1: { // gaussian
        for (k=0; k<rn; k++) {
            rp[k] = gaussrand(&phase,&V1,&V2,&S);
        }
        OK
      }

      default: ERROR(BAD_CODE);
    }
}

#endif

#else

inline double urandom(struct random_data * buffer) {
    int32_t res;
    random_r(buffer,&res);
    return (double)res/RAND_MAX;
}


// http://c-faq.com/lib/gaussian.html
double gaussrand(struct random_data *buffer,
                 int *phase, double *pV1, double *pV2, double *pS)
{
	double V1=*pV1, V2=*pV2, S=*pS;
	double X;

	if(*phase == 0) {
		do {
            double U1 = urandom(buffer);
			double U2 = urandom(buffer);

			V1 = 2 * U1 - 1;
			V2 = 2 * U2 - 1;
			S = V1 * V1 + V2 * V2;
			} while(S >= 1 || S == 0);

		X = V1 * sqrt(-2 * log(S) / S);
	} else
		X = V2 * sqrt(-2 * log(S) / S);

	*phase = 1 - *phase;
    *pV1=V1; *pV2=V2; *pS=S;

	return X;

}

int random_vector(unsigned int seed, int code, DVEC(r)) {
    struct random_data buffer;
    char   random_state[128];
    memset(&buffer, 0, sizeof(struct random_data));
    memset(random_state, 0, sizeof(random_state));

    initstate_r(seed,random_state,sizeof(random_state),&buffer);
    // setstate_r(random_state,&buffer);
    // srandom_r(seed,&buffer);

    int phase = 0;
    double V1,V2,S;

    int k;
    switch (code) {
      case 0: { // uniform
        for (k=0; k<rn; k++) {
            rp[k] = urandom(&buffer);
        }
        OK
      }
      case 1: { // gaussian
        for (k=0; k<rn; k++) {
            rp[k] = gaussrand(&buffer,&phase,&V1,&V2,&S);
        }
        OK
      }

      default: ERROR(BAD_CODE);
    }
}

#endif

////////////////////////////////////////////////////////////////////////////////

int
compare_doubles (const void *a, const void *b) {
  return *(double*)a > *(double*)b;
}

int sort_valuesD(KDVEC(v),DVEC(r)) {
    memcpy(rp,vp,vn*sizeof(double));
    qsort(rp,rn,sizeof(double),compare_doubles);
    OK
}

int
compare_floats (const void *a, const void *b) {
  return *(float*)a > *(float*)b;
}

int sort_valuesF(KFVEC(v),FVEC(r)) {
    memcpy(rp,vp,vn*sizeof(float));
    qsort(rp,rn,sizeof(float),compare_floats);
    OK
}

int
compare_ints(const void *a, const void *b) {
  return *(int*)a > *(int*)b;
}

int sort_valuesI(KIVEC(v),IVEC(r)) {
    memcpy(rp,vp,vn*sizeof(int));
    qsort(rp,rn,sizeof(int),compare_ints);
    OK
}

int
compare_longs(const void *a, const void *b) {
  return *(int64_t*)a > *(int64_t*)b;
}

int sort_valuesL(KLVEC(v),LVEC(r)) {
    memcpy(rp,vp,vn*sizeof(int64_t));
    qsort(rp,rn,sizeof(int64_t),compare_ints);
    OK
}


////////////////////////////////////////


#define SORTIDX_IMP(T,C)                   \
    T* x = (T*)malloc(sizeof(T)*vn);       \
    int k;                                 \
    for (k=0;k<vn;k++) {                   \
        x[k].pos = k;                      \
        x[k].val = vp[k];                  \
    }                                      \
                                           \
    qsort(x,vn,sizeof(T),C);               \
                                           \
    for (k=0;k<vn;k++) {                   \
        rp[k] = x[k].pos;                  \
    }                                      \
    free(x);                               \
    OK


typedef struct DI { int pos; double val;} DI;

int compare_doubles_i (const void *a, const void *b) {
  return ((DI*)a)->val > ((DI*)b)->val;
}

int sort_indexD(KDVEC(v),IVEC(r)) {
    SORTIDX_IMP(DI,compare_doubles_i)
}


typedef struct FI { int pos; float  val;} FI;

int compare_floats_i (const void *a, const void *b) {
  return ((FI*)a)->val > ((FI*)b)->val;
}

int sort_indexF(KFVEC(v),IVEC(r)) {
    SORTIDX_IMP(FI,compare_floats_i)
}


typedef struct II { int pos; int    val;} II;

int compare_ints_i (const void *a, const void *b) {
  return ((II*)a)->val > ((II*)b)->val;
}

int sort_indexI(KIVEC(v),IVEC(r)) {
    SORTIDX_IMP(II,compare_ints_i)
}


typedef struct LI { int pos; int64_t val;} LI;

int compare_longs_i (const void *a, const void *b) {
  return ((II*)a)->val > ((II*)b)->val;
}

int sort_indexL(KLVEC(v),LVEC(r)) {
    SORTIDX_IMP(II,compare_longs_i)
}


////////////////////////////////////////////////////////////////////////////////

int round_vector(KDVEC(v),DVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = round(vp[k]);
    }
    OK
}

////////////////////////////////////////////////////////////////////////////////

int round_vector_i(KDVEC(v),IVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = round(vp[k]);
    }
    OK
}


int mod_vector(int m, KIVEC(v), IVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = mod(vp[k],m);
    }
    OK
}

int div_vector(int m, KIVEC(v), IVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = vp[k] / m;
    }
    OK
}

int range_vector(IVEC(r)) {
    int k;
    for(k=0; k<rn; k++) {
        rp[k] = k;
    }
    OK
}

///////////////////////////


int round_vector_l(KDVEC(v),LVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = round(vp[k]);
    }
    OK
}


int mod_vector_l(int64_t m, KLVEC(v), LVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = mod_l(vp[k],m);
    }
    OK
}

int div_vector_l(int64_t m, KLVEC(v), LVEC(r)) {
    int k;
    for(k=0; k<vn; k++) {
        rp[k] = vp[k] / m;
    }
    OK
}

int range_vector_l(LVEC(r)) {
    int k;
    for(k=0; k<rn; k++) {
        rp[k] = k;
    }
    OK
}



//////////////////// constant /////////////////////////

int constantF(float * pval, FVEC(r)) {
    DEBUGMSG("constantF")
    int k;
    double val = *pval;
    for(k=0;k<rn;k++) {
        rp[k]=val;
    }
    OK
}

int constantR(double * pval, DVEC(r)) {
    DEBUGMSG("constantR")
    int k;
    double val = *pval;
    for(k=0;k<rn;k++) {
        rp[k]=val;
    }
    OK
}

int constantQ(complex* pval, QVEC(r)) {
    DEBUGMSG("constantQ")
    int k;
    complex val = *pval;
    for(k=0;k<rn;k++) {
        rp[k]=val;
    }
    OK
}

int constantC(doublecomplex* pval, CVEC(r)) {
    DEBUGMSG("constantC")
    int k;
    doublecomplex val = *pval;
    for(k=0;k<rn;k++) {
        rp[k]=val;
    }
    OK
}



int constantI(int * pval, IVEC(r)) {
    DEBUGMSG("constantI")
    int k;
    int val = *pval;
    for(k=0;k<rn;k++) {
        rp[k]=val;
    }
    OK
}



int constantL(int64_t * pval, LVEC(r)) {
    DEBUGMSG("constantL")
    int k;
    int64_t val = *pval;
    for(k=0;k<rn;k++) {
        rp[k]=val;
    }
    OK
}


//////////////////// type conversions /////////////////////////

#define CONVERT_IMP {     \
    int k;                \
    for(k=0;k<xn;k++) {   \
        yp[k]=xp[k];      \
    }                     \
    OK }

int float2double(FVEC(x),DVEC(y)) CONVERT_IMP

int float2int(KFVEC(x),IVEC(y)) CONVERT_IMP

int double2float(DVEC(x),FVEC(y)) CONVERT_IMP

int double2int(KDVEC(x),IVEC(y)) CONVERT_IMP

int double2long(KDVEC(x),LVEC(y)) CONVERT_IMP

int int2float(KIVEC(x),FVEC(y)) CONVERT_IMP

int int2double(KIVEC(x),DVEC(y)) CONVERT_IMP

int int2long(KIVEC(x),LVEC(y)) CONVERT_IMP

int long2int(KLVEC(x),IVEC(y)) CONVERT_IMP

int long2double(KLVEC(x),DVEC(y)) CONVERT_IMP


//////////////////// conjugate /////////////////////////

int conjugateQ(KQVEC(x),QVEC(t)) {
    REQUIRES(xn==tn,BAD_SIZE);
    DEBUGMSG("conjugateQ");
    int k;
    for(k=0;k<xn;k++) {
        tp[k].r =  xp[k].r;
        tp[k].i = -xp[k].i;
    }
    OK
}

int conjugateC(KCVEC(x),CVEC(t)) {
    REQUIRES(xn==tn,BAD_SIZE);
    DEBUGMSG("conjugateC");
    int k;
    for(k=0;k<xn;k++) {
        tp[k].r =  xp[k].r;
        tp[k].i = -xp[k].i;
    }
    OK
}

//////////////////// step /////////////////////////

#define STEP_IMP         \
    int k;               \
    for(k=0;k<xn;k++) {  \
        yp[k]=xp[k]>0;   \
    }                    \
    OK

int stepF(KFVEC(x),FVEC(y)) {
    STEP_IMP
}

int stepD(KDVEC(x),DVEC(y)) {
    STEP_IMP
}

int stepI(KIVEC(x),IVEC(y)) {
    STEP_IMP
}

int stepL(KLVEC(x),LVEC(y)) {
    STEP_IMP
}


//////////////////// cond /////////////////////////

#define COMPARE_IMP                               \
    REQUIRES(xn==yn && xn==rn ,BAD_SIZE);         \
    int k;                                        \
    for(k=0;k<xn;k++) {                           \
        rp[k] = xp[k]<yp[k]?-1:(xp[k]>yp[k]?1:0); \
    }                                             \
    OK


int compareF(KFVEC(x),KFVEC(y),IVEC(r)) {
    COMPARE_IMP
}

int compareD(KDVEC(x),KDVEC(y),IVEC(r)) {
    COMPARE_IMP
}

int compareI(KIVEC(x),KIVEC(y),IVEC(r)) {
    COMPARE_IMP
}

int compareL(KLVEC(x),KLVEC(y),IVEC(r)) {
    COMPARE_IMP
}



#define CHOOSE_IMP                                                      \
    REQUIRES(condn==ltn && ltn==eqn && ltn==gtn && ltn==rn ,BAD_SIZE);  \
    int k;                                                              \
    for(k=0;k<condn;k++) {                                              \
        rp[k] = condp[k]<0?ltp[k]:(condp[k]>0?gtp[k]:eqp[k]);           \
    }                                                                   \
    OK

int chooseF(KIVEC(cond),KFVEC(lt),KFVEC(eq),KFVEC(gt),FVEC(r)) {
    CHOOSE_IMP
}

int chooseD(KIVEC(cond),KDVEC(lt),KDVEC(eq),KDVEC(gt),DVEC(r)) {
    CHOOSE_IMP
}

int chooseI(KIVEC(cond),KIVEC(lt),KIVEC(eq),KIVEC(gt),IVEC(r)) {
    CHOOSE_IMP
}

int chooseL(KIVEC(cond),KLVEC(lt),KLVEC(eq),KLVEC(gt),LVEC(r)) {
    CHOOSE_IMP
}


int chooseC(KIVEC(cond),KCVEC(lt),KCVEC(eq),KCVEC(gt),CVEC(r)) {
    CHOOSE_IMP
}

int chooseQ(KIVEC(cond),KQVEC(lt),KQVEC(eq),KQVEC(gt),QVEC(r)) {
    CHOOSE_IMP
}

//////////////////// reorder /////////////////////////

#define REORDER_IMP                                                                     \
    REQUIRES(kn == stridesn && stridesn == dimsn ,BAD_SIZE);                            \
    int i,j,l;                                                                          \
    for (i=1,j=0,l=0;l<kn;++l) {                                                        \
        kp[l] = 0;                                                                      \
        i *= dimsp[l];                                                                  \
        j += (dimsp[l]-1) * stridesp[l];                                                \
    }                                                                                   \
    REQUIRES(i <= vn && j < rn ,BAD_SIZE);                                              \
    for (i=0,j=0;;i++) {                                                                \
        rp[i] = vp[j];                                                                  \
        for(l=kn-1;;l--) {                                                              \
            ++kp[l];                                                                    \
            if (kp[l] < dimsp[l]) {                                                     \
                j += stridesp[l];                                                       \
                break;                                                                  \
            } else {                                                                    \
                if (l == 0) {                                                           \
                    return 0;                                                           \
                }                                                                       \
                kp[l] = 0;                                                              \
                j -= (dimsp[l]-1) * stridesp[l];                                        \
            }                                                                           \
        }                                                                               \
    }

int reorderF(IVEC(k), KIVEC(strides),KIVEC(dims),KFVEC(v),FVEC(r)) {
    REORDER_IMP
}

int reorderD(IVEC(k), KIVEC(strides),KIVEC(dims),KDVEC(v),DVEC(r)) {
    REORDER_IMP
}

int reorderI(IVEC(k), KIVEC(strides),KIVEC(dims),KIVEC(v),IVEC(r)) {
    REORDER_IMP
}

int reorderL(IVEC(k), KIVEC(strides),KIVEC(dims),KLVEC(v),LVEC(r)) {
    REORDER_IMP
}

int reorderC(IVEC(k), KIVEC(strides),KIVEC(dims),KCVEC(v),CVEC(r)) {
    REORDER_IMP
}

int reorderQ(IVEC(k), KIVEC(strides),KIVEC(dims),KQVEC(v),QVEC(r)) {
    REORDER_IMP
}
