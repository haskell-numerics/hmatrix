#include <complex.h>

typedef double complex TCD;
typedef float  complex TCF;

#undef complex

#include "lapack-aux.h"

#define V(x) x##n,x##p

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

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

double vector_max_index(KDVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[0]) {
            r = k;
        }
    }
    return r;
}

double vector_min_index(KDVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[0]) {
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

float vector_max_index_f(KFVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[0]) {
            r = k;
        }
    }
    return r;
}

float vector_min_index_f(KFVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[0]) {
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
            //printf("R\n");
        }
        p[k++] = d;
    }
    *n = k;
    *pp = p;
    fclose(fp);
    OK
}    

int saveMatrix(char * file, char * format, KDMAT(a)){
    FILE * fp;
    fp = fopen (file, "w");
    int r, c;
    for (r=0;r<ar; r++) {
        for (c=0; c<ac; c++) {
            fprintf(fp,format,ap[r*ac+c]);
            if (c<ac-1) {
                fprintf(fp," ");
            } else {
                fprintf(fp,"\n");
            }
        }
    }
    fclose(fp);
    OK
}

////////////////////////////////////////////////////////////////////////////////

// http://c-faq.com/lib/gaussian.html
double gaussrand()
{
	static double V1, V2, S;
	static int phase = 0;
	double X;

	if(phase == 0) {
		do {
			double U1 = (double)rand() / RAND_MAX;
			double U2 = (double)rand() / RAND_MAX;

			V1 = 2 * U1 - 1;
			V2 = 2 * U2 - 1;
			S = V1 * V1 + V2 * V2;
			} while(S >= 1 || S == 0);

		X = V1 * sqrt(-2 * log(S) / S);
	} else
		X = V2 * sqrt(-2 * log(S) / S);

	phase = 1 - phase;

	return X;
}

int random_vector(int seed, int code, DVEC(r)) {
    srand(seed);
    int k;
    switch (code) {
      case 0: { // uniform
        for (k=0; k<rn; k++) {
            rp[k] = (double)rand()/RAND_MAX;
        }
        OK
      }
      case 1: { // gaussian
        for (k=0; k<rn; k++) {
            rp[k] = gaussrand();
        }
        OK
      }

      default: ERROR(BAD_CODE);
    }
}

////////////////////////////////////////////////////////////////////////////////

int smXv(KDVEC(vals),KIVEC(cols),KIVEC(rows),KDVEC(x),DVEC(r)) {
    int r, c;
    for (r = 0; r < rowsn - 1; r++) {
        rp[r] = 0;
        for (c = rowsp[r]; c < rowsp[r+1]; c++) {
            rp[r] += valsp[c-1] * xp[colsp[c-1]-1];
        }
    }
    OK
}

int smTXv(KDVEC(vals),KIVEC(cols),KIVEC(rows),KDVEC(x),DVEC(r)) {
    int r,c;
    for (c = 0; c < rn; c++) {
        rp[c] = 0;
    }
    for (r = 0; r < rowsn - 1; r++) {
        for (c = rowsp[r]; c < rowsp[r+1]; c++) {
            rp[colsp[c-1]-1] += valsp[c-1] * xp[r];
        }
    }
    OK
}

