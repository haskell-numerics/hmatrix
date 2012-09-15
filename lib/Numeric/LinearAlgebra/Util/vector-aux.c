#include "../LAPACK/lapack-aux.h"

#define RVEC(A) int A##n, double*A##p
#define RMAT(A) int A##r, int A##c, double* A##p
#define KRVEC(A) int A##n, const double*A##p
#define KRMAT(A) int A##r, int A##c, const double* A##p

#define CVEC(A) int A##n, doublecomplex*A##p
#define CMAT(A) int A##r, int A##c, doublecomplex* A##p
#define KCVEC(A) int A##n, const doublecomplex*A##p
#define KCMAT(A) int A##r, int A##c, const doublecomplex* A##p

#define FVEC(A) int A##n, float*A##p
#define FMAT(A) int A##r, int A##c, float* A##p
#define KFVEC(A) int A##n, const float*A##p
#define KFMAT(A) int A##r, int A##c, const float* A##p

#define QVEC(A) int A##n, complex*A##p
#define QMAT(A) int A##r, int A##c, complex* A##p
#define KQVEC(A) int A##n, const complex*A##p
#define KQMAT(A) int A##r, int A##c, const complex* A##p

#define V(x) x##n,x##p

#include <string.h>
#include <math.h>
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

double vector_max(KRVEC(x)) {
    double r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]>r) {
            r = xp[k];
        }
    }
    return r;
}

double vector_min(KRVEC(x)) {
    double r = xp[0];
    int k;
    for (k = 1; k<xn; k++) {
        if(xp[k]<r) {
            r = xp[k];
        }
    }
    return r;
}

double vector_max_index(KRVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]>xp[0]) {
            r = k;
        }
    }
    return r;
}

double vector_min_index(KRVEC(x)) {
    int k, r = 0;
    for (k = 1; k<xn; k++) {
        if(xp[k]<xp[0]) {
            r = k;
        }
    }
    return r;
}
   
int toScalarR(int code, KRVEC(x), RVEC(r)) { 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarR");
    double res;
    integer one = 1;
    integer n = rn;
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
    integer n = rn;
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

int toScalarC(int code, KCVEC(x), RVEC(r)) { 
    REQUIRES(rn==1,BAD_SIZE);
    DEBUGMSG("toScalarC");
    double res;
    integer one = 1;
    integer n = rn;
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
    integer n = rn;
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
//        OP(10,gsl_asinh)
//        OP(11,gsl_acosh)
//        OP(12,gsl_atanh)
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
//        OP(10,gsl_asinh)
//        OP(11,gsl_acosh)
//        OP(12,gsl_atanh)
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



int mapC(int code, KCVEC(x), CVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapC");
    switch (code) {
/*
        OP(0,gsl_complex_sin)
        OP(1,gsl_complex_cos)
        OP(2,gsl_complex_tan)
*/
        OP(3,complex_abs_complex)
/*
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
*/
        OP(15,complex_signum_complex)
        
//      OP(16,gsl_complex_sqrt)
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

inline complex complex_f_math_op(doublecomplex (*cf)(doublecomplex,doublecomplex),
                                 complex a,complex b)
{
  doublecomplex c1,c2,r;

  complex float_r;

  c1.r = a.r;
  c1.i = a.i;

  c2.r = b.r;
  c2.i = b.i;

  r = (*cf)(c1,c2);

  float_r.r = r.r;
  float_r.i = r.i;

  return float_r;
}

#define OPC(C,F) case C: { for(k=0;k<xn;k++) rp[k] = complex_f_math_fun(&F,xp[k]); OK }
#define OPCA(C,F,A,B) case C: { for(k=0;k<xn;k++) rp[k] = complex_f_math_op(&F,A,B); OK }
int mapQ(int code, KQVEC(x), QVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapQ");
    switch (code) {
/*
        OPC(0,gsl_complex_sin)
        OPC(1,gsl_complex_cos)
        OPC(2,gsl_complex_tan)
*/
        OPC(3,complex_abs_complex)
/*
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
*/
        OPC(15,complex_signum_complex)
        
//      OPC(16,gsl_complex_sqrt)
        default: ERROR(BAD_CODE);
    }
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



inline doublecomplex complex_add(doublecomplex a, doublecomplex b) {
    doublecomplex r;
    r.r = a.r+b.r;
    r.i = a.i+b.i;
    return r;
}


int mapValC(int code, doublecomplex* pval, KCVEC(x), CVEC(r)) {
    int k;
    doublecomplex val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValC");
    switch (code) {
//        OPV(0,gsl_complex_mul(val,xp[k]))
//        OPV(1,gsl_complex_div(val,xp[k]))
        OPV(2,complex_add(val,xp[k]))
//        OPV(3,gsl_complex_sub(val,xp[k]))
//        OPV(4,gsl_complex_pow(val,xp[k]))
//        OPV(5,gsl_complex_pow(xp[k],val))
        default: ERROR(BAD_CODE);
    }
}


int mapValQ(int code, complex* pval, KQVEC(x), QVEC(r)) {
    int k;
    complex val = *pval;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapValQ");
    switch (code) {
//        OPCA(0,gsl_complex_mul,val,xp[k])
//        OPCA(1,gsl_complex_div,val,xp[k])
        OPCA(2,complex_add,val,xp[k])
//        OPCA(3,gsl_complex_sub,val,xp[k])
//        OPCA(4,gsl_complex_pow,val,xp[k])
//        OPCA(5,gsl_complex_pow,xp[k],val)
        default: ERROR(BAD_CODE);
    }
}



#define OPZE(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = E(ap[k],bp[k]); OK }
#define OPZV(C,msg,E) case C: {DEBUGMSG(msg) res = E(V(r),V(b)); CHECK(res,res); OK }
#define OPZO(C,msg,O) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = ap[k] O bp[k]; OK }

int zipR(int code, KRVEC(a), KRVEC(b), RVEC(r)) {
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




int zipC(int code, KCVEC(a), KCVEC(b), CVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPZE(0,"zipC Add",complex_add)
//        OPZE(1,"zipC Sub",gsl_complex_sub)
//        OPZE(2,"zipC Mul",gsl_complex_mul)
//        OPZE(3,"zipC Div",gsl_complex_div)
//        OPZE(4,"zipC Pow",gsl_complex_pow)
//        //OPZE(5,"zipR ATan2",atan2)
        default: ERROR(BAD_CODE);
    }
}





#define OPCZE(C,msg,E) case C: {DEBUGMSG(msg) for(k=0;k<an;k++) rp[k] = complex_f_math_op(&E,ap[k],bp[k]); OK }

int zipQ(int code, KQVEC(a), KQVEC(b), QVEC(r)) {
    REQUIRES(an == bn && an == rn, BAD_SIZE);
    int k;
    switch(code) {
        OPCZE(0,"zipQ Add",complex_add)
//        OPCZE(1,"zipQ Sub",gsl_complex_sub)
//        OPCZE(2,"zipQ Mul",gsl_complex_mul)
//        OPCZE(3,"zipQ Div",gsl_complex_div)
//        OPCZE(4,"zipQ Pow",gsl_complex_pow)
//        //OPZE(5,"zipR ATan2",atan2)
        default: ERROR(BAD_CODE);
    }
}




/*
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
*/


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


/*
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

*/

