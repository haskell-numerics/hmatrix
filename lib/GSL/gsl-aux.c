#include "gsl-aux.h"
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_fft_complex.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_deriv.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>
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

#define V(a) (&a.vector)
#define M(a) (&a.matrix)

#define GCVEC(A) int A##n, gsl_complex*A##p
#define KGCVEC(A) int A##n, const gsl_complex*A##p

#define BAD_SIZE 1000
#define BAD_CODE 1001
#define MEM      1002
#define BAD_FILE 1003


void no_abort_on_error() {
    gsl_set_error_handler_off();
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


inline double sign(double x) {
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

int mapCAux(int code, KGCVEC(x), GCVEC(r)) {
    int k;
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("mapC");
    switch (code) {
        OP(0,gsl_complex_sin)
        OP(1,gsl_complex_cos)
        OP(2,gsl_complex_tan)

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

        OP(16,gsl_complex_sqrt)

        // gsl_complex_arg
        // gsl_complex_abs
        default: ERROR(BAD_CODE);
    }
}

int mapC(int code, KCVEC(x), CVEC(r)) {
    return mapCAux(code, xn, (gsl_complex*)xp, rn, (gsl_complex*)rp);
}


/*
int scaleR(double* alpha, KRVEC(x), RVEC(r)) {
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("scaleR");
    KDVVIEW(x);
    DVVIEW(r);
    CHECK( gsl_vector_memcpy(V(r),V(x)) , MEM);
    int res = gsl_vector_scale(V(r),*alpha);
    CHECK(res,res);
    OK
}

int scaleC(gsl_complex *alpha, KCVEC(x), CVEC(r)) {
    REQUIRES(xn == rn,BAD_SIZE);
    DEBUGMSG("scaleC");
    //KCVVIEW(x);
    CVVIEW(r);
    gsl_vector_const_view vrx = gsl_vector_const_view_array((double*)xp,xn*2);
    gsl_vector_view vrr = gsl_vector_view_array((double*)rp,rn*2);
    CHECK(gsl_vector_memcpy(V(vrr),V(vrx)) , MEM);
    gsl_blas_zscal(*alpha,V(r)); // void !
    int res = 0; 
    CHECK(res,res);
    OK
}

int addConstantR(double offs, KRVEC(x), RVEC(r)) { 
    REQUIRES(xn == rn,BAD_SIZE); 
    DEBUGMSG("addConstantR");
    KDVVIEW(x);
    DVVIEW(r);
    CHECK(gsl_vector_memcpy(V(r),V(x)), MEM);
    int res = gsl_vector_add_constant(V(r),offs);
    CHECK(res,res);
    OK
}

*/



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
        OPV(5,gsl_complex_pow(val,xp[k]))
        default: ERROR(BAD_CODE);
    }
}

int mapValC(int code, gsl_complex* val, KCVEC(x), CVEC(r)) {
    return mapValCAux(code, val, xn, (gsl_complex*)xp, rn, (gsl_complex*)rp);
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




int luSolveR(KRMAT(a),KRMAT(b),RMAT(r)) {
    REQUIRES(ar==ac && ac==br && ar==rr && bc==rc,BAD_SIZE);
    DEBUGMSG("luSolveR");
    KDMVIEW(a);
    KDMVIEW(b);
    DMVIEW(r);
    int res;
    gsl_matrix *LU = gsl_matrix_alloc(ar,ar);
    CHECK(!LU,MEM);
    int s;
    gsl_permutation * p = gsl_permutation_alloc (ar);
    CHECK(!p,MEM);
    CHECK(gsl_matrix_memcpy(LU,M(a)),MEM);
    res = gsl_linalg_LU_decomp(LU, p, &s);
    CHECK(res,res);
    int c;

    for (c=0; c<bc; c++) {
        gsl_vector_const_view colb = gsl_matrix_const_column (M(b), c);
        gsl_vector_view colr = gsl_matrix_column (M(r), c);
        res = gsl_linalg_LU_solve (LU, p, V(colb), V(colr));
        CHECK(res,res);
    }
    gsl_permutation_free(p);
    gsl_matrix_free(LU);
    OK
}


int luSolveC(KCMAT(a),KCMAT(b),CMAT(r)) {
    REQUIRES(ar==ac && ac==br && ar==rr && bc==rc,BAD_SIZE);
    DEBUGMSG("luSolveC");
    KCMVIEW(a);
    KCMVIEW(b);
    CMVIEW(r);
    gsl_matrix_complex *LU = gsl_matrix_complex_alloc(ar,ar);
    CHECK(!LU,MEM);
    int s;
    gsl_permutation * p = gsl_permutation_alloc (ar);
    CHECK(!p,MEM);
    CHECK(gsl_matrix_complex_memcpy(LU,M(a)),MEM);
    int res;
    res = gsl_linalg_complex_LU_decomp(LU, p, &s);
    CHECK(res,res);
    int c;
    for (c=0; c<bc; c++) {
        gsl_vector_complex_const_view colb = gsl_matrix_complex_const_column (M(b), c);
        gsl_vector_complex_view colr = gsl_matrix_complex_column (M(r), c);
        res = gsl_linalg_complex_LU_solve (LU, p, V(colb), V(colr));
        CHECK(res,res);
    }
    gsl_permutation_free(p);
    gsl_matrix_complex_free(LU);
    OK
}


int luRaux(KRMAT(a),RVEC(b)) {
    REQUIRES(ar==ac && bn==ar*ar+ar+1,BAD_SIZE);
    DEBUGMSG("luRaux");
    KDMVIEW(a);
    //DVVIEW(b);
    gsl_matrix_view LU = gsl_matrix_view_array(bp,ar,ac);
    int s;
    gsl_permutation * p = gsl_permutation_alloc (ar);
    CHECK(!p,MEM);
    CHECK(gsl_matrix_memcpy(M(LU),M(a)),MEM);
    gsl_linalg_LU_decomp(M(LU), p, &s);
    bp[ar*ar] = s;
    int k;
    for (k=0; k<ar; k++) {
        bp[ar*ar+k+1] = gsl_permutation_get(p,k);
    }
    gsl_permutation_free(p);
    OK
}

int luCaux(KCMAT(a),CVEC(b)) {
    REQUIRES(ar==ac && bn==ar*ar+ar+1,BAD_SIZE);
    DEBUGMSG("luCaux");
    KCMVIEW(a);
    //DVVIEW(b);
    gsl_matrix_complex_view LU = gsl_matrix_complex_view_array((double*)bp,ar,ac);
    int s;
    gsl_permutation * p = gsl_permutation_alloc (ar);
    CHECK(!p,MEM);
    CHECK(gsl_matrix_complex_memcpy(M(LU),M(a)),MEM);
    int res;
    res = gsl_linalg_complex_LU_decomp(M(LU), p, &s);
    CHECK(res,res);
    ((double*)bp)[2*ar*ar] = s;
    ((double*)bp)[2*ar*ar+1] = 0;
    int k;
    for (k=0; k<ar; k++) {
        ((double*)bp)[2*ar*ar+2*k+2] = gsl_permutation_get(p,k);
        ((double*)bp)[2*ar*ar+2*k+2+1] = 0;
    }
    gsl_permutation_free(p);
    OK
}

int svd(KRMAT(a),RMAT(u), RVEC(s),RMAT(v)) {
    REQUIRES(ar==ur && ac==uc && ac==sn && ac==vr && ac==vc,BAD_SIZE);
    DEBUGMSG("svd");
    KDMVIEW(a);
    DMVIEW(u);
    DVVIEW(s);
    DMVIEW(v);
    gsl_vector  *workv = gsl_vector_alloc(ac);
    CHECK(!workv,MEM);
    gsl_matrix  *workm = gsl_matrix_alloc(ac,ac);
    CHECK(!workm,MEM);
    CHECK(gsl_matrix_memcpy(M(u),M(a)),MEM);
    // int res = gsl_linalg_SV_decomp_jacobi (&U.matrix, &V.matrix, &S.vector);
    // doesn't work 
    //int res = gsl_linalg_SV_decomp (&U.matrix, &V.matrix, &S.vector, workv);
    int res = gsl_linalg_SV_decomp_mod (M(u), workm, M(v), V(s), workv);
    CHECK(res,res);
    //gsl_matrix_transpose(M(v));
    gsl_vector_free(workv);
    gsl_matrix_free(workm);
    OK
}


// for real symmetric matrices
int eigensystemR(KRMAT(x),RVEC(l),RMAT(v)) {
    REQUIRES(xr==xc && xr==ln && xr==vr && vr==vc,BAD_SIZE);
    DEBUGMSG("eigensystemR (gsl_eigen_symmv)");
    KDMVIEW(x);
    DVVIEW(l);
    DMVIEW(v);
    gsl_matrix *XC = gsl_matrix_alloc(xr,xr);
    gsl_matrix_memcpy(XC,M(x)); // needed because the argument is destroyed
                                // many thanks to Nico Mahlo for the bug report 
    gsl_eigen_symmv_workspace * w = gsl_eigen_symmv_alloc (xc);
    int res = gsl_eigen_symmv (XC, V(l), M(v), w);
    CHECK(res,res);
    gsl_eigen_symmv_free (w);
    gsl_matrix_free(XC);
    gsl_eigen_symmv_sort (V(l), M(v), GSL_EIGEN_SORT_ABS_DESC);
    OK
}

// for hermitian matrices
int eigensystemC(KCMAT(x),RVEC(l),CMAT(v)) {
    REQUIRES(xr==xc && xr==ln && xr==vr && vr==vc,BAD_SIZE);
    DEBUGMSG("eigensystemC");
    KCMVIEW(x);
    DVVIEW(l);
    CMVIEW(v);
    gsl_matrix_complex *XC = gsl_matrix_complex_alloc(xr,xr);
    gsl_matrix_complex_memcpy(XC,M(x)); // again needed because the argument is destroyed
    gsl_eigen_hermv_workspace * w = gsl_eigen_hermv_alloc (xc);
    int res = gsl_eigen_hermv (XC, V(l), M(v), w);
    CHECK(res,res);
    gsl_eigen_hermv_free (w);
    gsl_matrix_complex_free(XC);
    gsl_eigen_hermv_sort (V(l), M(v), GSL_EIGEN_SORT_ABS_DESC);
    OK
}

int QR(KRMAT(x),RMAT(q),RMAT(r)) {
    REQUIRES(xr==rr && xc==rc && qr==qc && xr==qr,BAD_SIZE);
    DEBUGMSG("QR");
    KDMVIEW(x);
    DMVIEW(q);
    DMVIEW(r);
    gsl_matrix * a = gsl_matrix_alloc(xr,xc);
    gsl_vector * tau = gsl_vector_alloc(MIN(xr,xc));
    gsl_matrix_memcpy(a,M(x));
    int res = gsl_linalg_QR_decomp(a,tau);
    CHECK(res,res);
    gsl_linalg_QR_unpack(a,tau,M(q),M(r));
    gsl_vector_free(tau);
    gsl_matrix_free(a);
    OK
}

int chol(KRMAT(x),RMAT(l)) {
    REQUIRES(xr==xc && lr==xr && lr==lc,BAD_SIZE);
    DEBUGMSG("chol");
    KDMVIEW(x);
    DMVIEW(l);
    gsl_matrix_memcpy(M(l),M(x));
    int res = gsl_linalg_cholesky_decomp(M(l));
    CHECK(res,res);
    int r,c;
    for (r=0; r<xr-1; r++) {
        for(c=r+1; c<xc; c++) {
            lp[r*lc+c] = 0.;
        }
    }
    OK
}


int fft(int code, KCVEC(X), CVEC(R)) {
    REQUIRES(Xn == Rn,BAD_SIZE);
    DEBUGMSG("fft");
    int s = Xn;
    gsl_fft_complex_wavetable * wavetable = gsl_fft_complex_wavetable_alloc (s);
    gsl_fft_complex_workspace * workspace = gsl_fft_complex_workspace_alloc (s);
    gsl_vector_const_view X = gsl_vector_const_view_array((double*)Xp, 2*Xn);
    gsl_vector_view R = gsl_vector_view_array((double*)Rp, 2*Rn);
    gsl_blas_dcopy(&X.vector,&R.vector);
    if(code==0) {
        gsl_fft_complex_forward ((double*)Rp, 1, s, wavetable, workspace);
    } else {
        gsl_fft_complex_inverse ((double*)Rp, 1, s, wavetable, workspace);
    }
    gsl_fft_complex_wavetable_free (wavetable);
    gsl_fft_complex_workspace_free (workspace);
    OK
}


int integrate_qng(double f(double, void*), double a, double b, double prec,
                   double *result, double*error) {
    DEBUGMSG("integrate_qng");
    gsl_function F;
    F.function = f;
    F.params = NULL;
    size_t neval;
    int res = gsl_integration_qng (&F, a,b, 0, prec, result, error, &neval); 
    CHECK(res,res);
    OK
}

int integrate_qags(double f(double,void*), double a, double b, double prec, int w, 
               double *result, double* error) {
    DEBUGMSG("integrate_qags");
    gsl_integration_workspace * wk = gsl_integration_workspace_alloc (w);
    gsl_function F;
    F.function = f;
    F.params = NULL;
    int res = gsl_integration_qags (&F, a,b, 0, prec, w,wk, result, error); 
    CHECK(res,res);
    gsl_integration_workspace_free (wk); 
    OK
}

int polySolve(KRVEC(a), CVEC(z)) {
    DEBUGMSG("polySolve");
    REQUIRES(an>1,BAD_SIZE);
    gsl_poly_complex_workspace * w = gsl_poly_complex_workspace_alloc (an);
    int res = gsl_poly_complex_solve ((double*)ap, an, w, (double*)zp);
    CHECK(res,res);
    gsl_poly_complex_workspace_free (w);
    OK;
}

int matrix_fscanf(char*filename, RMAT(a)) {
    DEBUGMSG("gsl_matrix_fscanf");
    //printf(filename); printf("\n");
    DMVIEW(a);
    FILE * f = fopen(filename,"r");
    CHECK(!f,BAD_FILE);
    int res = gsl_matrix_fscanf(f, M(a));
    CHECK(res,res);
    fclose (f);
    OK
}

//---------------------------------------------------------------

typedef double Trawfun(int, double*);

double only_f_aux_min(const gsl_vector*x, void *pars) {
    Trawfun * f = (Trawfun*) pars;  
    double* p = (double*)calloc(x->size,sizeof(double));
    int k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }  
    double res = f(x->size,p);
    free(p);
    return res;
}

// this version returns info about intermediate steps
int minimize(double f(int, double*), double tolsize, int maxit, 
                 KRVEC(xi), KRVEC(sz), RMAT(sol)) {
    REQUIRES(xin==szn && solr == maxit && solc == 3+xin,BAD_SIZE);
    DEBUGMSG("minimizeList (nmsimplex)");
    gsl_multimin_function my_func;
    // extract function from pars
    my_func.f = only_f_aux_min;
    my_func.n = xin; 
    my_func.params = f;
    size_t iter = 0;
    int status;
    double size;
    const gsl_multimin_fminimizer_type *T;
    gsl_multimin_fminimizer *s = NULL;
    // Initial vertex size vector 
    KDVVIEW(sz);
    // Starting point
    KDVVIEW(xi);
    // Minimizer nmsimplex, without derivatives
    T = gsl_multimin_fminimizer_nmsimplex;
    s = gsl_multimin_fminimizer_alloc (T, my_func.n);
    gsl_multimin_fminimizer_set (s, &my_func, V(xi), V(sz));
    do {
        status = gsl_multimin_fminimizer_iterate (s);
        size = gsl_multimin_fminimizer_size (s);

        solp[iter*solc+0] = iter;
        solp[iter*solc+1] = s->fval;
        solp[iter*solc+2] = size;

        int k;
        for(k=0;k<xin;k++) {
            solp[iter*solc+k+3] = gsl_vector_get(s->x,k);
        }
        status = gsl_multimin_test_size (size, tolsize);
        iter++;
    } while (status == GSL_CONTINUE && iter < maxit);
    int i,j;
    for (i=iter; i<solr; i++) {
        solp[i*solc+0] = iter;
        for(j=1;j<solc;j++) {
            solp[i*solc+j]=0.;
        }
    }
    gsl_multimin_fminimizer_free(s);
    OK
}

// working with the gradient

typedef struct {double (*f)(int, double*); void (*df)(int, double*, double*);} Tfdf;

double f_aux_min(const gsl_vector*x, void *pars) {
    Tfdf * fdf = ((Tfdf*) pars);
    double* p = (double*)calloc(x->size,sizeof(double));
    int k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }
    double res = fdf->f(x->size,p);
    free(p);
    return res;
}


void df_aux_min(const gsl_vector * x, void * pars, gsl_vector * g) {
    Tfdf * fdf = ((Tfdf*) pars);  
    double* p = (double*)calloc(x->size,sizeof(double));
    double* q = (double*)calloc(x->size,sizeof(double));
    int k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }

    fdf->df(x->size,p,q);

    for(k=0;k<x->size;k++) {
        gsl_vector_set(g,k,q[k]);
    }
    free(p);
    free(q);
}

void fdf_aux_min(const gsl_vector * x, void * pars, double * f, gsl_vector * g) {
    *f = f_aux_min(x,pars);
    df_aux_min(x,pars,g);
}

// conjugate gradient
int minimizeWithDeriv(double f(int, double*), void df(int, double*, double*), 
                      double initstep, double minimpar, double tolgrad, int maxit, 
                      KRVEC(xi), RMAT(sol)) {
    REQUIRES(solr == maxit && solc == 2+xin,BAD_SIZE);
    DEBUGMSG("minimizeWithDeriv (conjugate_fr)");
    gsl_multimin_function_fdf my_func;
    // extract function from pars
    my_func.f = f_aux_min;
    my_func.df = df_aux_min;
    my_func.fdf = fdf_aux_min;
    my_func.n = xin; 
    Tfdf stfdf;
    stfdf.f = f;
    stfdf.df = df;
    my_func.params = &stfdf;
    size_t iter = 0;
    int status;
    const gsl_multimin_fdfminimizer_type *T;
    gsl_multimin_fdfminimizer *s = NULL;
    // Starting point
    KDVVIEW(xi);
    // conjugate gradient fr
    T = gsl_multimin_fdfminimizer_conjugate_fr;
    s = gsl_multimin_fdfminimizer_alloc (T, my_func.n);
    gsl_multimin_fdfminimizer_set (s, &my_func, V(xi), initstep, minimpar);
    do {
        status = gsl_multimin_fdfminimizer_iterate (s);
        solp[iter*solc+0] = iter;
        solp[iter*solc+1] = s->f;
        int k;
        for(k=0;k<xin;k++) {
            solp[iter*solc+k+2] = gsl_vector_get(s->x,k);
        }
        status = gsl_multimin_test_gradient (s->gradient, tolgrad);
        iter++;
    } while (status == GSL_CONTINUE && iter < maxit);
    int i,j;
    for (i=iter; i<solr; i++) {
        solp[i*solc+0] = iter;
        for(j=1;j<solc;j++) {
            solp[i*solc+j]=0.;
        }
    }
    gsl_multimin_fdfminimizer_free(s);
    OK
}


int deriv(int code, double f(double, void*), double x, double h, double * result, double * abserr)
{
    gsl_function F;
    F.function = f;
    F.params = 0;

    if(code==0) return gsl_deriv_central (&F, x, h, result, abserr);

    if(code==1) return gsl_deriv_forward (&F, x, h, result, abserr);

    if(code==2) return gsl_deriv_backward (&F, x, h, result, abserr);

    return 0;
}
