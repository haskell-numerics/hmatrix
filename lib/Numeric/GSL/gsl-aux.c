#include <gsl/gsl_complex.h>

#define RVEC(A) int A##n, double*A##p
#define RMAT(A) int A##r, int A##c, double* A##p
#define KRVEC(A) int A##n, const double*A##p
#define KRMAT(A) int A##r, int A##c, const double* A##p

#define CVEC(A) int A##n, gsl_complex*A##p
#define CMAT(A) int A##r, int A##c, gsl_complex* A##p
#define KCVEC(A) int A##n, const gsl_complex*A##p
#define KCMAT(A) int A##r, int A##c, const gsl_complex* A##p

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
#include <gsl/gsl_odeiv.h>
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

#define BAD_SIZE 2000
#define BAD_CODE 2001
#define MEM      2002
#define BAD_FILE 2003


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
        OPV(5,gsl_complex_pow(xp[k],val))
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
int minimize(int method, double f(int, double*), double tolsize, int maxit, 
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
    switch(method) {
        case 0 : {T = gsl_multimin_fminimizer_nmsimplex; break; }
#ifdef GSL110
        case 1 : {T = gsl_multimin_fminimizer_nmsimplex; break; }
#else
        case 1 : {T = gsl_multimin_fminimizer_nmsimplex2; break; }
#endif
        default: ERROR(BAD_CODE);
    }
    s = gsl_multimin_fminimizer_alloc (T, my_func.n);
    gsl_multimin_fminimizer_set (s, &my_func, V(xi), V(sz));
    do {
        status = gsl_multimin_fminimizer_iterate (s);
        size = gsl_multimin_fminimizer_size (s);

        solp[iter*solc+0] = iter+1;
        solp[iter*solc+1] = s->fval;
        solp[iter*solc+2] = size;

        int k;
        for(k=0;k<xin;k++) {
            solp[iter*solc+k+3] = gsl_vector_get(s->x,k);
        }
        iter++;
        if (status) break;
        status = gsl_multimin_test_size (size, tolsize);
    } while (status == GSL_CONTINUE && iter <= maxit);
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

typedef struct {double (*f)(int, double*); int (*df)(int, double*, int, double*);} Tfdf;

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
    double* q = (double*)calloc(g->size,sizeof(double));
    int k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }

    fdf->df(x->size,p,g->size,q);

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


int minimizeD(int method, double f(int, double*), int df(int, double*, int, double*),
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
    switch(method) {
        case 0 : {T = gsl_multimin_fdfminimizer_conjugate_fr; break; }
        case 1 : {T = gsl_multimin_fdfminimizer_conjugate_pr; break; }
        case 2 : {T = gsl_multimin_fdfminimizer_vector_bfgs; break; }
        case 3 : {T = gsl_multimin_fdfminimizer_vector_bfgs2; break; }
        case 4 : {T = gsl_multimin_fdfminimizer_steepest_descent; break; }
        default: ERROR(BAD_CODE);
    }
    s = gsl_multimin_fdfminimizer_alloc (T, my_func.n);
    gsl_multimin_fdfminimizer_set (s, &my_func, V(xi), initstep, minimpar);
    do {
        status = gsl_multimin_fdfminimizer_iterate (s);
        solp[iter*solc+0] = iter+1;
        solp[iter*solc+1] = s->f;
        int k;
        for(k=0;k<xin;k++) {
            solp[iter*solc+k+2] = gsl_vector_get(s->x,k);
        }
        iter++;
        if (status) break;
        status = gsl_multimin_test_gradient (s->gradient, tolgrad);
    } while (status == GSL_CONTINUE && iter <= maxit);
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

//---------------------------------------------------------------

typedef void TrawfunV(int, double*, int, double*);

int only_f_aux_root(const gsl_vector*x, void *pars, gsl_vector*y) {
    TrawfunV * f = (TrawfunV*) pars;
    double* p = (double*)calloc(x->size,sizeof(double));
    double* q = (double*)calloc(y->size,sizeof(double));
    int k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }
    f(x->size,p,y->size,q);
    for(k=0;k<y->size;k++) {
        gsl_vector_set(y,k,q[k]);
    }
    free(p);
    free(q);
    return 0; //hmmm
}

int root(int method, void f(int, double*, int, double*),
         double epsabs, int maxit,
         KRVEC(xi), RMAT(sol)) {
    REQUIRES(solr == maxit && solc == 1+2*xin,BAD_SIZE);
    DEBUGMSG("root_only_f");
    gsl_multiroot_function my_func;
    // extract function from pars
    my_func.f = only_f_aux_root;
    my_func.n = xin;
    my_func.params = f;
    size_t iter = 0;
    int status;
    const gsl_multiroot_fsolver_type *T;
    gsl_multiroot_fsolver *s;
    // Starting point
    KDVVIEW(xi);
    switch(method) {
        case 0 : {T = gsl_multiroot_fsolver_hybrids;; break; }
        case 1 : {T = gsl_multiroot_fsolver_hybrid; break; }
        case 2 : {T = gsl_multiroot_fsolver_dnewton; break; }
        case 3 : {T = gsl_multiroot_fsolver_broyden; break; }
        default: ERROR(BAD_CODE);
    }
    s = gsl_multiroot_fsolver_alloc (T, my_func.n);
    gsl_multiroot_fsolver_set (s, &my_func, V(xi));

    do {
           status = gsl_multiroot_fsolver_iterate (s);

           solp[iter*solc+0] = iter+1;

           int k;
           for(k=0;k<xin;k++) {
               solp[iter*solc+k+1] = gsl_vector_get(s->x,k);
           }
           for(k=xin;k<2*xin;k++) {
               solp[iter*solc+k+1] = gsl_vector_get(s->f,k-xin);
           }

           iter++;
           if (status)   /* check if solver is stuck */
             break;

           status =
             gsl_multiroot_test_residual (s->f, epsabs);
        }
        while (status == GSL_CONTINUE && iter <= maxit);

    int i,j;
    for (i=iter; i<solr; i++) {
        solp[i*solc+0] = iter;
        for(j=1;j<solc;j++) {
            solp[i*solc+j]=0.;
        }
    }
    gsl_multiroot_fsolver_free(s);
    OK
}

// working with the jacobian

typedef struct {int (*f)(int, double*, int, double *); int (*jf)(int, double*, int, int, double*);} Tfjf;

int f_aux_root(const gsl_vector*x, void *pars, gsl_vector*y) {
    Tfjf * fjf = ((Tfjf*) pars);
    double* p = (double*)calloc(x->size,sizeof(double));
    double* q = (double*)calloc(y->size,sizeof(double));
    int k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }
    (fjf->f)(x->size,p,y->size,q);
    for(k=0;k<y->size;k++) {
        gsl_vector_set(y,k,q[k]);
    }
    free(p);
    free(q);
    return 0;
}

int jf_aux_root(const gsl_vector * x, void * pars, gsl_matrix * jac) {
    Tfjf * fjf = ((Tfjf*) pars);
    double* p = (double*)calloc(x->size,sizeof(double));
    double* q = (double*)calloc((x->size)*(x->size),sizeof(double));
    int i,j,k;
    for(k=0;k<x->size;k++) {
        p[k] = gsl_vector_get(x,k);
    }

    (fjf->jf)(x->size,p,x->size,x->size,q);

    k=0;
    for(i=0;i<x->size;i++) {
        for(j=0;j<x->size;j++){
            gsl_matrix_set(jac,i,j,q[k++]);
        }
    }
    free(p);
    free(q);
    return 0;
}

int fjf_aux_root(const gsl_vector * x, void * pars, gsl_vector * f, gsl_matrix * g) {
    f_aux_root(x,pars,f);
    jf_aux_root(x,pars,g);
    return 0;
}

int rootj(int method, int f(int, double*, int, double*),
                      int jac(int, double*, int, int, double*),
         double epsabs, int maxit,
         KRVEC(xi), RMAT(sol)) {
    REQUIRES(solr == maxit && solc == 1+2*xin,BAD_SIZE);
    DEBUGMSG("root_fjf");
    gsl_multiroot_function_fdf my_func;
    // extract function from pars
    my_func.f = f_aux_root;
    my_func.df = jf_aux_root;
    my_func.fdf = fjf_aux_root;
    my_func.n = xin;
    Tfjf stfjf;
    stfjf.f = f;
    stfjf.jf = jac;
    my_func.params = &stfjf;
    size_t iter = 0;
    int status;
    const gsl_multiroot_fdfsolver_type *T;
    gsl_multiroot_fdfsolver *s;
    // Starting point
    KDVVIEW(xi);
    switch(method) {
        case 0 : {T = gsl_multiroot_fdfsolver_hybridsj;; break; }
        case 1 : {T = gsl_multiroot_fdfsolver_hybridj; break; }
        case 2 : {T = gsl_multiroot_fdfsolver_newton; break; }
        case 3 : {T = gsl_multiroot_fdfsolver_gnewton; break; }
        default: ERROR(BAD_CODE);
    }
    s = gsl_multiroot_fdfsolver_alloc (T, my_func.n);

    gsl_multiroot_fdfsolver_set (s, &my_func, V(xi));

    do {
           status = gsl_multiroot_fdfsolver_iterate (s);

           solp[iter*solc+0] = iter+1;

           int k;
           for(k=0;k<xin;k++) {
               solp[iter*solc+k+1] = gsl_vector_get(s->x,k);
           }
           for(k=xin;k<2*xin;k++) {
               solp[iter*solc+k+1] = gsl_vector_get(s->f,k-xin);
           }

           iter++;
           if (status)   /* check if solver is stuck */
             break;

           status =
             gsl_multiroot_test_residual (s->f, epsabs);
        }
        while (status == GSL_CONTINUE && iter <= maxit);

    int i,j;
    for (i=iter; i<solr; i++) {
        solp[i*solc+0] = iter;
        for(j=1;j<solc;j++) {
            solp[i*solc+j]=0.;
        }
    }
    gsl_multiroot_fdfsolver_free(s);
    OK
}

//////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////
//                        ODE

typedef struct {int n; int (*f)(double,int, const double*, int, double *); int (*j)(double,int, const double*, int, int, double*);} Tode;

int odefunc (double t, const double y[], double f[], void *params) { 
    Tode * P = (Tode*) params;
    (P->f)(t,P->n,y,P->n,f);
    return GSL_SUCCESS;
}

int odejac (double t, const double y[], double *dfdy, double dfdt[], void *params) {
     Tode * P = ((Tode*) params);
     (P->j)(t,P->n,y,P->n,P->n,dfdy);
     int j;
     for (j=0; j< P->n; j++)
        dfdt[j] = 0.0;
     return GSL_SUCCESS;
}


int ode(int method, double h, double eps_abs, double eps_rel,
        int f(double, int, const double*, int, double*),
        int jac(double, int, const double*, int, int, double*),
        KRVEC(xi), KRVEC(ts), RMAT(sol)) {

    const gsl_odeiv_step_type * T;

    switch(method) {
        case 0 : {T = gsl_odeiv_step_rk2; break; }
        case 1 : {T = gsl_odeiv_step_rk4; break; }
        case 2 : {T = gsl_odeiv_step_rkf45; break; }
        case 3 : {T = gsl_odeiv_step_rkck; break; }
        case 4 : {T = gsl_odeiv_step_rk8pd; break; }
        case 5 : {T = gsl_odeiv_step_rk2imp; break; }
        case 6 : {T = gsl_odeiv_step_rk4imp; break; }
        case 7 : {T = gsl_odeiv_step_bsimp; break; }
        case 8 : {T = gsl_odeiv_step_gear1; break; }
        case 9 : {T = gsl_odeiv_step_gear2; break; }
        default: ERROR(BAD_CODE);
    }


    gsl_odeiv_step * s = gsl_odeiv_step_alloc (T, xin);
    gsl_odeiv_control * c = gsl_odeiv_control_y_new (eps_abs, eps_rel);
    gsl_odeiv_evolve * e = gsl_odeiv_evolve_alloc (xin);

    Tode P;
    P.f = f;
    P.j = jac;
    P.n = xin;

    gsl_odeiv_system sys = {odefunc, odejac, xin, &P};

    double t = tsp[0];

    double* y = (double*)calloc(xin,sizeof(double));
    int i,j;
    for(i=0; i< xin; i++) {
        y[i] = xip[i];
        solp[i] = xip[i];
    }

       for (i = 1; i < tsn ; i++)
         {
           double ti = tsp[i];
           while (t < ti)
             {
               gsl_odeiv_evolve_apply (e, c, s,
                                       &sys,
                                       &t, ti, &h,
                                       y);
               // if (h < hmin) h = hmin;
             }
           for(j=0; j<xin; j++) {
               solp[i*xin + j] = y[j];
           }
         }

    free(y);
    gsl_odeiv_evolve_free (e);
    gsl_odeiv_control_free (c);
    gsl_odeiv_step_free (s);
    return 0;
}
