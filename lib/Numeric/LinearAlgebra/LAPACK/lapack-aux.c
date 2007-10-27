#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "lapack-aux.h"

#define MACRO(B) do {B} while (0)
#define ERROR(CODE) MACRO(return CODE;)
#define REQUIRES(COND, CODE) MACRO(if(!(COND)) {ERROR(CODE);})

#define MIN(A,B) ((A)<(B)?(A):(B))
#define MAX(A,B) ((A)>(B)?(A):(B))
 
#ifdef DBGL
#define DEBUGMSG(M) printf("LAPACK Wrapper "M"\n: "); size_t t0 = time(NULL);
#define OK MACRO(printf("%ld s\n",time(0)-t0); return 0;);
#else
#define DEBUGMSG(M)
#define OK return 0;
#endif

#define CHECK(RES,CODE) MACRO(if(RES) return CODE;)

#define BAD_SIZE 2000
#define BAD_CODE 2001
#define MEM      2002
#define BAD_FILE 2003
#define SINGULAR 2004
#define NOCONVER 2005
#define NODEFPOS 2006
#define NOSPRTD  2007


//////////////////// real svd ////////////////////////////////////

int svd_l_R(KDMAT(a),DMAT(u), DVEC(s),DMAT(v)) {
    integer m = ar;
    integer n = ac;
    integer q = MIN(m,n);
    REQUIRES(ur==m && uc==m && sn==q && vr==n && vc==n,BAD_SIZE);
    DEBUGMSG("svd_l_R");
    double *B = (double*)malloc(m*n*sizeof(double));
    CHECK(!B,MEM);
    memcpy(B,ap,m*n*sizeof(double));
    integer lwork = -1;
    integer res;
    // ask for optimal lwork
    double ans;
    //printf("ask zgesvd\n");
    char* job = "A";
    dgesvd_ (job,job,
             &m,&n,B,&m,
             sp,
             up,&m,
             vp,&n,
             &ans, &lwork,
             &res);
    lwork = ceil(ans);
    //printf("ans = %d\n",lwork);
    double * work = (double*)malloc(lwork*sizeof(double));
    CHECK(!work,MEM);
    //printf("dgesdd\n");
    dgesvd_ (job,job,
             &m,&n,B,&m,
             sp,
             up,&m,
             vp,&n,
             work, &lwork,
             &res);
    CHECK(res,res);
    free(work);
    free(B);
    OK
}

// (alternative version)

int svd_l_Rdd(KDMAT(a),DMAT(u), DVEC(s),DMAT(v)) {
    integer m = ar;
    integer n = ac;
    integer q = MIN(m,n);
    REQUIRES(ur==m && uc==m && sn==q && vr==n && vc==n,BAD_SIZE);
    DEBUGMSG("svd_l_Rdd");
    double *B = (double*)malloc(m*n*sizeof(double));
    CHECK(!B,MEM);
    memcpy(B,ap,m*n*sizeof(double));
    integer* iwk = (integer*) malloc(8*q*sizeof(int));
    CHECK(!iwk,MEM);
    integer lwk = -1;
    integer res;
    // ask for optimal lwk
    double ans;
    //printf("ask dgesdd\n");
    dgesdd_ ("A",&m,&n,B,&m,sp,up,&m,vp,&n,&ans,&lwk,iwk,&res);
    lwk = 2*ceil(ans); // ????? otherwise 50x100 rejects lwk
    //printf("lwk = %d\n",lwk);
    double * workv = (double*)malloc(lwk*sizeof(double));
    CHECK(!workv,MEM);
    //printf("dgesdd\n");
    dgesdd_ ("A",&m,&n,B,&m,sp,up,&m,vp,&n,workv,&lwk,iwk,&res);
    CHECK(res,res);
    free(iwk);
    free(workv);
    free(B);
    OK
}

//////////////////// complex svd ////////////////////////////////////

// not in clapack.h

int zgesvd_(char *jobu, char *jobvt, integer *m, integer *n, 
    doublecomplex *a, integer *lda, doublereal *s, doublecomplex *u, 
    integer *ldu, doublecomplex *vt, integer *ldvt, doublecomplex *work, 
    integer *lwork, doublereal *rwork, integer *info);

int svd_l_C(KCMAT(a),CMAT(u), DVEC(s),CMAT(v)) {
    integer m = ar;
    integer n = ac;
    integer q = MIN(m,n);
    REQUIRES(ur==m && uc==m && sn==q && vr==n && vc==n,BAD_SIZE);
    DEBUGMSG("svd_l_C");
    double *B = (double*)malloc(2*m*n*sizeof(double));
    CHECK(!B,MEM);
    memcpy(B,ap,m*n*2*sizeof(double));

    double *rwork = (double*) malloc(5*q*sizeof(double));
    CHECK(!rwork,MEM);
    integer lwork = -1;
    integer res;
    // ask for optimal lwork
    doublecomplex ans;
    //printf("ask zgesvd\n");
    char* job = "A";
    zgesvd_ (job,job,
             &m,&n,(doublecomplex*)B,&m,
             sp,
             (doublecomplex*)up,&m,
             (doublecomplex*)vp,&n,
             &ans, &lwork,
             rwork,
             &res);
    lwork = ceil(ans.r);
    //printf("ans = %d\n",lwork);
    doublecomplex * work = (doublecomplex*)malloc(lwork*2*sizeof(double));
    CHECK(!work,MEM);
    //printf("zgesvd\n");
    zgesvd_ (job,job,
             &m,&n,(doublecomplex*)B,&m,
             sp,
             (doublecomplex*)up,&m,
             (doublecomplex*)vp,&n,
             work, &lwork,
             rwork,
             &res);
    CHECK(res,res);
    free(work);
    free(rwork);
    free(B);
    OK
}



//////////////////// general complex eigensystem ////////////

int eig_l_C(KCMAT(a),CMAT(u), CVEC(s),CMAT(v)) {
    integer n = ar;
    REQUIRES(n>=2 && ac==n && (ur==1 || (ur==n && uc==n)) && sn==n && (vr==1 || (vr==n && vc==n)),BAD_SIZE);
    DEBUGMSG("eig_l_C");
    double *B = (double*)malloc(2*n*n*sizeof(double));
    CHECK(!B,MEM);
    memcpy(B,ap,n*n*2*sizeof(double));

    double *rwork = (double*) malloc(2*n*sizeof(double));
    CHECK(!rwork,MEM);
    integer lwork = -1;
    char jobvl = ur==1?'N':'V';
    char jobvr = vr==1?'N':'V';
    integer res;
    // ask for optimal lwork
    doublecomplex ans;
    //printf("ask zgeev\n");
    zgeev_  (&jobvl,&jobvr,
             &n,(doublecomplex*)B,&n,
             (doublecomplex*)sp,
             (doublecomplex*)up,&n,
             (doublecomplex*)vp,&n,
             &ans, &lwork,
             rwork,
             &res);
    lwork = ceil(ans.r);
    //printf("ans = %d\n",lwork);
    doublecomplex * work = (doublecomplex*)malloc(lwork*2*sizeof(double));
    CHECK(!work,MEM);
    //printf("zgeev\n");
    zgeev_  (&jobvl,&jobvr,
             &n,(doublecomplex*)B,&n,
             (doublecomplex*)sp,
             (doublecomplex*)up,&n,
             (doublecomplex*)vp,&n,
             work, &lwork,
             rwork,
             &res);
    CHECK(res,res);
    free(work);
    free(rwork);
    free(B);
    OK
}



//////////////////// general real eigensystem ////////////

int eig_l_R(KDMAT(a),DMAT(u), CVEC(s),DMAT(v)) {
    integer n = ar;
    REQUIRES(n>=2 && ac == n && (ur==1 || (ur==n && uc==n)) && sn==n && (vr==1 || (vr==n && vc==n)),BAD_SIZE);
    DEBUGMSG("eig_l_R");
    double *B = (double*)malloc(n*n*sizeof(double));
    CHECK(!B,MEM);
    memcpy(B,ap,n*n*sizeof(double));
    integer lwork = -1;
    char jobvl = ur==1?'N':'V';
    char jobvr = vr==1?'N':'V';
    integer res;
    // ask for optimal lwork
    double ans;
    //printf("ask dgeev\n");
    dgeev_  (&jobvl,&jobvr,
             &n,B,&n,
             sp, sp+n,
             up,&n,
             vp,&n,
             &ans, &lwork,
             &res);
    lwork = ceil(ans);
    //printf("ans = %d\n",lwork);
    double * work = (double*)malloc(lwork*sizeof(double));
    CHECK(!work,MEM);
    //printf("dgeev\n");
    dgeev_  (&jobvl,&jobvr,
             &n,B,&n,
             sp, sp+n,
             up,&n,
             vp,&n,
             work, &lwork,
             &res);
    CHECK(res,res);
    free(work);
    free(B);
    OK
}


//////////////////// symmetric real eigensystem ////////////


int eig_l_S(KDMAT(a),DVEC(s),DMAT(v)) {
    integer n = ar;
    REQUIRES(n>=2 && ac == n && sn==n && (vr==1 || (vr==n && vc==n)),BAD_SIZE);
    DEBUGMSG("eig_l_S");
    memcpy(vp,ap,n*n*sizeof(double));
    integer lwork = -1;
    char jobz = vr==1?'N':'V';
    char uplo = 'U';
    integer res;
    // ask for optimal lwork
    double ans;
    //printf("ask dsyev\n");
    dsyev_  (&jobz,&uplo,
             &n,vp,&n,
             sp,
             &ans, &lwork,
             &res);
    lwork = ceil(ans);
    //printf("ans = %d\n",lwork);
    double * work = (double*)malloc(lwork*sizeof(double));
    CHECK(!work,MEM);
    dsyev_  (&jobz,&uplo,
             &n,vp,&n,
             sp,
             work, &lwork,
             &res);
    CHECK(res,res);
    free(work);
    OK
}

//////////////////// hermitian complex eigensystem ////////////

int eig_l_H(KCMAT(a),DVEC(s),CMAT(v)) {
    integer n = ar;
    REQUIRES(n>=2 && ac==n && sn==n && (vr==1 || (vr==n && vc==n)),BAD_SIZE);
    DEBUGMSG("eig_l_H");
    memcpy(vp,ap,2*n*n*sizeof(double));
    double *rwork = (double*) malloc((3*n-2)*sizeof(double));
    CHECK(!rwork,MEM);
    integer lwork = -1;
    char jobz = vr==1?'N':'V';
    char uplo = 'U';
    integer res;
    // ask for optimal lwork
    doublecomplex ans;
    //printf("ask zheev\n");
    zheev_  (&jobz,&uplo,
             &n,(doublecomplex*)vp,&n,
             sp,
             &ans, &lwork,
             rwork,
             &res);
    lwork = ceil(ans.r);
    //printf("ans = %d\n",lwork);
    doublecomplex * work = (doublecomplex*)malloc(lwork*2*sizeof(double));
    CHECK(!work,MEM);
    zheev_  (&jobz,&uplo,
             &n,(doublecomplex*)vp,&n,
             sp,
             work, &lwork,
             rwork,
             &res);
    CHECK(res,res);
    free(work);
    free(rwork);
    OK
}

//////////////////// general real linear system ////////////

int linearSolveR_l(KDMAT(a),KDMAT(b),DMAT(x)) {
    integer n = ar;
    integer nhrs = bc;
    REQUIRES(n>=1 && ar==ac && ar==br,BAD_SIZE);
    DEBUGMSG("linearSolveR_l");
    double*AC = (double*)malloc(n*n*sizeof(double));
    memcpy(AC,ap,n*n*sizeof(double));
    memcpy(xp,bp,n*nhrs*sizeof(double));
    integer * ipiv = (integer*)malloc(n*sizeof(integer));
    integer res;
    dgesv_  (&n,&nhrs,
             AC, &n,
             ipiv,
             xp, &n,
             &res);
    if(res>0) {
        return SINGULAR;
    }
    CHECK(res,res);
    free(ipiv);
    free(AC);
    OK
}

//////////////////// general complex linear system ////////////

int linearSolveC_l(KCMAT(a),KCMAT(b),CMAT(x)) {
    integer n = ar;
    integer nhrs = bc;
    REQUIRES(n>=1 && ar==ac && ar==br,BAD_SIZE);
    DEBUGMSG("linearSolveC_l");
    double*AC = (double*)malloc(2*n*n*sizeof(double));
    memcpy(AC,ap,2*n*n*sizeof(double));
    memcpy(xp,bp,2*n*nhrs*sizeof(double));
    integer * ipiv = (integer*)malloc(n*sizeof(integer));
    integer res;
    zgesv_  (&n,&nhrs,
             (doublecomplex*)AC, &n,
             ipiv,
             (doublecomplex*)xp, &n,
             &res);
    if(res>0) {
        return SINGULAR;
    }
    CHECK(res,res);
    free(ipiv);
    free(AC);
    OK
}

//////////////////// least squares real linear system ////////////

int linearSolveLSR_l(KDMAT(a),KDMAT(b),DMAT(x)) {
    integer m = ar;
    integer n = ac;
    integer nrhs = bc;
    integer ldb = xr;
    REQUIRES(m>=1 && n>=1 && ar==br && xr==MAX(m,n) && xc == bc, BAD_SIZE);
    DEBUGMSG("linearSolveLSR_l");
    double*AC = (double*)malloc(m*n*sizeof(double));
    memcpy(AC,ap,m*n*sizeof(double));
    if (m>=n) {
        memcpy(xp,bp,m*nrhs*sizeof(double));
    } else {
        int k;
        for(k = 0; k<nrhs; k++) {
            memcpy(xp+ldb*k,bp+m*k,m*sizeof(double));
        }
    }
    integer res;
    integer lwork = -1;
    double ans;
    dgels_  ("N",&m,&n,&nrhs,
             AC,&m,
             xp,&ldb,
             &ans,&lwork,
             &res);
    lwork = ceil(ans);
    //printf("ans = %d\n",lwork);
    double * work = (double*)malloc(lwork*sizeof(double));
    dgels_  ("N",&m,&n,&nrhs,
             AC,&m,
             xp,&ldb,
             work,&lwork,
             &res);
    if(res>0) {
        return SINGULAR;
    }
    CHECK(res,res);
    free(work);
    free(AC);
    OK
}

//////////////////// least squares complex linear system ////////////

int linearSolveLSC_l(KCMAT(a),KCMAT(b),CMAT(x)) {
    #ifdef _WIN32
    return NOSPRTD;
    #else
    integer m = ar;
    integer n = ac;
    integer nrhs = bc;
    integer ldb = xr;
    REQUIRES(m>=1 && n>=1 && ar==br && xr==MAX(m,n) && xc == bc, BAD_SIZE);
    DEBUGMSG("linearSolveLSC_l");
    double*AC = (double*)malloc(2*m*n*sizeof(double));
    memcpy(AC,ap,2*m*n*sizeof(double));
    memcpy(AC,ap,2*m*n*sizeof(double));
    if (m>=n) {
        memcpy(xp,bp,2*m*nrhs*sizeof(double));
    } else {
        int k;
        for(k = 0; k<nrhs; k++) {
            memcpy(xp+2*ldb*k,bp+2*m*k,m*2*sizeof(double));
        }
    }
    integer res;
    integer lwork = -1;
    doublecomplex ans;
    zgels_  ("N",&m,&n,&nrhs,
             (doublecomplex*)AC,&m,
             (doublecomplex*)xp,&ldb,
             &ans,&lwork,
             &res);
    lwork = ceil(ans.r);
    //printf("ans = %d\n",lwork);
    doublecomplex * work = (doublecomplex*)malloc(lwork*sizeof(doublecomplex));
    zgels_  ("N",&m,&n,&nrhs,
             (doublecomplex*)AC,&m,
             (doublecomplex*)xp,&ldb,
             work,&lwork,
             &res);
    if(res>0) {
        return SINGULAR;
    }
    CHECK(res,res);
    free(work);
    free(AC);
    OK
    #endif
}

//////////////////// least squares real linear system using SVD ////////////

int linearSolveSVDR_l(double rcond,KDMAT(a),KDMAT(b),DMAT(x)) {
    integer m = ar;
    integer n = ac;
    integer nrhs = bc;
    integer ldb = xr;
    REQUIRES(m>=1 && n>=1 && ar==br && xr==MAX(m,n) && xc == bc, BAD_SIZE);
    DEBUGMSG("linearSolveSVDR_l");
    double*AC = (double*)malloc(m*n*sizeof(double));
    double*S = (double*)malloc(MIN(m,n)*sizeof(double));
    memcpy(AC,ap,m*n*sizeof(double));
    if (m>=n) {
        memcpy(xp,bp,m*nrhs*sizeof(double));
    } else {
        int k;
        for(k = 0; k<nrhs; k++) {
            memcpy(xp+ldb*k,bp+m*k,m*sizeof(double));
        }
    }
    integer res;
    integer lwork = -1;
    integer rank;
    double ans;
    dgelss_  (&m,&n,&nrhs,
             AC,&m,
             xp,&ldb,
             S,
             &rcond,&rank,
             &ans,&lwork,
             &res);
    lwork = ceil(ans);
    //printf("ans = %d\n",lwork);
    double * work = (double*)malloc(lwork*sizeof(double));
    dgelss_  (&m,&n,&nrhs,
             AC,&m,
             xp,&ldb,
             S,
             &rcond,&rank,
             work,&lwork,
             &res);
    if(res>0) {
        return NOCONVER;
    }
    CHECK(res,res);
    free(work);
    free(S);
    free(AC);
    OK
}

//////////////////// least squares complex linear system using SVD ////////////

// not in clapack.h

int zgelss_(integer *m, integer *n, integer *nhrs,
    doublecomplex *a, integer *lda, doublecomplex *b, integer *ldb, doublereal *s,
    doublereal *rcond, integer* rank,
    doublecomplex *work, integer* lwork, doublereal* rwork,
    integer *info);

int linearSolveSVDC_l(double rcond, KCMAT(a),KCMAT(b),CMAT(x)) {
    #ifdef _WIN32
    return NOSPRTD;
    #else
    integer m = ar;
    integer n = ac;
    integer nrhs = bc;
    integer ldb = xr;
    REQUIRES(m>=1 && n>=1 && ar==br && xr==MAX(m,n) && xc == bc, BAD_SIZE);
    DEBUGMSG("linearSolveSVDC_l");
    double*AC = (double*)malloc(2*m*n*sizeof(double));
    double*S = (double*)malloc(MIN(m,n)*sizeof(double));
    double*RWORK = (double*)malloc(5*MIN(m,n)*sizeof(double));
    memcpy(AC,ap,2*m*n*sizeof(double));
    if (m>=n) {
        memcpy(xp,bp,2*m*nrhs*sizeof(double));
    } else {
        int k;
        for(k = 0; k<nrhs; k++) {
            memcpy(xp+2*ldb*k,bp+2*m*k,m*2*sizeof(double));
        }
    }
    integer res;
    integer lwork = -1;
    integer rank;
    doublecomplex ans;
    zgelss_  (&m,&n,&nrhs,
             (doublecomplex*)AC,&m,
             (doublecomplex*)xp,&ldb,
             S,
             &rcond,&rank,
             &ans,&lwork,
             RWORK,
             &res);
    lwork = ceil(ans.r);
    //printf("ans = %d\n",lwork);
    doublecomplex * work = (doublecomplex*)malloc(lwork*sizeof(doublecomplex));
    zgelss_  (&m,&n,&nrhs,
             (doublecomplex*)AC,&m,
             (doublecomplex*)xp,&ldb,
             S,
             &rcond,&rank,
             work,&lwork,
             RWORK,
             &res);
    if(res>0) {
        return NOCONVER;
    }
    CHECK(res,res);
    free(work);
    free(RWORK);
    free(S);
    free(AC);
    OK
    #endif
}

//////////////////// Cholesky factorization /////////////////////////

int chol_l_H(KCMAT(a),CMAT(l)) {
    integer n = ar;
    REQUIRES(n>=1 && ac == n && lr==n && lc==n,BAD_SIZE);
    DEBUGMSG("chol_l_H");
    memcpy(lp,ap,n*n*sizeof(doublecomplex));
    char uplo = 'U';
    integer res;
    zpotrf_ (&uplo,&n,(doublecomplex*)lp,&n,&res);
    CHECK(res>0,NODEFPOS);
    CHECK(res,res);
    doublecomplex zero = {0.,0.};
    int r,c;
    for (r=0; r<lr-1; r++) {
        for(c=r+1; c<lc; c++) {
            ((doublecomplex*)lp)[r*lc+c] = zero;
        }
    }
    OK
}

int chol_l_S(KDMAT(a),DMAT(l)) {
    integer n = ar;
    REQUIRES(n>=1 && ac == n && lr==n && lc==n,BAD_SIZE);
    DEBUGMSG("chol_l_S");
    memcpy(lp,ap,n*n*sizeof(double));
    char uplo = 'U';
    integer res;
    dpotrf_ (&uplo,&n,lp,&n,&res);
    CHECK(res>0,NODEFPOS);
    CHECK(res,res);
    int r,c;
    for (r=0; r<lr-1; r++) {
        for(c=r+1; c<lc; c++) {
            lp[r*lc+c] = 0.;
        }
    }
    OK
}

//////////////////// QR factorization /////////////////////////

int qr_l_R(KDMAT(a), DVEC(tau), DMAT(r)) {
    integer m = ar;
    integer n = ac;
    integer mn = MIN(m,n);
    REQUIRES(m>=1 && n >=1 && rr== m && rc == n && taun == mn, BAD_SIZE);
    DEBUGMSG("qr_l_R");
    double *WORK = (double*)malloc(n*sizeof(double));
    CHECK(!WORK,MEM);
    memcpy(rp,ap,m*n*sizeof(double));
    integer res;
    dgeqr2_ (&m,&n,rp,&m,taup,WORK,&res);
    CHECK(res,res);
    free(WORK);
    OK
}

int qr_l_C(KCMAT(a), CVEC(tau), CMAT(r)) {
    integer m = ar;
    integer n = ac;
    integer mn = MIN(m,n);
    REQUIRES(m>=1 && n >=1 && rr== m && rc == n && taun == mn, BAD_SIZE);
    DEBUGMSG("qr_l_C");
    doublecomplex *WORK = (doublecomplex*)malloc(n*sizeof(doublecomplex));
    CHECK(!WORK,MEM);
    memcpy(rp,ap,m*n*sizeof(doublecomplex));
    integer res;
    zgeqr2_ (&m,&n,(doublecomplex*)rp,&m,(doublecomplex*)taup,WORK,&res);
    CHECK(res,res);
    free(WORK);
    OK
}

//////////////////// Hessenberg factorization /////////////////////////

int hess_l_R(KDMAT(a), DVEC(tau), DMAT(r)) {
    integer m = ar;
    integer n = ac;
    integer mn = MIN(m,n);
    REQUIRES(m>=1 && n == m && rr== m && rc == n && taun == mn-1, BAD_SIZE);
    DEBUGMSG("hess_l_R");
    integer lwork = 5*n; // fixme
    double *WORK = (double*)malloc(lwork*sizeof(double));
    CHECK(!WORK,MEM);
    memcpy(rp,ap,m*n*sizeof(double));
    integer res;
    integer one = 1;
    dgehrd_ (&n,&one,&n,rp,&n,taup,WORK,&lwork,&res);
    CHECK(res,res);
    free(WORK);
    OK
}

int hess_l_C(KCMAT(a), CVEC(tau), CMAT(r)) {
    integer m = ar;
    integer n = ac;
    integer mn = MIN(m,n);
    REQUIRES(m>=1 && n == m && rr== m && rc == n && taun == mn-1, BAD_SIZE);
    DEBUGMSG("hess_l_C");
    integer lwork = 5*n; // fixme
    doublecomplex *WORK = (doublecomplex*)malloc(lwork*sizeof(doublecomplex));
    CHECK(!WORK,MEM);
    memcpy(rp,ap,m*n*sizeof(doublecomplex));
    integer res;
    integer one = 1;
    zgehrd_ (&n,&one,&n,(doublecomplex*)rp,&n,(doublecomplex*)taup,WORK,&lwork,&res);
    CHECK(res,res);
    free(WORK);
    OK
}

//////////////////// Schur factorization /////////////////////////

int schur_l_R(KDMAT(a), DMAT(u), DMAT(s)) {
    integer m = ar;
    integer n = ac;
    REQUIRES(m>=1 && n==m && ur==n && uc==n && sr==n && sc==n, BAD_SIZE);
    DEBUGMSG("schur_l_R");
    memcpy(sp,ap,n*n*sizeof(double));
    integer lwork = 6*n; // fixme
    double *WORK = (double*)malloc(lwork*sizeof(double));
    double *WR = (double*)malloc(n*sizeof(double));
    double *WI = (double*)malloc(n*sizeof(double));
    // WR and WI not really required in this call
    logical *BWORK = (logical*)malloc(n*sizeof(logical));
    integer res;
    integer sdim;
    dgees_ ("V","N",NULL,&n,sp,&n,&sdim,WR,WI,up,&n,WORK,&lwork,BWORK,&res);
    CHECK(res,res);
    free(WR);
    free(WI);
    free(BWORK);
    free(WORK);
    OK
}

int schur_l_C(KCMAT(a), CMAT(u), CMAT(s)) {
    #ifdef _WIN32
    return NOSPRTD;
    #else
    integer m = ar;
    integer n = ac;
    REQUIRES(m>=1 && n==m && ur==n && uc==n && sr==n && sc==n, BAD_SIZE);
    DEBUGMSG("schur_l_C");
    memcpy(sp,ap,n*n*sizeof(doublecomplex));
    integer lwork = 6*n; // fixme
    doublecomplex *WORK = (doublecomplex*)malloc(lwork*sizeof(doublecomplex));
    doublecomplex *W = (doublecomplex*)malloc(n*sizeof(doublecomplex));
    // W not really required in this call
    logical *BWORK = (logical*)malloc(n*sizeof(logical));
    double *RWORK = (double*)malloc(n*sizeof(double));
    integer res;
    integer sdim;
    zgees_ ("V","N",NULL,&n,(doublecomplex*)sp,&n,&sdim,W,
                            (doublecomplex*)up,&n,
                            WORK,&lwork,RWORK,BWORK,&res);
    CHECK(res,res);
    free(W);
    free(BWORK);
    free(WORK);
    OK
    #endif
}
