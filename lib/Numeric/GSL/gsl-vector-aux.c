#include <gsl/gsl_blas.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <string.h>
#include <stdio.h>

#define MACRO(B) do {B} while (0)
#define ERROR(CODE) MACRO(return CODE;)
#define REQUIRES(COND, CODE) MACRO(if(!(COND)) {ERROR(CODE);})
#define OK return 0;
#define CHECK(RES,CODE) MACRO(if(RES) return CODE;)

#ifdef DBG
#define DEBUGMSG(M) printf("*** calling aux C function: %s\n",M);
#else
#define DEBUGMSG(M)
#endif

#define RVEC(A) int A##n, double*A##p
#define DVVIEW(A) gsl_vector_view A = gsl_vector_view_array(A##p,A##n)
#define V(a) (&a.vector)

#define BAD_SIZE 2000
#define BAD_CODE 2001
#define MEM      2002
#define BAD_FILE 2003


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

