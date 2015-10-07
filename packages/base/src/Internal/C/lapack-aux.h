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

#define IVEC(A) int A##n, int*A##p
#define LVEC(A) int A##n, int64_t*A##p
#define FVEC(A) int A##n, float*A##p
#define DVEC(A) int A##n, double*A##p
#define QVEC(A) int A##n, complex*A##p
#define CVEC(A) int A##n, doublecomplex*A##p
#define PVEC(A) int A##n, void* A##p, int A##s

#define IMAT(A) int A##r, int A##c, int* A##p
#define LMAT(A) int A##r, int A##c, int64_t* A##p
#define FMAT(A) int A##r, int A##c, float* A##p
#define DMAT(A) int A##r, int A##c, double* A##p
#define QMAT(A) int A##r, int A##c, complex* A##p
#define CMAT(A) int A##r, int A##c, doublecomplex* A##p
#define PMAT(A) int A##r, int A##c, void* A##p, int A##s

#define KIVEC(A) int A##n, const int*A##p
#define KLVEC(A) int A##n, const int64_t*A##p
#define KFVEC(A) int A##n, const float*A##p
#define KDVEC(A) int A##n, const double*A##p
#define KQVEC(A) int A##n, const complex*A##p
#define KCVEC(A) int A##n, const doublecomplex*A##p
#define KPVEC(A) int A##n, const void* A##p, int A##s

#define KIMAT(A) int A##r, int A##c, const int* A##p
#define KLMAT(A) int A##r, int A##c, const int64_t* A##p
#define KFMAT(A) int A##r, int A##c, const float* A##p
#define KDMAT(A) int A##r, int A##c, const double* A##p
#define KQMAT(A) int A##r, int A##c, const complex* A##p
#define KCMAT(A) int A##r, int A##c, const doublecomplex* A##p
#define KPMAT(A) int A##r, int A##c, const void* A##p, int A##s

#define VECG(T,A) int A##n, T* A##p
#define MATG(T,A) int A##r, int A##c, int A##Xr, int A##Xc, T* A##p

#define OIMAT(A) MATG(int,A)
#define OLMAT(A) MATG(int64_t,A)
#define OFMAT(A) MATG(float,A)
#define ODMAT(A) MATG(double,A)
#define OQMAT(A) MATG(complex,A)
#define OCMAT(A) MATG(doublecomplex,A)

#define KOIMAT(A) MATG(const int,A)
#define KOLMAT(A) MATG(const int64_t,A)
#define KOFMAT(A) MATG(const float,A)
#define KODMAT(A) MATG(const double,A)
#define KOQMAT(A) MATG(const complex,A)
#define KOCMAT(A) MATG(const doublecomplex,A)

#define AT(m,i,j) (m##p[(i)*m##Xr + (j)*m##Xc])
#define TRAV(m,i,j) int i,j; for (i=0;i<m##r;i++) for (j=0;j<m##c;j++)

/********************************************************/

static inline
int mod (int a, int b) {
    int m = a % b;
    if (b>0) {
        return m >=0 ? m : m+b;
    } else {
        return m <=0 ? m : m+b;
    }
}

static inline
int64_t mod_l (int64_t a, int64_t b) {
    int64_t m = a % b;
    if (b>0) {
        return m >=0 ? m : m+b;
    } else {
        return m <=0 ? m : m+b;
    }
}
