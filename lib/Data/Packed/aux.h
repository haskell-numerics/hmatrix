#define VEC(A) int A##n, double*A##p
#define MAT(A) int A##r, int A##c, double* A##p
#define KVEC(A) int A##n, const double*A##p
#define KMAT(A) int A##r, int A##c, const double* A##p

int trans(int size, KMAT(x),MAT(t));
