
typedef struct { double r, i; } doublecomplex;

#define VEC(T,A) int A##n, T* A##p
#define MAT(T,A) int A##r, int A##c, int A##Xr, int A##Xc, T* A##p

#define AT(m,i,j) (m##p[(i)*m##Xr + (j)*m##Xc])
#define TRAV(m,i,j) int i,j; for (i=0;i<m##r;i++) for (j=0;j<m##c;j++)


int c_diag(MAT(double,m), VEC(double,y), MAT(double,z)) {
    int k;
    for (k=0; k<yn; k++) {
        yp[k] = AT(m,k,k);
    }
    { TRAV(z,i,j) {
        AT(z,i,j) = i==j?yp[i]:0;
        }
    }
    return 0;
}

