#define DVEC(A) int A##n, double*A##p
#define DMAT(A) int A##r, int A##c, double*A##p

#define AT(M,r,co) (M##p[(r)*M##c+(co)])

#include <stdlib.h>
#include <stdio.h>
#include <glpk.h>
#include <math.h>

/*-----------------------------------------------------*/

int c_simplex_sparse(int m, int n, DMAT(c), DMAT(b), DVEC(s)) {
    glp_prob *lp;
    lp = glp_create_prob();
    glp_set_obj_dir(lp, GLP_MAX);
    int i,j,k;
    int tot = cr - n;
    glp_add_rows(lp, m);
    glp_add_cols(lp, n);

    //printf("%d %d\n",m,n);

    // the first n values
    for (k=1;k<=n;k++) {
        glp_set_obj_coef(lp, k, AT(c, k-1, 2));
        //printf("%d %f\n",k,AT(c, k-1, 2));
    }

    int * ia = malloc((1+tot)*sizeof(int));
    int * ja = malloc((1+tot)*sizeof(int));
    double * ar = malloc((1+tot)*sizeof(double));

    for (k=1; k<= tot; k++) {
        ia[k] = rint(AT(c,k-1+n,0));
        ja[k] = rint(AT(c,k-1+n,1));
        ar[k] =      AT(c,k-1+n,2);
        //printf("%d %d %f\n",ia[k],ja[k],ar[k]);
    }
    glp_load_matrix(lp, tot, ia, ja, ar);

    int t;
    for (i=1;i<=m;i++) {
    switch((int)rint(AT(b,i-1,0))) {
        case 0: { t = GLP_FR; break; }
        case 1: { t = GLP_LO; break; }
        case 2: { t = GLP_UP; break; }
        case 3: { t = GLP_DB; break; }
       default: { t = GLP_FX; break; }
    }
    glp_set_row_bnds(lp, i, t , AT(b,i-1,1), AT(b,i-1,2));
    }
    for (j=1;j<=n;j++) {
    switch((int)rint(AT(b,m+j-1,0))) {
        case 0: { t = GLP_FR; break; }
        case 1: { t = GLP_LO; break; }
        case 2: { t = GLP_UP; break; }
        case 3: { t = GLP_DB; break; }
       default: { t = GLP_FX; break; }
    }
    glp_set_col_bnds(lp, j, t , AT(b,m+j-1,1), AT(b,m+j-1,2));
    }
    glp_term_out(0);
    glp_simplex(lp, NULL);
    sp[0] = glp_get_status(lp);
    sp[1] = glp_get_obj_val(lp);
    for (k=1; k<=n; k++) {
        sp[k+1] = glp_get_col_prim(lp, k);
    }
    glp_delete_prob(lp);
    free(ia);
    free(ja);
    free(ar);

    return 0;
}
