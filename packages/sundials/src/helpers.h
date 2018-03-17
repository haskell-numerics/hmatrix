#if defined(SUNDIALS_EXTENDED_PRECISION)
#define GSYM "Lg"
#define ESYM "Le"
#define FSYM "Lf"
#else
#define GSYM "g"
#define ESYM "e"
#define FSYM "f"
#endif

typedef struct _generic_N_Vector BarType;
typedef struct _N_VectorContent_Serial BazType;

/* Check function return value...
    opt == 0 means SUNDIALS function allocates memory so check if
             returned NULL pointer
    opt == 1 means SUNDIALS function returns a flag so check if
             flag >= 0
    opt == 2 means function allocates memory so check if returned
             NULL pointer  
*/
int check_flag(void *flagvalue, const char *funcname, int opt);

/* f routine to compute the ODE RHS function f(t,y). */
int f(realtype t, N_Vector y, N_Vector ydot, void *user_data);

int FARKfi(realtype t, N_Vector y, N_Vector ydot, void *user_data);

/* Jacobian routine to compute J(t,y) = df/dy. */
int Jac(realtype t, N_Vector y, N_Vector fy, SUNMatrix J,
        void *user_data, N_Vector tmp1, N_Vector tmp2, N_Vector tmp3);

/* check the computed solution */
int check_ans(N_Vector y, realtype t, realtype rtol, realtype atol);
