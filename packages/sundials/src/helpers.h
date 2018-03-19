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

/* check the computed solution */
int check_ans(N_Vector y, realtype t, realtype rtol, realtype atol);
