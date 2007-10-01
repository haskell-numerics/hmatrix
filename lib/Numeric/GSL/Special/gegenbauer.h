int gsl_sf_gegenpoly_1_e(double lambda,double x,double* result);
int gsl_sf_gegenpoly_2_e(double lambda,double x,double* result);
int gsl_sf_gegenpoly_3_e(double lambda,double x,double* result);
double gsl_sf_gegenpoly_1(double lambda,double x);
double gsl_sf_gegenpoly_2(double lambda,double x);
double gsl_sf_gegenpoly_3(double lambda,double x);
int gsl_sf_gegenpoly_n_e(int n,double lambda,double x,double* result);
double gsl_sf_gegenpoly_n(int n,double lambda,double x);
int gsl_sf_gegenpoly_array(int nmax,double lambda,double x,double* result_array);
