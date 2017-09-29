
#ifdef GSLODE1

////////////////////////////// ODE V1 //////////////////////////////////////////

#include <gsl/gsl_odeiv.h>

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


int ode(int method, int control, double h,
        double eps_abs, double eps_rel, double a_y, double a_dydt,
        int f(double, int, const double*, int, double*),
        int jac(double, int, const double*, int, int, double*),
        KRVEC(sc), KRVEC(xi), KRVEC(ts), RMAT(sol)) {

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
        case 8 : { fprintf(stderr, "Sorry: ODE rk1imp not available in this GSL version\n"); exit(0); }
        case 9 : { fprintf(stderr, "Sorry: ODE msadams not available in this GSL version\n"); exit(0); }
        case 10: { fprintf(stderr, "Sorry: ODE msbdf not available in this GSL version\n"); exit(0); }
        default: ERROR(BAD_CODE);
    }

    gsl_odeiv_step * s = gsl_odeiv_step_alloc (T, xin);
    gsl_odeiv_evolve * e = gsl_odeiv_evolve_alloc (xin);
    gsl_odeiv_control * c;

    switch(control) {
        case 0: { c = gsl_odeiv_control_standard_new
                      (eps_abs, eps_rel, a_y, a_dydt); break; }
        case 1: { c = gsl_odeiv_control_scaled_new
                      (eps_abs, eps_rel, a_y, a_dydt, scp, scn); break; }
        default: ERROR(BAD_CODE);
    }

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

#else

///////////////////// ODE V2 ///////////////////////////////////////////////////
                   
#include <gsl/gsl_odeiv2.h>

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


int ode(int method, int control, double h,
        double eps_abs, double eps_rel, double a_y, double a_dydt,
        int f(double, int, const double*, int, double*),
        int jac(double, int, const double*, int, int, double*),
        KRVEC(sc), KRVEC(xi), KRVEC(ts), RMAT(sol)) {

    const gsl_odeiv2_step_type * T;

    switch(method) {
        case 0 : {T = gsl_odeiv2_step_rk2; break; }
        case 1 : {T = gsl_odeiv2_step_rk4; break; }
        case 2 : {T = gsl_odeiv2_step_rkf45; break; }
        case 3 : {T = gsl_odeiv2_step_rkck; break; }
        case 4 : {T = gsl_odeiv2_step_rk8pd; break; }
        case 5 : {T = gsl_odeiv2_step_rk2imp; break; }
        case 6 : {T = gsl_odeiv2_step_rk4imp; break; }
        case 7 : {T = gsl_odeiv2_step_bsimp; break; }
        case 8 : {T = gsl_odeiv2_step_rk1imp; break; }
        case 9 : {T = gsl_odeiv2_step_msadams; break; }
        case 10: {T = gsl_odeiv2_step_msbdf; break; }
        default: ERROR(BAD_CODE);
    }

    Tode P;
    P.f = f;
    P.j = jac;
    P.n = xin;

    gsl_odeiv2_system sys = {odefunc, odejac, xin, &P};

    gsl_odeiv2_driver * d;

    switch(control) {
        case 0: { d = gsl_odeiv2_driver_alloc_standard_new
                      (&sys, T, h, eps_abs, eps_rel, a_y, a_dydt); break; }
        case 1: { d = gsl_odeiv2_driver_alloc_scaled_new
                      (&sys, T, h, eps_abs, eps_rel, a_y, a_dydt, scp); break; }
        default: ERROR(BAD_CODE);
    }

    double t = tsp[0];

    double* y = (double*)calloc(xin,sizeof(double));
    int i,j;
    int status=0;
    for(i=0; i< xin; i++) {
        y[i] = xip[i];
        solp[i] = xip[i];
    }

       for (i = 1; i < tsn ; i++)
         {
           double ti = tsp[i];
           
           status = gsl_odeiv2_driver_apply (d, &t, ti, y);
     
           if (status != GSL_SUCCESS) {
             int k;
             fprintf(stderr, "error in ode, return value=%d\n", status);
             fprintf(stderr, "last successful values are:\n");
             fprintf(stderr, "t = %.5e\n", t);
             for (k=0; k < xin; k++)
               {
                 fprintf(stderr, "y[%d] = %.5e\n", k, y[k]);
               }
             break;
           }
           
           for(j=0; j<xin; j++) {
               solp[i*xin + j] = y[j];
           }
         }

    free(y);
    gsl_odeiv2_driver_free (d);
    
    return status;
}

#endif

