Currently only an interface to the Runge-Kutta methods:
[ARKode](https://computation.llnl.gov/projects/sundials/arkode)

The interface is almost certainly going to change. Sundials gives a
rich set of "combinators" for controlling the solution of your problem
and reporting on how it performed. The idea is to initially mimic
hmatrix-gsl and add extra, richer functions but ultimately upgrade the
whole interface both for sundials and for gsl.
