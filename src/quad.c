#include <stdio.h>
#include <math.h>
#include <gsl/gsl_integration.h>

static double
dummy_function (double x, void * params)
{
    double(*fn_ptr)(double) = params;
    return fn_ptr(x);
}

double
quad (double (*fn_ptr)(double), double a, double b, double epsrel)
{
  // Workspace limit
    size_t limit = 10000;
    gsl_integration_workspace * w
      = gsl_integration_workspace_alloc(limit);

    double result, error;

    gsl_function F;
    F.function = &dummy_function;
    F.params = fn_ptr;

    gsl_integration_qag (&F, a, b, 1e-8, epsrel, limit, 4, w, &result, &error);

    //printf ("result          = % .18f\n", result);
    /* printf ("exact result    = % .18f\n", expected); */
    //printf ("estimated error = % .18f\n", error);
    /* printf ("actual error    = % .18f\n", result - expected); */
    /* printf ("intervals       = %zu\n", w->size); */

    gsl_integration_workspace_free (w);

    return result;
}

/* int */
/* main (void) */
/* { */
/*   double result; */
/*   double expected = -4.0; */
/*   //double alpha = 1.0; */

/*   result = quad (&f, 0, 1); */

/*   printf ("result          = % .18f\n", result); */
/*   printf ("exact result    = % .18f\n", expected); */
/*   //printf ("estimated error = % .18f\n", error); */
/*   printf ("actual error    = % .18f\n", result - expected); */
/*   //printf ("intervals       = %zu\n", w->size); */

/*   //gsl_integration_workspace_free (w); */

/*   return 0; */
/* } */
