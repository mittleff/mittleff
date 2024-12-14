#include <stdio.h>
#include <math.h>
#include <gsl/gsl_integration.h>
#include <complex.h>
#include <flint/acb.h>
#include <flint/acb_hypgeom.h>
#include <flint/acb_calc.h>

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



static double
arbtod (const arb_t x)
{
    return arf_get_d(arb_midref(x), ARF_RND_NEAR);
}

static double complex
acbtocmplx (const acb_t z)
{
    double complex res;
    arb_t re, im;

    arb_init(re);
    arb_init(im);

    acb_get_real(re, z);
    acb_get_imag(im, z);

    res = arbtod(re) + I * arbtod(im);

    arb_clear(re);
    arb_clear(im);

    return res;
}

static int
dummy_function_flint(acb_ptr res, const acb_t z, void * param, slong order, slong prec)
{
    if (order > 1)
        flint_abort();  /* Would be needed for Taylor method. */

    double(*fn_ptr)(double) = param;
    if (fn_ptr == NULL)
    {
        printf("Error: fn_ptr is null\n");
    }
    //acb_print(z);
    //double x = creal(acbtocmplx(z));

    /* printf("\n========================\n"); */
    /* printf("x = %g\n", x); */
    /* printf("sin(x) = %g, fn_ptr(x) = %g\n", sin(x), fn_ptr(x)); */
    /* printf("pi = %g\n", M_PI); */
    /* printf("sin(pi) = %g\n", fn_ptr(M_PI)); */
    /* printf("sin(pi/2) = %g\n", fn_ptr(0.5*M_PI)); */
    /* printf("sin(3*pi/2) = %g\n", fn_ptr(1.5*M_PI)); */

    //acb_set_d(res, fn_ptr(x));
    acb_sin(res, z, prec);
    
    return 0;
}

double
quad_flint (double (*fn_ptr)(double), double x, double y, double epsrel)
{
    double res;
    acb_t s, t, a, b;
    mag_t tol;
    //slong num_threads;
    slong prec, goal;
    //slong N;
    //ulong k;
    //int integral, ifrom, ito;
    //int i, twice, havegoal, havetol;
    acb_calc_integrate_opt_t options;

    acb_calc_integrate_opt_init(options);

   

    prec = 64;
    //twice = 0;
    goal = prec;
    //havetol = havegoal = 0;
    //num_threads = 1;

    options->deg_limit = 0.5*prec + 100;
    options->verbose = 2;

    acb_init(a);
    acb_init(b);
    acb_init(s);
    acb_init(t);
    mag_init(tol);

    //mag_set_ui_2exp_si(tol, 1, -prec);
    mag_set_d(tol, 1e-7);
    acb_set_d(a, x);
    acb_set_d(b, y);

    /* printf("\n========================\n"); */
    /* printf("xx = %g\n", xx); */
    /* printf("sin(xx) = %g, fn_ptr(xx) = %g\n", sin(xx), fn_ptr(xx)); */
    /* printf("pi = %g\n", M_PI); */
    /* printf("sin(pi) = %g\n", fn_ptr(M_PI)); */
    /* printf("sin(pi/2) = %g\n", fn_ptr(0.5*M_PI)); */
    /* printf("sin(3*pi/2) = %g\n", fn_ptr(1.5*M_PI)); */
    
    acb_calc_integrate(s, dummy_function_flint, fn_ptr, a, b, goal, tol, options, prec);
    res = creal(acbtocmplx(s));
    printf("%g\n", res);

    acb_clear(a);
    acb_clear(b);
    acb_clear(s);
    acb_clear(t);
    mag_clear(tol);

    return res;
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
