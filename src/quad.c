#include <stdio.h>
#include <math.h>
#include <gsl/gsl_integration.h>
#include <flint/arb.h>
#include <flint/arf.h>
#include <flint/acb.h>
#include <flint/acb_calc.h>
#include <complex.h>

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

static double
dummy_function (double x, void * params)
{
    double(*fn_ptr)(double) = params;
    return fn_ptr(x);
}

static int
flint_dummy_function (acb_ptr res, const acb_t z, void * param, slong order, slong prec)
{
    if (order > 1)
        flint_abort();  /* Would be needed for Taylor method. */

    double(*fn_ptr)(double) = param;
    double x = creal(acbtocmplx(z));
    acb_set_d(res, fn_ptr(x));

    return 0;
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

double
quad_flint (double (*fn_ptr)(double), double a, double b, double epsrel)
{
    double res;
    acb_t x, y, s;
    slong goal, prec;
    mag_t tol;
    acb_calc_integrate_opt_t options;

    prec = (slong)ceil(fabs(log(epsrel)/log(2)));

    acb_calc_integrate_opt_init(options);
    
    goal = prec;

    mag_init(tol);    
    acb_init(x);
    acb_init(y);
    acb_init(s);

    //mag_set_ui_2exp_si(tol, 1, -prec);
    mag_set_d(tol, epsrel);
    acb_set_d(x, a);
    acb_set_d(y, b);
    
    acb_calc_integrate(s, flint_dummy_function, fn_ptr, x, y, goal, tol, options, prec);
    res = creal(acbtocmplx(s));
    
    mag_clear(tol);
    acb_clear(x);
    acb_clear(y);
    acb_clear(s);


    return creal(res);
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
