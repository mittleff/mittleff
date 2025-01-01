#include <flint/acb.h>
#include <flint/arf.h>
#include <flint/acb_calc.h>
#include <complex.h>

/* Parameters for integrating B */
typedef struct {
    acb_t a;
    acb_t b;
    acb_t z;
    acb_t c;
} integ_params_t;

// Compute eq. (4.30)
void
omega (acb_t res,
       const acb_t x,
       const acb_t y,
       const acb_t a,
       const acb_t b,
       slong prec)
{
    acb_t t1, t2, f1, f2;

    acb_init(t1);
    acb_init(t2);
    acb_init(f1);
    acb_init(f2);
    
    //////////////////////////////////////////////////////
    // f1 = t1 * t2, where t1 = x**(1/a), t2 = sin(y/a) //
    //////////////////////////////////////////////////////
    // Compute t1 = x**(1/a)
    acb_inv(t1, a, prec);
    acb_pow(t1, x, t1, prec);
    // Compute t2 = sin(y/a)
    acb_div(t2, y, a, prec);
    acb_sin(t2, t2, prec);
    // Compute f1
    acb_mul(f1, t1, t2, prec);

    // f2 = y * t2, where t2 = 1 + (1 - b)/a
    acb_one(t1);
    acb_one(t2);
    acb_sub(t2, t2, b, prec);
    acb_div(t2, t2, a, prec);
    acb_add(t2, t1, t2, prec);
    // Compute f2
    acb_mul(f2, y, t2, prec);
    
    // res = f1 + f2
    acb_add(res, f1, f2, prec);

    acb_clear(t1);
    acb_clear(t2);
    acb_clear(f1);
    acb_clear(f2);
}

// DONE: Working!
void
A (acb_t res,
   const acb_t z,
   const acb_t a,
   const acb_t b,
   const acb_t x,
   slong prec)
{
    acb_t f1, f2, f3, t1, t2;

    acb_init(f1);
    acb_init(f2);
    acb_init(f3);
    acb_init(t1);
    acb_init(t2);

    // f1 = 1/a
    acb_inv(f1, a, prec);

    // f2 = z ** ((1 - b) / a)
    acb_one(t1);
    acb_sub(t1, t1, b, prec);
    acb_div(t1, t1, a, prec);
    acb_pow(f2, z, t1, prec);

    // f3 = exp(t1 * t2), where t1 = z ** (1/a), t2 = cos(x/a)
    // Compute t1 = z ** (1/a)
    acb_inv(t1, a, prec);
    acb_pow(t1, z, t1, prec);
    // Compute t2 = cos(x/a)
    acb_div(t2, x, a, prec);
    acb_cos(t2, t2, prec);
    // Compute f3
    acb_mul(f3, t1, t2, prec);
    acb_exp(f3, f3, prec);
    
    // res = f1 * f2 * f3
    acb_mul(res, f1, f2, prec);
    acb_mul(res, res, f3, prec);

    acb_clear(f1);
    acb_clear(f2);
    acb_clear(f3);
    acb_clear(t1);
    acb_clear(t2);    
}

void
B (acb_t res,
   const acb_t r,
   const acb_t a,
   const acb_t b,
   const acb_t z,
   const acb_t phi,
   slong prec)
{
    acb_t f1, f2, f3, t1, t2, t3, w, x, y;

    acb_init(w);
    acb_init(x);
    acb_init(y);
    acb_init(f1);
    acb_init(f2);
    acb_init(f3);
    acb_init(t1);
    acb_init(t2);
    acb_init(t3);

    omega(w, r, phi, a, b, prec);

    // f1 = 1/pi
    acb_const_pi(f1, prec);
    acb_inv(f1, f1, prec);

    // f2 = A(r, a, b, phi);
    A(f2, r, a, b, phi, prec);

    // f3 = t1 / t2,
    // where
    //     t1 = r * sin(w - phi) - z * sin(w),
    //     t2 = r**2 - 2 * r * z * cos(phi) + z**2
    // Compute t1
    acb_sub(t3, w, phi, prec);
    acb_sin(t1, t3, prec);
    acb_mul(t1, t1, r, prec);
    acb_sin(t3, w, prec);
    acb_mul(t3, t3, z, prec);
    acb_sub(t1, t1, t3, prec);
    // Compute t2
    acb_cos(t3, phi, prec);
    acb_mul(t3, t3, z, prec);
    acb_mul(t3, t3, r, prec);
    acb_mul_si(t3, t3, -2, prec);    
    acb_mul(t2, r, r, prec);
    acb_add(t2, t2, t3, prec);
    acb_mul(t3, z, z, prec);
    acb_add(t2, t2, t3, prec);
    // Compute f3
    acb_div(f3, t1, t2, prec);

    // res = f1 * f2 * f3
    acb_mul(res, f1, f2, prec);
    acb_mul(res, res, f3, prec);

    acb_clear(w);
    acb_clear(x);
    acb_clear(y);
    acb_clear(f1);
    acb_clear(f2);
    acb_clear(f3);
    acb_clear(t1);
    acb_clear(t2);
    acb_clear(t3);    
}

double complex wrap_a (double complex z,
        double complex a,
        double complex b,
        double complex x,
        int prec)
{
    double complex res;
    acb_t _res;
    acb_t zz, aa, bb, xx;

    acb_init(_res);
    acb_init(zz); acb_init(aa); acb_init(bb); acb_init(xx);

    acb_set_d_d(zz, creal(z), cimag(z));
    acb_set_d_d(aa, creal(a), cimag(a));
    acb_set_d_d(bb, creal(b), cimag(b));
    acb_set_d_d(xx, creal(x), cimag(x));

    A(_res, zz, aa, bb, xx, (slong)prec);
    res = arf_get_d(arb_midref(acb_realref(_res)), ARF_RND_NEAR) + I * arf_get_d(arb_midref(acb_imagref(_res)), ARF_RND_NEAR);

    acb_clear(_res);
    acb_clear(zz); acb_clear(aa); acb_clear(bb); acb_clear(xx);
    
    return res;
}

double complex wrap_b (double complex r,
                       double complex a,
                       double complex b,
                       double complex z,
                       double complex phi,
                       int prec)
{
    double complex res;
    acb_t _res;
    acb_t zz, aa, bb, rr, pphi;

    acb_init(_res);
    acb_init(rr);
    acb_init(aa);
    acb_init(bb);
    acb_init(zz);
    acb_init(pphi);

    acb_set_d_d(rr, creal(r), cimag(r));
    acb_set_d_d(aa, creal(a), cimag(a));
    acb_set_d_d(bb, creal(b), cimag(b));
    acb_set_d_d(zz, creal(z), cimag(z));
    acb_set_d_d(pphi, creal(phi), cimag(phi));

    B(_res, rr, aa, bb, zz, pphi, (slong)prec);
    res = arf_get_d(arb_midref(acb_realref(_res)), ARF_RND_NEAR) + I * arf_get_d(arb_midref(acb_imagref(_res)), ARF_RND_NEAR);

    acb_clear(_res);
    acb_clear(rr);
    acb_clear(aa);
    acb_clear(bb);
    acb_clear(zz);
    acb_clear(pphi);

    return res;
}
   
static int
fnB (acb_ptr res, const acb_t r, void * param, slong order, slong prec)
{
    if (order > 1)
        flint_abort();  /* Would be needed for Taylor method. */

    integ_params_t* p = (integ_params_t*) param;

    acb_t f1, f2, f3, t1, t2, t3;
    acb_t w, x, y, a, b, z;

    acb_t phi;

    acb_init(phi);
    acb_init(w);
    acb_init(x); acb_init(y);
    acb_init(a); acb_init(b);
    acb_init(f1); acb_init(f2); acb_init(f3);
    acb_init(t1); acb_init(t2); acb_init(t3);
    acb_init(z);

    acb_set(phi, p->c);
    acb_set(a, p->a);
    acb_set(b, p->b);
    acb_set(z, p->z);

    // Init r, phi, a, b from param

    omega(w, r, phi, a, b, prec);

    // f1 = 1/pi
    acb_const_pi(f1, prec);
    acb_inv(f1, f1, prec);

    // f2 = A(r, a, b, phi);
    A(f2, r, a, b, phi, prec);

    // f3 = t1 / t2,
    // where
    //     t1 = r * sin(w - phi) - z * sin(w),
    //     t2 = r**2 - 2 * r * z * cos(phi) + z**2
    // Compute t1
    acb_sub(t3, w, phi, prec);
    acb_sin(t1, t3, prec);
    acb_mul(t1, t1, r, prec);
    // Compute t2
    acb_cos(t3, phi, prec);
    acb_mul(t3, t3, z, prec);
    acb_mul(t3, t3, r, prec);
    acb_mul_si(t3, t3, -2, prec);    
    acb_mul(t2, r, r, prec);
    acb_add(t2, t2, t3, prec);
    acb_mul(t3, z, z, prec);
    acb_add(t2, t2, t3, prec);
    // Compute f3
    acb_div(f3, t1, t2, prec);

    // res = f1 * f2 * f3
    acb_mul(res, f1, f2, prec);
    acb_mul(res, res, f3, prec);
    
    acb_clear(w);
    acb_clear(x); acb_clear(y);
    acb_clear(a); acb_clear(b);
    acb_clear(f1); acb_clear(f2); acb_clear(f3);
    acb_clear(t1); acb_clear(t2); acb_clear(t3);
    acb_clear(phi);
    
    return 0;
}

/* double complex */
/* wrap_b (double complex r, */
/*         double complex a, */
/*         double complex b, */
/*         double complex z, */
/*         double complex phi, */
/*         int prec) */
/* { */
/*     double complex res; */
/*     int status; */
/*     acb_t _res, rr; */
/*     integ_params_t par; */

/*     acb_init(rr); */
/*     acb_init(par.a); */
/*     acb_init(par.b); */
/*     acb_init(par.z); */
/*     acb_init(par.c); */

/*     acb_set_d_d(rr, creal(r), cimag(r)); */
/*     acb_set_d_d(par.a, creal(a), cimag(a)); */
/*     acb_set_d_d(par.b, creal(b), cimag(b)); */
/*     acb_set_d_d(par.z, creal(z), cimag(z)); */
/*     acb_set_d_d(par.c, creal(phi), cimag(phi)); */

/*     status = fnB (_res, rr, &par, 0, (slong)prec); */
/*     res = arf_get_d(arb_midref(acb_realref(_res)), ARF_RND_NEAR) + I * arf_get_d(arb_midref(acb_imagref(_res)), ARF_RND_NEAR); */

/*     acb_clear(rr); */
/*     acb_clear(_res); */
/*     acb_clear(par.a); */
/*     acb_clear(par.b); */
/*     acb_clear(par.z); */
/*     acb_clear(par.c); */
    
/*     return res; */
/* } */

static int
fnC (acb_ptr res, const acb_t ph, void * param, slong order, slong prec)
{
    acb_t w, rho, a, b, z;
    acb_t f1, f2, f3, t1, t2, t3;
    integ_params_t* p = (integ_params_t*) param;

    acb_init(w);
    acb_init(rho); acb_init(a); acb_init(b);
    acb_init(z);
    acb_init(t1); acb_init(t2); acb_init(t3);
    acb_init(f1); acb_init(f2); acb_init(f3);

     // Init x, y, a, b from param
    acb_set(rho, p->c);
    acb_set(a, p->a);
    acb_set(b, p->b);
    acb_set(z, p->z);

    omega(w, rho, ph, a, b, prec);

    // Compute f1 = rho/(2 * pi)
    acb_const_pi(t2, prec);
    acb_mul_si(t2, t2, 2, prec);
    acb_div(f1, rho, t2, prec);
    
    // Compute f2 = A(rho, a, b, ph);
    A(f2, rho, a, b, ph, prec);
    
    // Compute f3 = t1 / t2,
    // t1 = exp(I * w)
    acb_onei(t1);
    acb_mul(t1, t1, w, prec);
    acb_exp(t1, t1, prec);
    // t2 = rho * exp(I * ph) - z
    acb_onei(t2);
    acb_mul(t2, t2, ph, prec);
    acb_exp(t2, t2, prec);
    acb_mul(t2, t2, rho, prec);
    acb_sub(t2, t2, z, prec);
    // compute f3
    acb_div(f3, t1, t2, prec);

    // res = f1 * f2 * f3
    acb_mul(res, f1, f2, prec);
    acb_mul(res, res, f3, prec);

    acb_clear(f1); acb_clear(f2); acb_clear(f3);
    acb_clear(t1); acb_clear(t2); acb_clear(t3);
    acb_clear(w);
    acb_clear(rho); acb_clear(a); acb_clear(b);
    acb_clear(z);

    return 0;
}



/* double */
/* quad_flint (double (*fn_ptr)(double), double x, double y, double epsrel) */
/* { */
/*     double res; */
/*     acb_t s, t, a, b; */
/*     mag_t tol; */
/*     //slong num_threads; */
/*     slong prec, goal; */
/*     //slong N; */
/*     //ulong k; */
/*     //int integral, ifrom, ito; */
/*     //int i, twice, havegoal, havetol; */
/*     acb_calc_integrate_opt_t options; */

/*     acb_calc_integrate_opt_init(options); */

   

/*     prec = 64; */
/*     //twice = 0; */
/*     goal = prec; */
/*     //havetol = havegoal = 0; */
/*     //num_threads = 1; */

/*     options->deg_limit = 0.5*prec + 100; */
/*     options->verbose = 2; */

/*     acb_init(a); */
/*     acb_init(b); */
/*     acb_init(s); */
/*     acb_init(t); */
/*     mag_init(tol); */

/*     //mag_set_ui_2exp_si(tol, 1, -prec); */
/*     mag_set_d(tol, 1e-7); */
/*     acb_set_d(a, x); */
/*     acb_set_d(b, y); */

/*     /\* printf("\n========================\n"); *\/ */
/*     /\* printf("xx = %g\n", xx); *\/ */
/*     /\* printf("sin(xx) = %g, fn_ptr(xx) = %g\n", sin(xx), fn_ptr(xx)); *\/ */
/*     /\* printf("pi = %g\n", M_PI); *\/ */
/*     /\* printf("sin(pi) = %g\n", fn_ptr(M_PI)); *\/ */
/*     /\* printf("sin(pi/2) = %g\n", fn_ptr(0.5*M_PI)); *\/ */
/*     /\* printf("sin(3*pi/2) = %g\n", fn_ptr(1.5*M_PI)); *\/ */
    
/*     acb_calc_integrate(s, dummy_function_flint, fn_ptr, a, b, goal, tol, options, prec); */
/*     res = creal(acbtocmplx(s)); */
/*     printf("%g\n", res); */

/*     acb_clear(a); */
/*     acb_clear(b); */
/*     acb_clear(s); */
/*     acb_clear(t); */
/*     mag_clear(tol); */

/*     return res; */
/* } */


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
