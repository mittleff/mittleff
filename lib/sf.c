#include <complex.h>
#include <math.h>
#include <flint/arb.h>
#include <flint/acb.h>
#include <flint/arf.h>
#include <flint/arb_hypgeom.h>
#include <flint/acb_hypgeom.h>

static double
arbtod (arb_t x)
{
    return arf_get_d(arb_midref(x), ARF_RND_NEAR);
}

static double complex
acbtocmplx (acb_t z)
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

static double complex
ml_erfc (double complex x, double eps)
{
    double complex res;
    acb_t _res, _x;
    
    slong prec;
    
    res = 0;
    prec = ceil(fabs(log(eps)/log(2)));

    acb_init(_res);
    acb_init(_x);
    
    acb_set_d_d(_x, creal(x), cimag(x));
    
    acb_hypgeom_erfc(_res, _x, prec);
    res = acbtocmplx(_res);
    
    acb_clear(_res);
    acb_clear(_x);
    

    return res;
}

static double
ml_rgamma (double x, double eps)
{
    double res;
    arb_t _res, _x;
    slong prec;
    
    res = 0;
    prec = (slong)ceil(fabs(log(eps)/log(2)));

    arb_init(_res);
    arb_init(_x);
    
    arb_set_d(_x, x);
    
    arb_hypgeom_rgamma(_res, _x, prec);
    res = arbtod(_res);
    
    arb_clear(_res);
    arb_clear(_x);

    return res;
}
