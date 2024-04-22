#include <flint/acb.h>
#include <flint/arb.h>
#include <flint/arf.h>
#include <flint/acb_hypgeom.h>
#include <complex.h>
#include <math.h>

#undef erfc

extern double complex acbtocmplx (acb_t z);

double complex
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
