#include <flint/arb.h>
#include <flint/arf.h>
#include <flint/arb_hypgeom.h>
#include <math.h>

extern double arbtod (arb_t x);

double
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
