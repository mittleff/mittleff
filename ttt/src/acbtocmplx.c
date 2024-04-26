#include <flint/acb.h>
#include <flint/arb.h>
#include <flint/arf.h>
#include <complex.h>

extern double arbtod (arb_t x);

double complex
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
