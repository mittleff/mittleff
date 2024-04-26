#include <flint/arb.h>
#include <flint/arf.h>

double
arbtod (arb_t x)
{
    return arf_get_d(arb_midref(x), ARF_RND_NEAR);
}
