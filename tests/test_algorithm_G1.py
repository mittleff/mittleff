import pytest
import flint
from flint import arb, acb
from mittleff.algorithm import mittleff1
from cmath import isclose
 
def expected(z: complex) -> complex:
    x = acb(z)
    ret = acb.exp(x * x) * acb.erfc(-x)
    return complex(ret)

@pytest.mark.parametrize("z", [
    +1.00809273e+01+2.22251668e+00j,
    +4.22589759e+00+9.54335685e+00j,
    +7.07970270e+00-4.66671354e+00j,
    +1.03652185e+01+2.66482346e+00j,
    +7.36990152e+00+5.61334844e+00j,
    +1.02336488e+01-1.44223606e+00j,
    +8.17467292e+00-2.42910054e+00j,
    +4.34347484e+00+6.98890345e+00j,
    +9.93249059e+00+3.48082382e+00j,
    +5.72222055e+00+5.86522403e+00j])
def test_algorithm_G1(z):
    α, β = arb("0.5"), arb("1.0")
    x = flint.acb(z.real, z.imag)
    ε = flint.arb("1e-15")
    computed = complex(mittleff1(α, β, x, ε))
    assert(isclose(expected(z), computed))



    


                                                                                                 


