import pytest
import flint
from flint import arb, acb
from mittleff.algorithm import mittleff0
from cmath import isclose
 
def expected(z: complex) -> complex:
    x = acb(z)
    ret = acb.exp(x * x) * acb.erfc(-x)
    return complex(ret)

@pytest.mark.parametrize("z", [
    -7.33057219e-02-5.11934762e-01j,
    -2.71670473e-01-3.61159944e-01j,
    +2.65104392e-01+4.93166724e-01j,
    +3.83251931e-01-7.20912195e-01j,
    +6.15420692e-01+4.94737446e-01j,
    +5.77022431e-01+7.31978260e-01j,
    +5.50863360e-01+3.20026771e-01j,
    +3.19070375e-01-6.61764960e-01j,
    -4.45577540e-01-6.89742478e-01j,
    +1.59189786e-01+3.86077993e-02j])
def test_Taylor(z):
    α, β = arb("0.5"), arb("1.0")
    x = flint.acb(z.real, z.imag)
    ε = flint.arb("1e-15")
    computed = complex(mittleff0(α, β, x, ε))
    assert(isclose(expected(z), computed))


    

                                                                                                 


