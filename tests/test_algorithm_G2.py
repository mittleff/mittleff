import pytest
import flint
from flint import arb, acb
from mittleff.algorithm import mittleff2
from cmath import isclose
 
def expected(z: complex) -> complex:
    x = acb(z)
    ret = acb.exp(x * x) * acb.erfc(-x)
    return complex(ret)

@pytest.mark.parametrize("z", [
    -8.81638303e+00+4.53794350e+00j,
    -9.83286816e+00+3.89528981e+00j,
    -8.15754405e+00+3.51012135e+00j,
    -9.75448297e+00+4.41974518e+00j,
    -1.01655301e+01-1.84058210e+00j,
    -7.74353934e+00-7.18631674e+00j,
    -8.35629806e+00+6.52829764e+00j,
    -5.54981503e+00+6.60796993e+00j,
    -3.88573272e+00-7.26142380e+00j,
    -7.05943454e+00+7.46849452e+00j])
def test_Asymptotics_G1(z):
    α, β = arb("0.5"), arb("1.0")
    x = flint.acb(z.real, z.imag)
    ε = flint.arb("1e-15")
    computed = complex(mittleff2(α, β, x, ε))
    assert(isclose(expected(z), computed))




    

    


                                                                                                 


