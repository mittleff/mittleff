import pytest
import flint
from flint import arb, acb
from mittleff import _Taylor
from cmath import isclose
 
def expected(z: complex) -> complex:
    x = acb(z)
    ret = acb.exp(x * x) * acb.erfc(-x)
    return complex(ret)

@pytest.mark.parametrize("z", [
    0.59146658-0.11477529j, -0.49957248-0.49161737j,
    0.17839376+0.5405993j ,  0.10115017+0.77160969j,
    0.02800073-0.11940502j, -0.78133383-0.31170687j,
    0.06310724-0.52639207j, -0.24524721-0.47394135j,
    -0.30139173-0.17288442j, -0.52447468+0.11630114j,
    -0.18539627-0.18966495j, -0.57192257-0.24834642j,
    -0.61869792+0.07004775j, -0.11067348+0.14249896j,
    0.38857844+0.53353024j, -0.05984624-0.83141368j,
    0.77891533+0.05852533j,  0.3446462 -0.03561138j,
    0.34992531+0.62063138j, -0.76940794+0.25126257j])
def test_Taylor(z):
    α, β = arb("0.5"), arb("1.0")
    x = flint.acb(z.real, z.imag)
    ε = flint.arb("1e-15")
    computed = complex(_Taylor(α, β, x, ε))
    assert(isclose(expected(z), computed))


                                                                                                 


