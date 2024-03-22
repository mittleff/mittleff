import pytest
from cmath import isclose
from mittleff import mittleff

@pytest.mark.parametrize("α,β,z,expected", [
    ###############################################
    # $\alpha = \beta = 1$, when $E(z) = \exp(z)$ #
    ###############################################
    (+1.00000000000000e+00, +1.00000000000000e+00, +8.00000000000000e-01, 2.22554092849247e+00+0.00000000000000e+00j),
    (+1.00000000000000e+00, +1.00000000000000e+00, +2.00000000000000e+00, 7.38905609893065e+00+0.00000000000000e+00j),
    ])
def test_mittleff(α, β, z, expected):
    computed = mittleff(α, β, z, 1e-15)
    assert(isclose(expected, computed))

# def test_exp_1():
#     alpha, beta, z = 
#     computed = mittleff(alpha, beta, z, acc)    
#     assert(isclose(expected, computed))

# def test_exp_2():
#     alpha, beta, z = +1.00000000000000e+00, +1.00000000000000e+00, +3.00000000000000e+00+4.00000000000000e+00j
#     expected = -1.31287830814622e+01-1.52007844630680e+01j
#     computed = mittleff(alpha, beta, z, acc)    
#     assert(isclose(expected, computed))
