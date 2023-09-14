from mittleff import mittleff
from numpy import isclose

acc = 1.0e-15

def test_exp_1():
    alpha, beta, z = 1.0, 1.0, 2.0
    expected = 7.38905609893065
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(expected, computed))

def test_exp_2():
    alpha, beta, z = 1.0, 1.0, 3.0+4.0j
    expected = -1.31287831e+01-1.52007845e+01j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(expected, computed))    
