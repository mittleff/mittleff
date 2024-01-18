
#
# Tests for $cosh(z) = E_{2, 1}(z^2)$
#
from mittleff import mittleff
from numpy import isclose

acc = 1e-15

def test_cosh_0():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, -1.42469226000000e+00-1.93094380000000e-01j
    expected = 2.15772016942189e+00+3.75733317193863e-01j
    computed = mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_cosh_1():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, +6.64679980000000e-01+1.46133978000000e+00j
    expected = 1.34270384074874e-01+7.10437973043830e-01j
    computed = mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_cosh_2():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, +8.94857660000000e-01+1.89827224000000e+00j
    expected = -4.59266292056142e-01+9.64999829685609e-01j
    computed = mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_cosh_3():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, -1.32190510000000e-01+2.22985315000000e+00j
    expected = -6.17729663704252e-01-1.04810500062338e-01j
    computed = mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_cosh_4():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, -1.60041154000000e+00+3.95133590000000e-01j
    expected = 2.37976084266865e+00-9.14839293944298e-01j
    computed = mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))
