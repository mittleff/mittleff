
#
# Tests for $cos(z) = E_{2, 1}(-z^2)$
#
from mittleff import mittleff
from numpy import isclose

acc = 1e-15

def test_cos_0():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, -1.42469226000000e+00-1.93094380000000e-01j
    expected = 1.48307362593645e-01-1.92226474311045e-01j
    computed = mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_cos_1():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, +6.64679980000000e-01+1.46133978000000e+00j
    expected = 1.78818881219233e+00-1.25822734251227e+00j
    computed = mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_cos_2():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, +8.94857660000000e-01+1.89827224000000e+00j
    expected = 2.13470542364750e+00-2.54495655003334e+00j
    computed = mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_cos_3():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, -1.32190510000000e-01+2.22985315000000e+00j
    expected = 4.66199107535284e+00+6.05710930000310e-01j
    computed = mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_cos_4():
    alpha, beta, z = +2.00000000000000e+00, +1.00000000000000e+00, -1.60041154000000e+00+3.95133590000000e-01j
    expected = -3.19526988999422e-02+4.05318417916536e-01j
    computed = mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))
