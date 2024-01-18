
#
# Tests for $sinh(z) = z E_{2, 2}(z^2)$
#
from mittleff import mittleff
from numpy import isclose

acc = 1e-15

def test_sinh_0():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, -1.42469226000000e+00-1.93094380000000e-01j
    expected = -1.92160887420070e+00-4.21900298087567e-01j
    computed = z*mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_sinh_1():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, +6.64679980000000e-01+1.46133978000000e+00j
    expected = 7.80741317390111e-02+1.22179750677017e+00j
    computed = z*mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_sinh_2():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, +8.94857660000000e-01+1.89827224000000e+00j
    expected = -3.27817271209846e-01+1.35194796777751e+00j
    computed = z*mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_sinh_3():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, -1.32190510000000e-01+2.22985315000000e+00j
    expected = 8.11856608972997e-02+7.97487564190490e-01j
    computed = z*mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))

def test_sinh_4():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, -1.60041154000000e+00+3.95133590000000e-01j
    expected = -2.19349810100817e+00+9.92523644338986e-01j
    computed = z*mittleff(alpha, beta, z**2, acc)    
    assert(isclose(expected, computed))
