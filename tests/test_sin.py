
#
# Tests for $sin(z) = z E_{2, 2}(-z^2)$
#
from mittleff import mittleff
from numpy import isclose

acc = 1e-15

def test_sin_0():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, -1.42469226000000e+00-1.93094380000000e-01j
    expected = -1.00784724885197e+00-2.82866292071741e-02j
    computed = z*mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_sin_1():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, +6.64679980000000e-01+1.46133978000000e+00j
    expected = 1.40128057728241e+00+1.60563708193143e+00j
    computed = z*mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_sin_2():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, +8.94857660000000e-01+1.89827224000000e+00j
    expected = 2.66183979862789e+00+2.04096901440268e+00j
    computed = z*mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_sin_3():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, -1.32190510000000e-01+2.22985315000000e+00j
    expected = -6.19885888230589e-01+4.55538511767942e+00j
    computed = z*mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))

def test_sin_4():
    alpha, beta, z = +2.00000000000000e+00, +2.00000000000000e+00, -1.60041154000000e+00+3.95133590000000e-01j
    expected = -1.07861309811772e+00-1.20071018874968e-02j
    computed = z*mittleff(alpha, beta, -z**2, acc)    
    assert(isclose(expected, computed))
