
#
# Tests for $exp(z^2) * erfc(-z) = E_{0.5, 1}(z)$
#
from mittleff import mittleff
from numpy import isclose

acc = 1e-15

def test_erfc_0():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, -7.33057219000000e-02-5.11934762000000e-01j
    expected = 7.25165453880409e-01-4.32914368208589e-01j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))

def test_erfc_1():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, +1.00809273000000e+01+2.22251668000000e+00j
    expected = 1.32220943230009e+42+1.43926327412783e+42j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))

def test_erfc_2():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, -8.81638303000000e+00+4.53794350000000e+00j
    expected = 5.05454404812233e-02+2.57564201381802e-02j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))

def test_erfc_3():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, -3.22342758000000e-01+8.45119872000000e+00j
    expected = 2.59774904698260e-03+6.71347813921331e-02j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))

def test_erfc_4():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, -3.75588680000000e-01-9.83203507000000e+00j
    expected = 2.22360956770567e-03-5.75980077079615e-02j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))

def test_erfc_5():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, +4.08373780000000e+00+2.53485316000000e+00j
    expected = -1.58178109396067e+04+5.43930514682910e+04j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))

def test_erfc_6():
    alpha, beta, z = +5.00000000000000e-01, +1.00000000000000e+00, -5.00775165000000e+00+4.08876443000000e+00j
    expected = 6.80477811127566e-02+5.42607316062483e-02j
    computed = mittleff(alpha, beta, z, acc)    
    assert(isclose(expected, computed))
