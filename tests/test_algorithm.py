from mittleff import mittleff
from numpy import isclose

acc = 1.0e-15

def test_mittleff0():
    alpha, beta, z = 0.5, 1.0, -7.33057219e-02-5.11934762e-01j    
    expected = 0.725165453880409-0.432914368208589j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_mittleff1():
    alpha, beta, z = 0.5, 1.0, +1.00809273e+01+2.22251668e+00j
    expected = 1.32220943230009e42+1.43926327412783e42j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_mittleff2():
    alpha, beta, z = 0.5, 1.0, -8.81638303e+00+4.53794350e+00j
    expected = 0.05054544048122345 + 0.0257564201381803j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_mittleff3():
    alpha, beta, z = 0.5, 1.0, -3.22342758e-01+8.45119872e+00j
    expected = 0.00259774904698259+0.0671347813921331j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_mittleff4():
    alpha, beta, z = 0.5, 1.0, -3.75588680e-01-9.83203507e+00j
    expected = 0.00222360956770567-0.0575980077079615j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_mittleff5():
    alpha, beta, z = 0.5, 1.0, +4.08373780e+00+2.53485316e+00j
    expected = -15817.8109396066 + 54393.0514682909j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_mittleff6():
    alpha, beta, z = 0.5, 1.0, -5.00775165e+00+4.08876443e+00j
    expected = 0.0680477811130904 + 0.0542607316062114j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))     
