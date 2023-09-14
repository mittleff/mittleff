from mittleff import mittleff
from numpy import isclose
import pytest

acc = 1.0e-15

def test_sin():
    alpha, beta, z = 2.0, 2.0, 1.001
    expected = 0.842010866288257
    computed = z*mittleff(alpha, beta, -z**2, acc)
    assert(isclose(expected, computed))

def test_cos():
    alpha, beta, z = 2.0, 1.0, 1.001
    expected = 0.539460564872447
    computed = mittleff(alpha, beta, -z**2, acc)
    assert(isclose(expected, computed))

def test_sinh():
    alpha, beta, z = 2.0, 2.0, 1.001
    expected = 1.17674486213644
    computed = z*mittleff(alpha, beta, z**2, acc)
    assert(isclose(expected, computed))

def test_cosh():
    alpha, beta, z = 2.0, 1.0, 1.001
    expected = 1.54425660774514
    computed = mittleff(alpha, beta, z**2, acc)
    assert(isclose(expected, computed))    
