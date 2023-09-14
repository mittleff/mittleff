# from https://github.com/JuliaMath/MittagLeffler.jl
from mittleff import mittleff
from numpy import isclose
import pytest

acc = 1.0e-15

def test_misc_1():
    alpha, beta, z = 0.5, 0.5, 0.5
    expected = 1.5403698281390346
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_2():
    alpha, beta, z = 1.5, 0.5, 0.5
    expected = 1.1448466286155243
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_3():
    alpha, beta, z = 2.3, 1.0, 0.7+2.0j
    expected = 1.201890136368392+0.7895394560075035j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_4():
    alpha, beta, z = 2.3, 1.0, 0.7+0.2j
    expected = 1.268233154873853+0.07914994421659409j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_5():
    alpha, beta, z = 0.3, 1.0, 100.0
    expected = 8.721285946907744692995882256235296113802695745418015206361825134909144332670706e+2015816
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_5():
    alpha, beta, z = 0.9, 0.5, 22.0+22.0j
    expected = -2.7808021618204008e13-2.8561425165239754e13j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_6():
    alpha, beta, z = 0.1, 1.05, 0.9+0.5j
    expected = 0.17617901349590603+2.063981943021305j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_6():
    alpha, beta, z = 4.1, 1.0, 1.0
    expected = 1.0358176744122032
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_7():
    alpha, beta, z = 0.5, 1.0, -12.0
    expected = 0.046854221014893775
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_misc_8():
    alpha, beta, z = 0.125, 1.0, -1.0
    expected = 0.481952081535048487353320281623
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))     
