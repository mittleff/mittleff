from mittleff import mittleff
from numpy import isclose

acc = 1.0e-15

def test_siam_1():
    alpha, beta, z = 0.6, 0.8, 7.0    
    expected = 4.24680224735076e+11
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))
    
def test_siam_2():
    alpha, beta, z = 0.6, 0.8, 20.0    
    expected = 4.50513132816147e+64
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_3():
    alpha, beta, z = 0.6, 0.8, -7.0    
    expected = 0.0364029650876388
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_4():
    alpha, beta, z = 0.6, 0.8, -50.0    
    expected = 0.0044638678216643
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_5():
    alpha, beta, z = 0.6, 0.8, -2.16311896062463+6.65739561406608j
    expected = 0.00509750816218177+0.0329981074690976j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_6():
    alpha, beta, z = 0.6, 0.8, -6.18033988749895+19.0211303259031j
    expected = 0.00282134530403973+0.0107554765459201j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_7():
    alpha, beta, z = 0.6, 1.25, 7.0
    expected = 98682128538.563
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_8():
    alpha, beta, z = 0.6, 1.25, 20.0
    expected = 4.76359640442376e+63+9.21339224649432e-19j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_9():
    alpha, beta, z = 0.6, 1.25, -7.0
    expected = 0.101261033685572
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_10():
    alpha, beta, z = 0.6, 1.25, -50.0
    expected = 0.0144197663438114-7.6778268720786e-20j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_11():
    alpha, beta, z = 0.6, 1.25, -2.16311896062463+6.65739561406608j
    expected = 0.0333902562082633+0.0980431639835736j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_12():
    alpha, beta, z = 0.6, 1.25,-6.18033988749895+19.0211303259031j
    expected = 0.011289456355613+0.0342852434746551j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_13():
    alpha, beta, z = 0.6, -0.8, 7.0 
    expected = 76147703794042.9
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_14():
    alpha, beta, z = 0.6, -0.8, 20.0
    expected = 1.32776365747668e+68
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_15():
    alpha, beta, z = 0.6, -0.8, -7.0
    expected = 0.0501291913319244
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_16():
    """
    Obs: the expected value in the paper was typed incorrectly
    """
    alpha, beta, z = 0.6, -0.8, -50
    expected = 0.00751163297262774
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_17():
    alpha, beta, z = 0.6, -0.8, -2.16311896062463+6.65739561406608j
    expected = 0.0193182614473201+0.0537209282676945j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))

def test_siam_18():
    alpha, beta, z = 0.6, -0.8, -6.18033988749895+19.0211303259031j
    expected = 0.00592228306634142+0.0179734030934208j
    computed = mittleff(alpha, beta, z, acc)
    assert(isclose(computed, expected))
