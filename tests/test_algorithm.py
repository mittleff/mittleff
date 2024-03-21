import pytest
import flint
from flint import arb, acb
from cmath import isclose
from mittleff.algorithm import mittleff0, mittleff1, mittleff2, mittleff3, mittleff4, mittleff5, mittleff6

def expected(z: complex) -> complex:
    x = acb(z)
    ret = acb.exp(x * x) * acb.erfc(-x)
    return complex(ret)

@pytest.mark.parametrize("z", [
    -7.33057219e-02-5.11934762e-01j,
    -2.71670473e-01-3.61159944e-01j,
    +2.65104392e-01+4.93166724e-01j,
    +3.83251931e-01-7.20912195e-01j,
    +6.15420692e-01+4.94737446e-01j,
    +5.77022431e-01+7.31978260e-01j,
    +5.50863360e-01+3.20026771e-01j,
    +3.19070375e-01-6.61764960e-01j,
    -4.45577540e-01-6.89742478e-01j,
    +1.59189786e-01+3.86077993e-02j
    ])
def test_mittleff0(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff0(α, β, z, ε)
    assert(isclose(expected(z), computed))

@pytest.mark.parametrize("z", [
    +1.00809273e+01+2.22251668e+00j,
    +4.22589759e+00+9.54335685e+00j,
    +7.07970270e+00-4.66671354e+00j,
    +1.03652185e+01+2.66482346e+00j,
    +7.36990152e+00+5.61334844e+00j,
    +1.02336488e+01-1.44223606e+00j,
    +8.17467292e+00-2.42910054e+00j,
    +4.34347484e+00+6.98890345e+00j,
    +9.93249059e+00+3.48082382e+00j,
    +5.72222055e+00+5.86522403e+00j
    ])
def test_mittleff1(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff1(α, β, z, ε)
    assert(isclose(expected(z), computed))

@pytest.mark.parametrize("z", [
    -8.81638303e+00+4.53794350e+00j,
    -9.83286816e+00+3.89528981e+00j,
    -8.15754405e+00+3.51012135e+00j,
    -9.75448297e+00+4.41974518e+00j,
    -1.01655301e+01-1.84058210e+00j,
    -7.74353934e+00-7.18631674e+00j,
    -8.35629806e+00+6.52829764e+00j,
    -5.54981503e+00+6.60796993e+00j,
    -3.88573272e+00-7.26142380e+00j,
    -7.05943454e+00+7.46849452e+00j
    ])
def test_mittleff2(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff2(α, β, z, ε)
    assert(isclose(expected(z), computed))

@pytest.mark.parametrize("z", [
    -3.22342758e-01+8.45119872e+00j,
    +7.13347688e-01+1.06864127e+01j,
    +4.77462676e-01+1.00665352e+01j,
    +9.88018957e-01+1.05615109e+01j,
    -9.62473717e-01+9.61880150e+00j,
    +1.39209663e+00+8.13417968e+00j,
    -1.11915976e-01+9.18185740e+00j,
    +1.93827983e+00+1.05413022e+01j,
    -9.93493529e-01+1.02465564e+01j,
    +1.13040766e+00+9.12078090e+00j
    ])
def test_mittleff3(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff3(α, β, z, ε)
    assert(isclose(expected(z), computed))

@pytest.mark.parametrize("z", [
    -3.75588680e-01-9.83203507e+00j,
    -8.88774219e-01-9.55277101e+00j,
    -6.41990869e-02-8.27306571e+00j,
    -7.41899625e-02-8.24863509e+00j,
    +7.94012305e-01-9.21742744e+00j,
    +2.04734829e+00-1.04412313e+01j,
    +1.24541783e+00-9.61437859e+00j,
    +1.57560906e+00-9.41839253e+00j,
    +8.01100940e-01-9.12867016e+00j,
    -1.52222011e+00-9.69011955e+00j
    ])
def test_mittleff4(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff4(α, β, z, ε)
    assert(isclose(expected(z), computed))

@pytest.mark.parametrize("z", [
    +4.08373780e+00+2.53485316e+00j,
    +4.28734692e+00+1.48338558e+00j,
    +5.58693005e+00-5.46478678e+00j,
    +4.20513707e+00-1.51605969e-01j,
    +1.06269938e+00-3.29532657e+00j,
    +5.31116784e+00-5.90116020e+00j,
    +6.98695414e+00+4.17832152e+00j,
    +6.38791794e+00-2.61371742e+00j,
    +4.76970575e+00-3.58214698e+00j,
    +2.03837467e+00-3.27614785e+00j,
    ])
def test_mittleff5(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff5(α, β, z, ε)
    assert(isclose(expected(z), computed))

@pytest.mark.parametrize("z", [
    -5.00775165e+00+4.08876443e+00j,
    +1.29206756e+00+6.06330787e+00j,
    -3.61542114e+00-6.31484347e+00j,
    -4.41578509e+00+6.29748333e+00j,
    -1.98160394e+00+2.44111893e+00j,
    -8.53012232e-02+7.19321871e+00j,
    -7.28144099e+00-2.17268099e+00j,
    -6.56039361e+00+4.33423743e+00j,
    -3.85833401e+00-4.25315083e+00j,
    -2.97631495e+00+6.48320798e+00j,
    ])
def test_mittleff6(z):
    α, β, z, ε = arb("0.5"), arb("1.0"), acb(z), arb("1e-15")
    computed = mittleff6(α, β, z, ε)
    assert(isclose(expected(z), computed))
