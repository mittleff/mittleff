from mpmath import mp, pi
from math import sin, cos
from cmath import exp

def numerical_integral(func, a, b, acc):
    return mp.quad(func, [a, b])

def omega(x, y, alpha, beta):
    """
    Equation (4.30)
    """
    res = x**(1.0/alpha) * sin(y/alpha) + y * (1.0 + (1.0 - beta)/alpha)
    return res

def A(z, alpha, beta, x):
    """
    Equation (4.27)
    """
    return (1.0/alpha) * z**((1.0 - beta)/alpha) * exp(z**(1.0/alpha) * cos(x/alpha))

def B(r, alpha, beta, z, phi):
    """
    Equation (4.28)
    """
    num = r * sin(omega(r, phi, alpha, beta) - phi) - z * sin(omega(r, phi, alpha, beta))
    den = r**2 - 2.0 * r * z * cos(phi) + z**2
    return (1.0/pi) * A(r, alpha, beta, phi) * (num/den)

def C(varphi, alpha, beta, z, varrho):
    """
    Equation (4.29)
    """
    num = cos(omega(varrho, varphi, alpha, beta)) + 1j * sin(omega(varrho, varphi, alpha, beta))
    den = varrho * (cos(varphi) + 1j * sin(varphi)) - z
    return (varrho/(2.0 * pi)) * A(varrho, alpha, beta, varphi) * (num/den)
