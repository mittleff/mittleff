from mpmath import mp, pi
from math import ceil, log
from ..num import *
from .series import __taylor, __asymptotic
from .integrate import numerical_integral, A, B, C

def mittleff0(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    return __taylor(alpha, beta, z, acc)

def mittleff1(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    """
    Apply asymptotic series (2.4)
    """
    fac1 = (1.0/alpha) * mp.power(z, (1.0 - beta)/alpha) * mp.exp(mp.power(z, 1.0/alpha))
    res = mp.fadd(fac1, __asymptotic(alpha, beta, z, acc))
    return res

def mittleff2(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    """
    Apply eq. (2.3)
    """
    return __asymptotic(alpha, beta, z, acc)

def mittleff3(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    """
    Apply eq. (2.6)
    """
    th = mp.arg(mp.power(z, 1.0/alpha)) - pi
    c = mp.sqrt(2.0 * (1.0 + 1j*th - mp.exp(1j*th)))
    fac1 = mp.power(z, (1 - beta)/alpha)
    fac2 = mp.exp(mp.power(z, 1/alpha))
    fac4 = mp.fmul(0.5, mp.power(mp.fabs(z), (1/alpha)))
    fac3 = mp.erfc(mp.fmul(c, mp.sqrt(fac4)))
    fac = mp.fmul(1.0/(2.0 * alpha), mp.fmul(fac1, mp.fmul(fac2, fac3)))
    return fac + __asymptotic(alpha, beta, z, acc)

def mittleff4(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    """
    Apply eq. (2.5)
    """
    th = mp.arg(mp.power(z, 1.0/alpha)) + pi
    c = mp.sqrt(2.0 * (1.0 + 1j*th - mp.exp(1j*th)))
    fac1 = mp.power(z, (1.0 - beta)/alpha)
    fac2 = mp.exp(mp.power(z, 1.0/alpha))
    fac4 = mp.fmul(0.5, mp.power(mp.fabs(z), (1.0/alpha)))
    fac3 = mp.erfc(mp.fmul(-c, mp.sqrt(fac4)))
    fac = mp.fmul(1.0/(2.0 * alpha), mp.fmul(fac1, mp.fmul(fac2, fac3)))
    return fac + __asymptotic(alpha, beta, z, acc)

def mittleff5(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    """
    apply eqs. (4.25) and (4.26)
    """
    res, eps = 0.0, acc
    
    if le(beta, 1.0):
        rmax = 0.0
        if ge(beta, 0.0):
            fac1 = 2.0 * abs(z)
            fac2 = 2.0**alpha
            fac3 = (-2.0 * log(pi * eps * (2.0**beta)/12.0))**alpha
            rmax = max(fac1, fac2, fac3)
        else:
            fac1 = (2.0 * (abs(beta) + 1.0))**alpha
            fac2 = 2.0 * abs(z)
            den = 12.0 * (abs(beta) + 2.0) * (4.0 * abs(beta))**abs(beta)
            fac3 = (-4.0 * log(pi * eps * (2.0**beta)/den))**alpha
            rmax = max(fac1, fac2, fac3)
        int1 = numerical_integral(lambda r: B(r, alpha, beta, z, pi * alpha), 0.0, rmax, acc)
        return mp.fadd(A(z, alpha, beta, 0.0), int1)
    else:
        rmax = 0.0
        if ge(beta, 0):
            fac1 = 2.0 * abs(z)
            fac2 = (-log(pi * eps/6.0))**alpha
            rmax = max(1.0, fac1, fac2)
        else:
            fac1 = (abs(beta) + 1.0)**alpha
            fac2 = 2.0 * abs(z)
            den = (6.0 * (abs(beta) + 2.0) * (2.0 * abs(beta))**abs(beta))
            fac3 = (-2.0 * log(pi * eps/den))**alpha
            rmax = max(fac1, fac2, fac3)    
        int1 = numerical_integral(lambda r: B(r, alpha, beta, z, pi * alpha), 0.5, 2.0 * rmax, acc)
        int2 = numerical_integral(lambda phi: C(phi, alpha, beta, z, 0.5), -pi * alpha, pi * alpha, acc)
        return mp.fadd(A(z, alpha, beta, 0), mp.fadd(int1, int2))

def mittleff6(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:
    """
    Apply equations (4.31) and (4.32)
    """
    res, eps = 0.0, acc
    
    if le(beta, 1.0):
        rmax = 0.0
        if ge(beta, 0.0):
            fac1 = 2.0 * abs(z)
            fac2 = 2.0**alpha
            fac3 = (-2.0 * log(pi * eps * (2.0**beta)/12.0))**alpha
            rmax = max(fac1, fac2, fac3)
        else:
            fac1 = (2.0 * (abs(beta) + 1.0))**alpha
            fac2 = 2.0 * abs(z)
            den = 12.0 * (abs(beta) + 2.0) * (4.0 * abs(beta))**abs(beta)
            fac3 = (-4.0 * log(pi * eps * (2.0**beta)/den))**alpha
            rmax = max(fac1, fac2, fac3)        
        return numerical_integral(lambda r: B(r, alpha, beta, z, 2.0 * pi * alpha/3.0), 0.0, rmax, acc)
    else:
        rmax = 0.0
        if ge(beta, 0):
            fac1 = 2.0 * abs(z)
            fac2 = (-mp.log(pi*eps/6))**alpha
            rmax = max(1.0, fac1, fac2)
        else:
            fac1 = (abs(beta) + 1.0)**alpha
            fac2 = 2.0 * abs(z)
            den = 6.0 * (abs(beta) + 2.0) * (2.0 * abs(beta))**abs(beta)
            fac3 = (-2.0 * log(pi * eps/den))**alpha
            rmax = max(fac1, fac2, fac3)
    
        int1 = numerical_integral(lambda r: B(r, alpha, beta, z, 2.0 * pi * alpha/3.0), 0.5, rmax, acc)
        int2 = numerical_integral(lambda phi: C(phi, alpha, beta, z, 0.5), -2.0 * pi * alpha/3.0, 2.0 * pi * alpha/3.0, acc)
        return mp.fadd(int1, int2)
