from ..num import *
from mpmath import mp, pi
from math import sin, log, fmod

r0 = 0.95

def compute_r1(alpha, acc):
    C0 = (1.3**(1.0 - alpha))/(pi * sin(pi * alpha)) # Equation (5.3)
    r1 = (-2.0 * log(acc/C0))**alpha # Equation (4.21)
    return r1    

def is_between(c, a, b, eq = False):
    n = 2.0 * pi
    a = fmod(a, n)
    b = fmod(b, n)
    c = fmod(c, n)

    _cmp = lt if eq else le
    
    if lt(a, b):
        if _cmp(a, c) and _cmp(c, b):
            return True;
        else:
            return False
    else: # a >= b
        if _cmp(b, c) and _cmp(c, a):
            return False  # if in [b, a] then not in [a, b]
        else:
            return True

def diskp(z, r):
    """
    Returns whether a complex number z lies within the (open) disk or radius r
    """
    return lt(abs(z), r)

def closure_diskp(z, r):
    """
    Returns whether a complex number z lies within the closure of the open disk or radius r
    """
    return diskp(z, r) or eq(abs(z), r)

def wedgep(z, phi1, phi2):
    """
    Returns whether a complex number z lies within the wegde defined by (3.2)
    """
    return is_between(mp.arg(z), phi1, phi2)

def closure_wedgep(z, phi1, phi2):
    """
    Returns whether a complex number z lies within the wegde defined by (3.3)
    """
    return is_between(mp.arg(z), phi1, phi2, True)

def in_region_G0(z):
    """
    Returns whether z lies in Region G0, eq. (3.4)
    """
    return closure_diskp(z, r0)

def in_region_G1(z, alpha, acc):
    """
    Returns whether z lies in Region G1, eq. (3.5)
    """
    r1 = compute_r1(alpha, acc)
    delta = pi * alpha/8.0
    phi1 = -pi * alpha + delta
    phi2 =  pi * alpha - delta
    return (not diskp(z, r1)) and wedgep(z, phi1, phi2) 

def in_region_G2(z, alpha, acc):
    """
    Returns whether z lies in Region G2, eq. (3.6)
    """
    r1 = compute_r1(alpha, acc)
    deltat = min(pi * alpha/8.0, pi * (alpha + 1.0)/2.0)
    phi1 =  pi * alpha + deltat
    phi2 = -pi * alpha - deltat
    return (not diskp(z, r1)) and wedgep(z, phi1, phi2)

def in_region_G3(z, alpha, acc):
    """
    Returns whether z lies in Region G3, eq. (3.7)
    """
    r1 = compute_r1(alpha, acc)
    delta = pi * alpha/8.0
    deltat = min(pi * alpha/8.0, pi * (alpha + 1.0)/2.0)
    phi1 = pi * alpha - delta
    phi2 = pi * alpha + deltat
    return (not diskp(z, r1)) and closure_wedgep(z, phi1, phi2)

def in_region_G4(z, alpha, acc):
    """
    Returns whether z lies in Region G4, eq. (3.8)
    """
    r1 = compute_r1(alpha, acc)
    delta = pi * alpha/8.0
    deltat = min(pi * alpha/8.0, pi * (alpha + 1.0)/2.0)
    phi1 = -pi * alpha - deltat
    phi2 = -pi * alpha + delta
    return (not diskp(z, r1)) and closure_wedgep(z, phi1, phi2)

def in_region_G5(z, alpha, acc):
    """
    Returns whether z lies in Region G5, eq. (3.9)
    """
    r1 = compute_r1(alpha, acc)
    phi1 = -5.0 * pi * alpha/6.0
    phi2 =  5.0 * pi * alpha/6.0
    return diskp(z, r1) and (closure_wedgep(z, phi1, phi2) and (not diskp(z, r0)))

def in_region_G6(z, alpha, acc):
    """
    Returns whether z lies in Region G6, eq. (3.10)
    """
    r1 = compute_r1(alpha, acc)
    phi1 =  5.0 * pi * alpha/6.0
    phi2 = -5.0 * pi * alpha/6.0
    return diskp(z, r1) and (wedgep(z, phi1, phi2) and (not diskp(z, r0)))
