from math import ceil, log
from mpmath import mp

def __taylor(alpha, beta, z, acc):
    """
    Apply Taylor series (2.1)
    """
    # Number of power series terms to keep, equation (4.5)
    k1 = int(ceil((2.0 - beta)/alpha) + 1.0)
    k2 = int(ceil(log(acc*(1.0 - abs(z)))/log(abs(z))) + 1.0)
    kmax = max(k1, k2)
    res = 0.0
    for k in range(kmax + 1):
        res = mp.fadd(res, mp.fmul(mp.power(z, k), mp.rgamma(alpha * k + beta)))
    return res

def __asymptotic(alpha, beta, z, acc):
    """
    Return the rhs of equation (2.3)
    """    
    kmax = int(ceil((1.0/alpha)*abs(z)**(1.0/alpha)) + 1.0)
    res = 0.0
    for k in range(1, kmax + 1):
        res = mp.fadd(res, mp.fmul(-mp.power(z, -k), mp.rgamma(beta - alpha*k)))
    return res
