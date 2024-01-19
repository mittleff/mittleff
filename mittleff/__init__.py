import logging
from mpmath import mp, pi
from .num import *
from .partition import in_region_G0, in_region_G1, in_region_G2, in_region_G3, in_region_G4, in_region_G5, in_region_G6
from .algorithm import mittleff0, mittleff1, mittleff2, mittleff3, mittleff4, mittleff5, mittleff6

logger = logging.getLogger(__name__)

def __mittleff(alpha: mp.mpf, beta: mp.mpf, z: mp.mpc, acc: mp.mpf) -> mp.mpc:    
    if eq(z, 0):
        res = mp.rgamma(beta)
    elif in_region_G0(z):
        # Apply Taylor series, eq. (2.1)
        logger.debug(f"{z} in region G_0")
        res = mittleff0(alpha, beta, z, acc)
    elif gt(alpha, 1): # alpha > 1
        # Apply recursive relation, eq. (2.2)
        # TODO fix this computation
        logger.debug(f"alpha={alpha} > 1: applying recursive relation")
        m = int(mp.fadd(mp.ceil(mp.fdiv(mp.fsub(alpha, 1.0), 2.0)), 1.0))
        one_over_2mp1 = mp.fdiv(1.0, mp.fadd(mp.fmul(2.0, m), 1.0))
        s = 0.0
        for h in range(-m, m+1):
            alphap = mp.fmul(alpha, one_over_2mp1)
            fac1 = mp.power(z, one_over_2mp1)
            th = mp.fmul(2.0, mp.fmul(pi, mp.fmul(1j*h, one_over_2mp1)))
            fac2 = mp.exp(th)
            zp = mp.fmul(fac1, fac2)
            s = mp.fadd(s, __mittleff(alphap, beta, zp, acc))
        res = mp.fmul(one_over_2mp1, s)
    else: # alpha <= 1
        if in_region_G1(z, alpha, acc): 
            logger.debug(f"{z} in region G_1")
            res = mittleff1(alpha, beta, z, acc)
        elif in_region_G2(z, alpha, acc):
            logger.debug(f"{z} in region G_2")
            res = mittleff2(alpha, beta, z, acc)
        elif in_region_G3(z, alpha, acc):
            logger.debug(f"{z} in region G_3")
            res = mittleff3(alpha, beta, z, acc)
        elif in_region_G4(z, alpha, acc):
            logger.debug(f"{z} in region G_4")
            res = mittleff4(alpha, beta, z, acc)
        elif in_region_G5(z, alpha, acc):
            logger.debug(f"{z} in region G_5")
            res = mittleff5(alpha, beta, z, acc)
        elif in_region_G6(z, alpha, acc):
            logger.debug(f"{z} in region G_6")
            res = mittleff6(alpha, beta, z, acc)                       
        else:
            raise Exception(f"z={z} (|z|={mp.fabs(z)}) was not located in any region.")
    return res


def mittleff(alpha: float, beta: float, z: complex, acc: float = 1.0e-15) -> complex:
    """Evaluation of the Generalized Mittag-Leffler function."""
    logger.debug(f"(alpha, beta, z, acc): {alpha:+.3e}, {beta:+.3e}, {z}, {acc:.3e}")

    assert(alpha > 0)
    
    res = __mittleff(mp.mpf(alpha), mp.mpf(beta), mp.mpc(z), mp.mpf(acc))

    return complex(res)
