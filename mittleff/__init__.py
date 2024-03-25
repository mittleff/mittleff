import logging
import flint
from flint import arb, acb
from .partition import in_region_G0, in_region_G1, in_region_G2, in_region_G3, in_region_G4, in_region_G5, in_region_G6
from .algorithm import mittleff0, mittleff1, mittleff2, mittleff3, mittleff4, mittleff5, mittleff6

logger = logging.getLogger(__name__)

def mittleff(α: arb, β: arb, z: acb, ε: arb = None, prec: int = 53) -> acb:
    #prec = 53          # real/complex precision (in bits)
    #dps = 15           # real/complex precision (in digits)
    flint.ctx.prec = prec
    res = None
    #######################
    # Check special cases #
    #######################
    # if acb.abs_lower(z) == 0:
    #     res = acb.rgamma(beta)
    # if α == 1 and β == 1:
    #     res = acb.exp(z)
    # elif α == 2 and β == 1:
    #     res = acb.cosh(acb.sqrt(z))
    # elif α == 2 and β == 2:
    #     res = acb.sinh(acb.sqrt(z))/acb.sqrt(z)
    # elif α == 0.5 and β == 1:
    #     res = acb.exp(z**2) * acb.erfc(-z)    
    # el
    if α == 1 and β == 1:
         res = acb.exp(z)
    elif in_region_G0(z, α):
        ################################
        # Evaluate using Taylor series #
        ################################
        logger.info(f"{z} ∈ G_0")
        res = mittleff0(α, β, z)
    else:
        if α > 1:
            logger.debug(f"α={α} > 1: applying recursive relation")
            ###############################################
            # Apply recursive relation (2.14) from thesis #
            ###############################################
            # π = flint.arb.pi()
            # one, two, J = arb("1"), arb("2"), acb(0+1j)
            # m = int(float(arb.mid(arb.ceil(α))))
            # s = acb(0+0j)
            # a = α / m
            # for h in range(0, m):
            #     zp = z.pow(one / m) * acb.exp(two * π * J * h / m)
            #     s += mittleff(a, β, zp)
            # s = (one/m) * s
            # res = s
            ##################################
            # Apply recursive relation (2.2) #
            ##################################
            π = flint.arb.pi()
            one, two, J = arb("1"), arb("2"), acb(0+1j)
            m = int(float(arb.mid(arb.floor((α - one)/two) + one)))
            s = acb(0+0j)
            a = α / (two * m + one)
            for h in range(-m, m+1):
                zp = z.pow(one / (two * m + one)) * acb.exp(two * π * J * h / (two*m + one))
                s += mittleff(a, β, zp)                
            return one/(two*m + one) * s
            res = s
        else:
            if in_region_G1(z, α):
                logger.info(f"{z} ∈ G_1")
                res = mittleff1(α, β, z)
            elif in_region_G2(z, α):
                logger.info(f"{z} ∈ G_2")
                res = mittleff2(α, β, z)
            elif in_region_G3(z, α):
                logger.info(f"{z} ∈ G_3")
                res = mittleff3(α, β, z)
            elif in_region_G4(z, α):
                logger.info(f"{z} ∈ G_4")
                res = mittleff4(α, β, z)
            elif in_region_G5(z, α):
                logger.info(f"{z} ∈ G_5")
                res = mittleff5(α, β, z)
            elif in_region_G6(z, α):
                logger.info(f"{z} ∈ G_6")
                res = mittleff6(α, β, z)
            else:
                raise Exception(f"Could not find region for {z}")

    logger.info(f'''============================== RESULTS ==============================
#   α = {α}
#   β = {β}
#   z = {z}
#   res = {res}
''')            

    return res
