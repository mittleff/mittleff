import logging
import flint
from flint import arb, acb
from .partition import in_region_G0, in_region_G1, in_region_G2, in_region_G3, in_region_G4, in_region_G5, in_region_G6
from .algorithm import mittleff0, mittleff1, mittleff2, mittleff3, mittleff4, mittleff5, mittleff6

logger = logging.getLogger(__name__)

def mittleff(α: arb, β: arb, z: acb, ε: arb) -> acb:
    logger.info(f'''
######################################################################
# Parameters:
#   α = {α}
#   β = {β}
#   z = {z}
#   ε = {ε}
######################################################################
''')
    if in_region_G0(z, α, ε):
        ################################
        # Evaluate using Taylor series #
        ################################
        logger.info(f"\n{z} ∈ G_0")
        return mittleff0(α, β, z, ε)
    else:
        if α > 1:
            logger.debug(f"α={α} > 1: applying recursive relation")
            ###############################################
            # Apply recursive relation (2.14) from thesis #
            ###############################################
            π = flint.arb.pi()
            one, two, J = arb("1"), arb("2"), acb(0+1j)
            m = int(float(arb.mid(arb.ceil(α))))
            #print('m = ', m)
            s = acb(0+0j)
            a = α / m
            #print(s)
            for h in range(0, m):
                zp = z.pow(one / m) * acb.exp(two * π * J * h / m)
                #print(f"h = {h}", mittleff(a, β, zp, ε))
                s += mittleff(a, β, zp, ε)
            s = (one/m) * s
            #print('s = ', s)
            return s
            ##################################
            # Apply recursive relation (2.2) #
            ##################################
            # π = flint.arb.pi()
            # one, two, J = arb("1"), arb("2"), acb(0+1j)
            # m = int(float(arb.mid(arb.floor((α - one)/two) + one)))
            # #print('m = ', m)
            # s = acb(0+0j)
            # a = α / (two * m + one)
            # #print(s)
            # for h in range(-m, m+1):
            #     #print(f"h = {h}")
            #     zp = z.pow(one / (two * m + one)) * acb.exp(two * π * J * h / (two*m + one))
            #     print(f"h = {h}", _mittleff(a, β, zp, ε))
            #     s += _mittleff(a, β, zp, ε)                
            # return one/(two*m + one) * s
        else:
            if in_region_G1(z, α, ε):
                logger.info(f"\n{z} ∈ G_1")
                return mittleff1(α, β, z, ε)
            elif in_region_G2(z, α, ε):
                logger.info(f"\n{z} ∈ G_2")
                return mittleff2(α, β, z, ε)
            elif in_region_G3(z, α, ε):
                logger.info(f"\n{z} ∈ G_3")
                return mittleff3(α, β, z, ε)
            elif in_region_G4(z, α, ε):
                logger.info(f"\n{z} ∈ G_4")
                return mittleff4(α, β, z, ε)
            elif in_region_G5(z, α, ε):
                logger.info(f"\n{z} ∈ G_5")
                return mittleff5(α, β, z, ε)
            elif in_region_G6(z, α, ε):
                logger.info(f"\n{z} ∈ G_6")
                return mittleff6(α, β, z, ε)
            else:
                raise Exception(f"Could not find region for {z}") 
