import flint
from flint import arb, acb
from .partition import in_region_G0, in_region_G1, in_region_G2, in_region_G3, in_region_G4, in_region_G5, in_region_G6
from .algorithm import mittleff0, mittleff1, mittleff2, mittleff3, mittleff4, mittleff5, mittleff6

def mittleff(α: float, β: float, z: complex, ε: float = 1e-15) -> complex:
    α, β, z, ε = arb(α), arb(β), acb(z), arb(ε)
    return complex(_mittleff(α, β, z, ε))

def _mittleff(α: arb, β: arb, z: acb, ε: arb) -> acb:
    if in_region_G0(z, α, ε):
        ################################
        # Evaluate using Taylor series #
        ################################
        return mittleff0(α, β, z, ε)
    else:
        if α > 1:
            ##################################
            # Apply recursive relation (2.2) #
            ##################################
            return recursive(α, β, z, ε)
        else:
            if in_region_G1(z, α, ε):
                return mittleff1(α, β, z, ε)
            elif in_region_G2(z, α, ε):
                return mittleff2(α, β, z, ε)
            elif in_region_G3(z, α, ε):
                return mittleff3(α, β, z, ε)
            elif in_region_G4(z, α, ε):
                return mittleff4(α, β, z, ε)
            elif in_region_G5(z, α, ε):
                return mittleff5(α, β, z, ε)
            elif in_region_G6(z, α, ε):
                return mittleff6(α, β, z, ε)
            else:
                print("kasjdhaksjdhaaaa")

