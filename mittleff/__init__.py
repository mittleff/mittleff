import flint
from flint import arb, acb
from .partition import in_region_G0
from .algorithm import taylor_series

def MittLeff(α: float, β: float, z: complex, ε: float = 1e-15) -> complex:
    α, β, z, ε = arb(α), arb(β), acb(z), arb(ε)
    return complex(_ML(α, β, z, ε))

def _MittLeff(α: arb, β: arb, z: acb, ε: arb) -> acb:
    if in_region_G0(z, α, ε):
        ################################
        # Evaluate using Taylor series #
        ################################
        return taylor_series(α, β, z, ε)
    elif α >= 1:
        ##################################
        # Apply recursive relation (2.2) #
        ##################################
        return _Recursion(α, β, z, ε)
    else:
        ##################################
        # Apply main algorithm for α < 1 #
        ##################################
        return _MainAlgorithm(α, β, z, ε)

