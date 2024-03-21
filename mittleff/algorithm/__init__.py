import flint
from flint import arb, acb

######################
# External functions #
######################
def mittleff0(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return taylor_series(α, β, z, ε)

def mittleff1(α: arb, β: arb, z: acb, ε: arb) -> acb:
    one = arb("1")
    fac = (one/α) * z.pow((one-β)/α) * acb.exp(z.pow(one/α))
    return fac - asymptotic_series(α, β, z, ε)

def mittleff2(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return -asymptotic_series(α, β, z, ε)

######################
# Internal functions #
######################
def taylor_series(α: arb, β: arb, z: acb, ε: arb) -> acb:
    one, two = arb("1"), arb("2")
    absz = acb.abs_lower(z)
    
    k1 = arb.ceil((two - arb.abs_lower(β))/α) + one
    k2 = arb.ceil(arb.log(ε * (one - absz))/arb.log(absz)) + one
    kmax = int(float(arb.mid(arb.max(k1, k2))))

    return sum([arb.rgamma(α * k + β) * z.pow(k) for k in range(kmax + 1)])

def asymptotic_series(α: arb, β: arb, z: acb, ε: arb) -> acb:
    one = arb("1")
    absz = acb.abs_lower(z)
    
    kmax = int(float(arb.mid(arb.ceil((one/α) * absz**(one/α)) + one)))

    return sum([arb.rgamma(-α * k + β) * z.pow(-k) for k in range(1, kmax + 1)])

def _Recursion(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)

def _MainAlgorithm(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)

def _IntegralRep(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)


