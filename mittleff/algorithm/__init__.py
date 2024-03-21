import flint
from flint import arb, acb

######################
# External functions #
######################
def mittleff0(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return taylor_series(α, β, z, ε)

def mittleff1(α: arb, β: arb, z: acb, ε: arb) -> acb:
    # one = arb("1")
    # fac = (one/α) * z.pow((one-β)/α) * acb.exp(z.pow(one/α))
    # return fac - asymptotic_series(α, β, z, ε)
    return asymptotic_series(α, β, z, ε, 1)

def mittleff2(α: arb, β: arb, z: acb, ε: arb) -> acb:
    #return -asymptotic_series(α, β, z, ε)
    return asymptotic_series(α, β, z, ε, 2)

def mittleff3(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return asymptotic_series(α, β, z, ε, 3)

def mittleff4(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return asymptotic_series(α, β, z, ε, 4)

def mittleff5(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return acb(0)

def mittleff6(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return acb(0)

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

def asymptotic_series(α: arb, β: arb, z: acb, ε: arb, region: int) -> acb:
    one = arb("1")
    absz = acb.abs_lower(z)
    
    kmax = int(float(arb.mid(arb.ceil((one/α) * absz**(one/α)) + one)))

    fac = (one/α) * z.pow((one-β)/α) * acb.exp(z.pow(one/α))

    return fac * _lambda(region, α, β, z) - sum([arb.rgamma(-α * k + β) * z.pow(-k) for k in range(1, kmax + 1)])

def _lambda(region: int, α: arb, β: arb, z: acb) -> acb:
    res = None
    
    if region == 1:
        res = arb("1")
    elif region == 2:
        res = arb("0")
    else:
        π, one, two, J = flint.arb.pi(), arb("1"), arb("2"), acb(0,1)
        half = arb("0.5")
        th = acb.arg(z.pow(one/α)) + π
        c = pow(-1, region+1) * acb.sqrt(two * (one + J * th - acb.exp(J * th)))
        f1 = z.pow((one - β)/α)
        f2 = acb.exp(z.pow(one/α))
        f4 = half * z.abs_lower()**(one/α)
        f3 = acb.erfc(c * arb.sqrt(f4))
        res = (one/(two * α)) * f1 * f2 * f3

    return res

# def _a
# fac = (one/α) * z.pow((one-β)/α) * acb.exp(z.pow(one/α))

def _Recursion(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)

def _MainAlgorithm(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)

def _IntegralRep(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)


