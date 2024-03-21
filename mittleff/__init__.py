import flint
from flint import arb, acb

def MittLeff(α: float, β: float, z: complex, ε: float = 1e-15) -> complex:
    return complex(_ML(arb(α), arb(β), acb(z.real, z.imag), arb(ε)))

def _ML(α: arb, β: arb, z: acb, ε: arb) -> acb:
    π = flint.arb.pi()
    r1 = arb("0.95") # Taylor series upper limit
    δ = π * α / 10.0 # Stokes line region        
    if z.abs_lower() < r1:
        # Evaluate the Taylor Series
        return _Taylor(α, β, z, ε)
    else:
        if α >= 1:
            # Apply the Recursion Relation
            return _Recursion(α, β, z, ε)
        else:
            r2 = _compute_r2(α, ε)
            if z.abs_lower() > r2:               
                # Evaluate the Asymptotic Series
                return _Asymptotics(α, β, z, ε)
            else:
                #  Evaluate the Integral Representation
                return _IntegralRep(α, β, z, ε)

def _Taylor(α: arb, β: arb, z: acb, ε: arb) -> acb:
    one, two = arb("1"), arb("2")
    absz = acb.abs_lower(z)
    
    k1 = arb.ceil((two - arb.abs_lower(β))/α) + one
    k2 = arb.ceil(arb.log(ε * (one - absz))/arb.log(absz)) + one
    kmax = int(float(arb.mid(arb.max(k1, k2))))

    return sum([arb.rgamma(α * k + β) * z.pow(k) for k in range(kmax + 1)])

def _Recursion(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)


def _asy(α: arb, β: arb, z: acb, ε: arb) -> acb:
    one = arb("1")
    absz = acb.abs_lower(z)
    
    kmax = int(float(arb.mid(arb.ceil((one/α) * absz**(one/α)) + one)))

    return sum([arb.rgamma(-α * k + β) * z.pow(-k) for k in range(1, kmax + 1)])

def _Asymptotics(α: arb, β: arb, z: acb, ε: arb) -> acb:
    one = arb("1")
    π = flint.arb.pi()
    δ = π * α / 8.0
    phi1, phi2 = -π * α + δ, π * α - δ
    
    argz = acb.arg(z)
    if argz >= phi1 and argz <= phi2: # Region G1
        return (one/α) * z.pow(one - β) * acb.exp(z.pow(one/α)) - _asy(α, β, z, ε)
    return 1

def _IntegralRep(α: arb, β: arb, z: acb, ε: arb) -> acb: return acb(0)

def _compute_r2(α: arb,  ε: arb) -> arb:
    π = flint.arb.pi()
    C0 = arb("1.3")**(arb("1") - α)/(π * arb.sin(π * α))
    r2 = (arb("-2") * arb.log(ε/C0))**α
    return r2
