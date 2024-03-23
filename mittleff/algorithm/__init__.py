import flint
from flint import arb, acb
from ..partition import _compute_r1, in_region_G1, in_region_G2, in_region_G3, in_region_G4, in_region_G5, in_region_G6
#from mittleff import _mittleff

######################
# External functions #
######################

def mittleff0(α: arb, β: arb, z: acb, ε: arb) -> acb:   
    return taylor_series(α, β, z, ε)

def mittleff1(α: arb, β: arb, z: acb, ε: arb) -> acb:    
    return asymptotic_series(α, β, z, ε, 1)

def mittleff2(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return asymptotic_series(α, β, z, ε, 2)

def mittleff3(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return asymptotic_series(α, β, z, ε, 3)

def mittleff4(α: arb, β: arb, z: acb, ε: arb) -> acb:
    return asymptotic_series(α, β, z, ε, 4)

def mittleff5(α: arb, β: arb, z: acb, ε: arb) -> acb:
    """
    apply eqs. (4.25) and (4.26)
    """
    π = flint.arb.pi()
    return _mittleff5_6(α, β, z, ε, π * α, 1.0)

def mittleff6(α: arb, β: arb, z: acb, ε: arb) -> acb:
    """
    Apply equations (4.31) and (4.32)
    """
    π = flint.arb.pi()
    return _mittleff5_6(α, β, z, ε, 2.0 * π * α/3.0, 0.0)

######################
# Internal functions #
######################
def taylor_series (α: arb, β: arb, z: acb, ε: arb) -> acb:
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

def _compute_rmax(α: arb, β: arb, z: acb, ε: arb) -> arb:
    π = flint.arb.pi()
    one, two = arb("1"), arb("2")
    r1, r2, r3 = arb(0.0), arb(0.0), arb(0.0)
    
    if β <= 1.0:
        if β >= 0.0:
            r1 = two * abs(z)
            r2 = two**α
            r3 = (-two * arb.log(π * ε * (two**β)/12.0))**α
        else:
            r1 = (two * (abs(β) + one))**α
            r2 = two * abs(z)
            den = 12.0 * (abs(β) + two) * (4.0 * abs(β))**abs(β)
            r3 = (-4.0 * arb.log(π * ε * (two**β)/den))**α
    else:
        if β >= 0:
            r1 = one
            r2 = two * abs(z)
            r3 = (-arb.log(π * ε/6))**α
        else:
            r1 = (abs(β) + one)**α
            r2 = two * abs(z)
            den = 6.0 * (abs(β) + two) * (two * abs(β))**abs(β)
            r3 = (-two * arb.log(π * ε/den))**α
    return arb.max(r1, arb.max(r2, r3))

def _mittleff5_6 (α: arb, β: arb, z: acb, ε: arb, c1: arb, c2: arb) -> acb:
    zero, half, one = arb("0"), arb("0.5"), arb("1")
    rmax = _compute_rmax(α, β, z, ε)
    aux = c2 * A(z, α, β, zero)
    integ = zero
    if β <= one:
        integ = quadb(α, β, z, c1, zero, rmax, ε)
    else:
        int1 = quadb(α, β, z, c1, half, (one + c2) * rmax, ε)
        int2 = quadc(α, β, z, half, -c1, c1, ε)
        integ = int1 + int2
    return aux + integ

def quadb(α: arb, β: arb, z: acb, φ: arb, a: arb, b: arb, ε: arb):
    return numerical_integral(lambda r: B(r, α, β, z, φ), a, b, ε)

def quadc(α: arb, β: arb, z: acb, ρ: arb, a: arb, b: arb, ε: arb):
    return numerical_integral(lambda φ: C(φ, α, β, z, ρ), a, b, ε)    

def numerical_integral(func, a, b, acc = 1e-12):
    return acb.integral(lambda x, _: func(x), a, b, verbose = None)

def omega(x: acb, y: acb, α: arb, β: arb) -> acb:
    """
    Equation (4.30)
    """    
    one = arb("1")
    res = x.pow(one/α) * acb.sin(y/α) + y * (one + (one - β)/α)
    return res

def A(z: acb, α: arb, β: arb, x: arb) -> acb:
    """
    Equation (4.27)
    """
    one = arb("1")
    # print('computing A(z, α, β, x) for (z, α, β, x) = ', z, α, β, x)
    # print('z, z**(one/α) = ', z, z**(one/α))
    # print('')
    return (one/α) * z.pow((one - β)/α) * acb.exp(z.pow(one/α) * arb.cos(x/α))

def B(r: acb, α: arb, β: arb, z: acb, φ: arb) -> acb:
    """
    Equation (4.28)
    """
    π = flint.arb.pi()
    ω = omega(r, acb(φ), α, β)
    num = r * acb.sin(ω - φ) - z * acb.sin(ω)
    den = r**2 - 2.0 * r * z * arb.cos(φ) + z**2
    #print('aa', (1.0/π) * A(r, α, β, φ) * (num/den))
    return (1.0/π) * A(r, α, β, φ) * (num/den)

def C(φ: acb, α: arb, β: arb, z: acb, ρ: arb):
    """
    Equation (4.29)
    """
    π = flint.arb.pi()
    J = acb(0+1j)
    ω = omega(acb(ρ), φ, α, β)
    num = acb.cos(ω) + J * acb.sin(ω)
    den = ρ * (acb.cos(φ) + J * acb.sin(φ)) - z
    return (ρ/(2.0 * π)) * A(acb(ρ), α, β, φ.real) * (num/den)


