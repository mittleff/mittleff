import logging
import flint
from flint import arb, acb

r0 = arb("0.95")

logger = logging.getLogger(__name__)

# ######################
# # Internal functions #
# ######################
def _compute_r1(α: arb) -> arb:
    ε = arb(10**(-flint.ctx.dps))
    π = flint.arb.pi()
    C0 = arb("1.3")**(arb("1") - α)/(π * arb.sin(π * α))
    r1 = (arb("-2") * arb.log(ε/C0))**α
    return r1

def _compute_delta_deltat(α: arb) -> [arb, arb]:
    """
    Compute $delta$ and $tilde{delta}$ parameters, described in the
    paragraph between eqs. (3.6) and (3.7).
    """
    π, one, two, eight = flint.arb.pi(), arb("1"), arb("2"), arb("8")
    δ = π * α/eight
    τ = arb.min(π * α/eight, π * (α + one)/two)
    return δ, τ

def _is_between(c: arb, a: arb, b: arb, closed: bool = False) -> bool:
    def _fmod(x: arb, y: arb) -> arb:
        return x - y * arb.floor(x / y)
    ret = None
    π = flint.arb.pi()

    n = arb("2") * π
    a, b, c = _fmod(a, n), _fmod(b, n), _fmod(c, n)

    if a < b:
        if closed:
            ret = a <= c and c <= b
        else:
            ret = a < c and c < b
    else: # a >= b
        if closed:
            ret = not (b <= c and c <= a)
        else:
            ret = not (b < c and c < a)
    return ret

def _open_diskp(z: acb, r: arb) -> bool:
    """
    Returns whether a complex number z lies within the open disk or radius r
    """
    return z.abs_lower() < r # lt(abs(z), r)

def _closed_diskp(z: acb, r: arb) -> bool:
    """
    Returns whether a complex number z lies within the closed disk or radius r
    """
    return z.abs_lower() <= r

def _open_wedgep(z: acb, ϕ1: arb, ϕ2: arb) -> bool:
    """
    Returns whether a complex number z lies within the open wegde defined by (3.2)
    """
    return _is_between(z.arg(), ϕ1, ϕ2, closed = False)

def _closed_wedgep(z: acb, ϕ1: arb, ϕ2: arb) -> bool:
    """
    Returns whether a complex number z lies within the wegde defined by (3.3)
    """
    return _is_between(z.arg(), ϕ1, ϕ2, closed = True)

# ######################
# # External functions #
# ######################
def in_region_G0(z: acb, α: arb) -> bool:
    """
    Returns whether z lies in Region G0, eq. (3.4)
    """
    #logger.info(flint.ctx)
    return _closed_diskp(z, r0)

def in_region_G1(z: acb, α: arb) -> bool:
    """
    Returns whether z lies in Region G1, eq. (3.5)
    """
    logger.debug(flint.ctx)
    π = flint.arb.pi()
    r1 = _compute_r1(α)
    δ, _ = _compute_delta_deltat(α)
    ϕ1, ϕ2 = -π * α + δ, +π * α - δ

    return (not _open_diskp(z, r1)) and _open_wedgep(z, ϕ1, ϕ2)

def in_region_G2(z: acb, α: arb) -> bool:
    """
    Returns whether z lies in Region G2, eq. (3.6)
    """
    π = flint.arb.pi()
    r1 = _compute_r1(α)
    _, τ = _compute_delta_deltat(α)
    ϕ1, ϕ2 = +π * α + τ, -π * α - τ

    return (not _open_diskp(z, r1)) and _open_wedgep(z, ϕ1, ϕ2)

def in_region_G3(z: acb, α: arb) -> bool:
    """
    Returns whether z lies in Region G3, eq. (3.7)
    """
    π = flint.arb.pi()
    r1 = _compute_r1(α)
    δ, τ = _compute_delta_deltat(α)
    ϕ1, ϕ2 = π * α - δ, π * α + τ

    return (not _open_diskp(z, r1)) and _closed_wedgep(z, ϕ1, ϕ2)

def in_region_G4(z: acb, α: arb) -> bool:
    """
    Returns whether z lies in Region G4, eq. (3.8)
    """
    π = flint.arb.pi()
    r1 = _compute_r1(α)
    δ, τ = _compute_delta_deltat(α)
    ϕ1, ϕ2 = -π * α - τ, -π * α + δ
    
    return (not _open_diskp(z, r1)) and _closed_wedgep(z, ϕ1, ϕ2)

def in_region_G5(z: acb, α: arb) -> float:
    """
    Returns whether z lies in Region G5, eq. (3.9)
    """
    π = flint.arb.pi()
    five_over_six = arb("5/6")
    r1 = _compute_r1(α)
    ϕ1, ϕ2 = -five_over_six * π * α, +five_over_six * π * α

    return _open_diskp(z, r1) and (_closed_wedgep(z, ϕ1, ϕ2) and (not _open_diskp(z, r0)))

def in_region_G6(z: acb, α: arb) -> float:
    """
    Returns whether z lies in Region G6, eq. (3.10)
    """
    π = flint.arb.pi()
    five_over_six = arb("5/6")
    r1 = _compute_r1(α)
    ϕ1, ϕ2 = +five_over_six * π * α, -five_over_six * π * α

    return _open_diskp(z, r1) and (_open_wedgep(z, ϕ1, ϕ2) and (not _open_diskp(z, r0)))
