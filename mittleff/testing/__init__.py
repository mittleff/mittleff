import numpy as np
import flint
from flint import acb, arb
from ..matlab import MittagLefflerSeybold, MittagLefflerGarrapa

np.random.seed(27456)

def sample_from_disk(r = 1.5, n = 30):
    U1 = np.random.uniform(size = n)
    U2 = np.random.uniform(size = n)
    X = r * np.sqrt(U2) * np.cos(2.0 * np.pi * U1)
    Y = r * np.sqrt(U2) * np.sin(2.0 * np.pi * U1)
    return list(X + 1j*Y)

def update_test_02_mittleff():
    fp = open("test_02_mittleff.py", "w")
    fp.write('''
import pytest
from flint import acb, arb
from cmath import isclose
from mittleff import mittleff
from mittleff.matlab import MittagLefflerSeybold, MittagLefflerGarrapa
    
    ''')
    
    print('''
    #############################################
    # Tests for $E_{1, 1}(z) = \\mathrm{exp}(z)$ #
    #############################################
    ''')
    s = []
    for α, β, z in [(1, 1, i) for i in sample_from_disk()]:
        expected, expected_seybold, expected_garrapa = flint.acb.exp(acb(z)), MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_exp(α, β, z, expected, expected_seybold, expected_garrapa):
    """Test special case

    .. math::
        E_{{1, 1}}(z) = \\\\exp(z)
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
    
    print('''
    ###############################################
    # Tests for $E_{2, 1}(z) = \\cosh(\\sqrt{z})$ #
    ###############################################
    ''')
    s = []
    for α, β, z in [(2, 1, i) for i in sample_from_disk()]:
        expected, expected_seybold, expected_garrapa = flint.acb.cosh(flint.acb.sqrt(acb(z))), MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_cos(α, β, z, expected, expected_seybold, expected_garrapa):
    """Test special case

    .. math::
        E_{{2, 1}}(z) = \\\\cosh(\\\\sqrt{z})
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
    

    print('''
    ####################################################################
    # Tests for $E_{2, 2}(z) = \\frac{\\mathrm{sinh}\\sqrt{z}}{\\sqrt{z}}$ #
    ####################################################################
    ''')
    s = []
    for α, β, z in [(2, 2, i) for i in sample_from_disk()]:
        expected, expected_seybold, expected_garrapa = flint.acb.sinh(flint.acb.sqrt(acb(z)))/flint.acb.sqrt(acb(z)), MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_sin(α, β, z, expected, expected_seybold, expected_garrapa):
    """Test special case

    .. math::
        E_{{2, 2}}(z) = \\\\mathrm{{sinh}}\\\\sqrt{{z}}/\\\\sqrt{{z}}
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
    

    print('''
    ########################################################
    # Tests for $E_{1/2, 1}(z) = e^{z^2}\\mathrm{erfc}(-z)$ #
    ########################################################
    ''')
    s = []
    for α, β, z in [(0.5, 1, i) for i in sample_from_disk()]:
        expected, expected_seybold, expected_garrapa = flint.acb.exp(acb(z)**2) * flint.acb.erfc(-acb(z)), MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_erfc(α, β, z, expected, expected_seybold, expected_garrapa):
    """Test special case

    .. math::
        E_{{1/2, 1}}(z) = e^{{z^2}}\\\\mathrm{{erfc}}(-z)
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
    

    print('''
    ########################
    # Tests for SIAM table #
    ########################
    ''')
    s = []
    for α, β, z in [
        (0.6,  0.8, 7.0), # => 4.24680224e+11
        (0.6,  0.8, 20.0), # => 4.50513132e+64
        (0.6,  0.8, -7.0), # => 0.036402965145
        (0.6,  0.8, -50.0), # => 0.004463867842
        (0.6,  0.8, 7.0*np.exp(0.6 * np.pi * 1j)),
        (0.6,  0.8, 20.0*np.exp(0.6 * np.pi * 1j)),
        (0.6, 1.25, 7.0),
        (0.6, 1.25, 20.0),
        (0.6, 1.25, -7.0),
        (0.6, 1.25, -50.0),
        (0.6, 1.25, 7.0*np.exp(0.6 * np.pi * 1j)),
        (0.6, 1.25, 20.0*np.exp(0.6 * np.pi * 1j)),
        (0.6, -0.8, 7.0),
        (0.6, -0.8, 20.0),
        (0.6, -0.8, -7.0),
        (0.6, -0.8, -50.0),
        (0.6, -0.8, 7.0*np.exp(0.6 * np.pi * 1j)),
        (0.6, -0.8, 20.0*np.exp(0.6 * np.pi * 1j)),
    ]:
        expected_seybold, expected_garrapa = MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, complex('NaN'), {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_siam(α, β, z, expected, expected_seybold, expected_garrapa):
    """Tests for compare with table from the paper
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
        

    print('''
    ############################################################
    # Tests from https://github.com/JuliaMath/MittagLeffler.jl #
    ############################################################
    ''')

    print('''
    ######################
    # testset `mittleff` #
    ######################
    ''')
    s = []
    for α, β, z, expected in [
        (0.5, 0.5, 0.5,  1.5403698281390346),
        (1.5, 0.5, 0.5, 1.1448466286155243),
        (2.3, 1, 0.7 + 2.0j, 1.201890136368392 + 0.7895394560075035j),
        (2.3, 1, 0.7 + 0.2j, 1.268233154873853 + 0.07914994421659409j),
    ]:
        expected_seybold, expected_garrapa = MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_mittleffjl_testset_mittleff(α, β, z, expected, expected_seybold, expected_garrapa):
    """Tests from https://github.com/JuliaMath/MittagLeffler.jl

    testset "mittleff"
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')

    print('''
    ######################
    # testset `branches` #
    ######################
    ''')
    s = []
    for α, β, z, expected in [
        (0.9, 0.5, 22 + 22j, -2.7808021618204008e13-2.8561425165239754e13j),
        (0.1, 1.05, 0.9 + 0.5j, 0.17617901349590603+2.063981943021305j),
        (4.1, 1, 1, 1.0358176744122032),
    ]:
        expected_seybold, expected_garrapa = MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_mittleffjl_testset_branches(α, β, z, expected, expected_seybold, expected_garrapa):
    """Tests from https://github.com/JuliaMath/MittagLeffler.jl

    testset "mittleff"
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
    
    
    print('''
    ##############################################################################
    # testset [issue #8](https://github.com/JuliaMath/MittagLeffler.jl/issues/8) #
    ##############################################################################
    ''')
    s = []
    for α, β, z, expected in [
        (0.5, 1, -12, 0.046854221014893775),
    ]:
        expected_seybold, expected_garrapa = MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_mittleffjl_testset_issue_008(α, β, z, expected, expected_seybold, expected_garrapa):
    """Tests from https://github.com/JuliaMath/MittagLeffler.jl

    testset for issue #8
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')


    print('''
    ################################################################################
    # testset [issue #12](https://github.com/JuliaMath/MittagLeffler.jl/issues/12) #
    ################################################################################
    ''')
    s = []
    for α, β, z, expected in [
        (0.125, 1, -1, -0.481952081535048487353320281623),
    ]:
        expected_seybold, expected_garrapa = MittagLefflerSeybold(α, β, z), MittagLefflerGarrapa(α, β, z)
        s.append(f"{'': <4}({α:+.2e}, {β:+.2e}, {complex(z):+.12e}, {complex(expected):+.12e}, {complex(expected_seybold):+.12e}, {complex(expected_garrapa):+.12e}),")
    s = "\n".join(s)
    
    fp.write(f'''
@pytest.mark.parametrize("α, β, z, expected, expected_seybold, expected_garrapa", [
{'': <4}# α, β, z, expected, expected_seybold, expected_garrapa
{s}
])
def test_mittleffjl_testset_issue_012(α, β, z, expected, expected_seybold, expected_garrapa):
    """Tests from https://github.com/JuliaMath/MittagLeffler.jl

    testset for issue #12
    """
    α, β, z = arb(α), arb(β), acb(z)
    computed = complex(mittleff(α, β, z))
    assert(isclose(expected, computed) or isclose(expected_seybold, computed) or isclose(expected_garrapa, computed))''')
        
    fp.close()
