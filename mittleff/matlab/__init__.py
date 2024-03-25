import oct2py

from importlib.resources import files
pkgpath = str(files("mittleff").joinpath("matlab"))

oct2py.octave.addpath(f'{pkgpath}/seybold')
oct2py.octave.addpath(f'{pkgpath}/garrapa')
oct2py.octave.eval("warning('off','all');")

def MittagLefflerSeybold(α: float, β: float, z: complex, tol: float = 1e-15):
    return oct2py.octave.MittagLeffler(complex(z), float(α), float(β), float(tol))

def MittagLefflerGarrapa(α: float, β: float, z: complex, tol: float = 1e-15):
    """From https://www.mathworks.com/matlabcentral/fileexchange/48154-the-mittag-leffler-function
    """
    return oct2py.octave.ml(complex(z), float(α), float(β))
