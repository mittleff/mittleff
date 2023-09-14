from mpmath import mp

def eq(a, b): return mp.almosteq(a, b)
def lt(a, b): return a < b
def le(a, b): return lt(a, b) or eq(a, b)
def gt(a, b): return a > b
def ge(a, b): return gt(a, b) or eq(a, b)
