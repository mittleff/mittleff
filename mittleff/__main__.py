from flint import arb, acb
from mittleff import mittleff

print(mittleff(arb(1), arb(1), acb(0.6), arb(1e-15))) # => 1.822118800390509

#print(mittleff(0.5, 1.0, +1.00809273e+01+2.22251668e+00j, 1.0e-15))
