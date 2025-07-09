# Mittag-Leffler function

This is the reference implementation of the algorithm presented in

> [Seybold, H. and Hilfer, R. _Numerical Algorithm for Calculating the Generalized Mittag-Leffler Function_, SIAM Journal on Numerical Analysis, 47(1), 69–88 (2008)](https://doi.org/10.1137/070700280)

for computing the Mittag-Leffler function. It is written in [Guile Scheme](https://www.gnu.org/software/guile/).

## Usage

Basic usage:

```
LD_LIBRARY_PATH=build/ GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH=./ guile -s mittleff.scm 1.0 1.0 0.6 0.0
1.822118800390509

LD_LIBRARY_PATH=build/ GUILE_AUTO_COMPILE=0 GUILE_LOAD_PATH=./ guile -s mittleff.scm 0.75 1.0 +1.81009264E+001 +4.36995019E+001
0.20603721071076414+1.3222507016083402i
```

## Testing

```
make test
```

