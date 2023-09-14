# Mittag-Leffler function

This is the reference implementation of the algorithm presented in

Seybold, H. & Hilfer, R. (2008). Numerical Algorithm for Calculating the Generalized Mittag-Leffler Function SIAM Journal on Numerical Analysis, 47(1), 69–88. [link](https://doi.org/10.1137/070700280),

for computing the Mittag-Leffler function. It should be used for debugging purposes only.

## Install

You can install the package using pip:
```
python -m pip install git+https://github.com/mittleff/mittleff.git
```

## Usage

Basic usage:
```
>>> from mittleff import mittleff
>>> mittleff(1, 1, 0.6) # => 1.822118800390509 
```

## Interactive debugging

```
>>> %load_ext autoreload
>>> %autoreload 2
>>> import logging
>>> logging.basicConfig(level=logging.DEBUG)
>>> from mittleff import mittleff
>>> logging.getLogger('mittleff').setLevel(logging.DEBUG)
>>> mittleff(1, 1, 0.6) # => 1.822118800390509 
```

## Testing

Use [Poetry](https://python-poetry.org/) to install the project dependencies:
```
$ poetry install
$ poetry run pytest -rP
```

