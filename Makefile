SHELL := /bin/bash
CC=gcc
CFLAGS=-Wall -Werror $$(gsl-config --cflags)
LDFLAGS=-lm -lflint $$(gsl-config --libs)

shared: build/libquad.so build/librgamma.so build/liberfc.so

build/libquad.so: src/quad.c
	mkdir -p build
	$(CC) $(CFLAGS) -c src/quad.c -o src/quad.o
	$(CC) $(CFLAGS) -fPIC -shared src/quad.o -o build/libquad.so $(LDFLAGS)

build/librgamma.so: src/arbtod.c src/ml_rgamma.c
	mkdir -p build
	$(CC) $(CFLAGS) -c src/arbtod.c -o src/arbtod.o
	$(CC) $(CFLAGS) -c src/ml_rgamma.c -o src/ml_rgamma.o
	$(CC) $(CFLAGS) -fPIC -shared src/arbtod.o src/ml_rgamma.o -o build/librgamma.so $(LDFLAGS)

build/liberfc.so: src/arbtod.c src/acbtocmplx.c src/ml_erfc.c
	mkdir -p build
	$(CC) $(CFLAGS) -c src/arbtod.c -o src/arbtod.o
	$(CC) $(CFLAGS) -c src/acbtocmplx.c -o src/acbtocmplx.o
	$(CC) $(CFLAGS) -c src/ml_erfc.c -o src/ml_erfc.o
	$(CC) $(CFLAGS) -fPIC -shared src/arbtod.o src/acbtocmplx.o src/ml_erfc.o -o build/liberfc.so $(LDFLAGS)

###########
# Testing #
###########
.PHONY: test test-partition test-algorithm test-mittleff

test: test-partition test-algorithm test-mittleff test-recursion

test-partition: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./ guile -s tests/test-partition.scm

test-algorithm: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./ guile -s tests/test-algorithm.scm

test-mittleff: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./ guile -s tests/test-mittleff.scm

test-recursion: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./ guile -s tests/test-recursion.scm
