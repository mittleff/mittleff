SHELL := /bin/bash
CC=gcc
CFLAGS=-Wall -Werror
LDFLAGS=-lm -lflint

shared: build/libmittleff.so

build/libmittleff.so: src/mittleff.c
	mkdir -p build
	$(CC) $(CFLAGS) -c src/mittleff.c -o src/mittleff.o
	$(CC) $(CFLAGS) -fPIC -shared src/mittleff.o -o build/libmittleff.so $(LDFLAGS)

###########
# Testing #
###########
.PHONY: test test-partition test-algorithm test-mittleff

# test: test-partition test-algorithm test-mittleff test-recursion
test: test-mittleff test-recursion

test-partition: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./ guile -s tests/test-partition.scm

test-algorithm: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./ guile -s tests/test-algorithm.scm

test-mittleff: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./bindings/guile:./ guile -s tests/test-mittleff.scm

test-recursion: shared
	time LD_LIBRARY_PATH=build/ GUILE_LOAD_PATH=./bindings/guile:./ guile -s tests/test-recursion.scm
