CC = gcc

CFLAGS  = -Wall -std=c11 -I./lib $(shell pkg-config --cflags hdf5)

FAST_FLAGS  = -Wall -std=c11 -Ofast -I./lib $(shell pkg-config --cflags hdf5)

LINK_LIBRARY = -lm -lfftw3 -lhdf5

LBFGSB  = lib/lbfgsb.c lib/linesearch.c lib/subalgorithms.c lib/print.c

LINPACK = lib/linpack.c

BLAS 	= lib/miniCBLAS.c

TIMER   = lib/timer.c

SRC = $(LBFGSB) $(LINPACK) $(BLAS) $(TIMER)

all: lbfgs-b.c problem.c utils.c $(SRC)
	$(CC) -o lbfgs-b $(FAST_FLAGS) lbfgs-b.c $(SRC) $(LINK_LIBRARY)

clean:
	git clean -dfX
