CC = gcc

CFLAGS  = -Wall -std=c11 -I./lib

FAST_FLAGS  = -Wall -std=c11 -Ofast -I./lib

LINK_LIBRARY = -lm -lfftw3 -lhdf5

LBFGSB  = lib/lbfgsb.c lib/linesearch.c lib/subalgorithms.c lib/print.c

LINPACK = lib/linpack.c

BLAS 	= lib/miniCBLAS.c

TIMER   = lib/timer.c

SRC = $(LBFGSB) $(LINPACK) $(BLAS) $(TIMER)

all: lbfgs-b.c problem.c utils.c $(SRC)
	$(CC) -o lbfgs-b $(CFLAGS) lbfgs-b.c $(SRC) $(LINK_LIBRARY)

fast: lbfgs-b.c problem.c utils.c $(SRC)
	$(CC) -o lbfgs-b $(FAST_FLAGS) lbfgs-b.c $(SRC) $(LINK_LIBRARY)

clean:
	git clean -dfX

