CC = gcc
CFLAGS  = -Wall -std=c11
FAST_FLAGS  = -Wall -std=c11 -Ofast
LINK_LIBRARY = -llbfgs -lm -lfftw3 -lhdf5

all: lbfgs.c problem.c
	$(CC) -o lbfgs $(FAST_FLAGS) lbfgs.c $(SRC) $(LINK_LIBRARY)

clean:
	git clean -dfX
