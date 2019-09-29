CC = g++
CFLAGS  = -Wall -std=c++11
FAST_FLAGS  = -Wall -std=c++11 -Ofast
LINK_LIBRARY = -lm -lfftw3 -lhdf5

all: lbfgs-b.c problem.c utils.c
	$(CC) $(CFLAGS) lbfgs-b.cpp -o lbfgs-b $(LINK_LIBRARY)

fast: lbfgs-b.c problem.c utils.c
	$(CC) $(FAST_FLAGS) lbfgs-b.cpp -o lbfgs-b $(LINK_LIBRARY)

clean:
	git clean -dfX
