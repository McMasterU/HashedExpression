CPP = g++
CFLAGS  = -Wall -std=c++11
FAST_FLAGS  = -Wall -std=c11 -Ofast
LINK_LIBRARY = -lm -lfftw3 -lhdf5

all: lbfgs-b.cpp problem.c utils.c
	$(CPP) $(CFLAGS) lbfgs-b.cpp -o lbfgs-b $(LINK_LIBRARY)

fast: lbfgs-b.cpp problem.c utils.c
	$(CPP) $(FAST_FLAGS) lbfgs-b.cpp -o lbfgs-b $(LINK_LIBRARY)

clean:
	git clean -dfX
