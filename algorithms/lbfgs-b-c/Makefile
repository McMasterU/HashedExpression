CC = gcc
CFLAGS  = -Wall -std=c11
FAST_FLAGS  = -Wall -std=c11 -Ofast
LINK_LIBRARY = -lm -lfftw3 -lhdf5
INCLUDES = -I./lib
CFLAGS += $(INCLUDES)

all: lbfgs-b.c problem.c utils.c
	$(CC) $(CFLAGS) lbfgs-b.c -o lbfgs-b $(LINK_LIBRARY)

fast: lbfgs-b.c problem.c utils.c
	$(CC) $(FAST_FLAGS) lbfgs-b.c -o lbfgs-b $(LINK_LIBRARY)

clean:
	git clean -dfX
