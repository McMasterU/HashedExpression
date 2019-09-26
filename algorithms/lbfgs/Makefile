CC = gcc
CFLAGS  = -Wall -std=c11
FAST_FLAGS  = -Wall -std=c11 -Ofast
UNAME := $(shell uname)
LINK_LIBRARY = -llbfgs -lm -lfftw3 -lhdf5
ifeq ($(UNAME),Darwin)
	LIBTOOL := glibtool
else
	LIBTOOL := libtool
endif

all: lbfgs.c problem.c utils.c
	$(CC) $(CFLAGS) lbfgs.c -o lbfgs.o -c
	$(LIBTOOL) --tag=CC --mode=link $(CC) $(CFLAGS) -o lbfgs lbfgs.o $(LINK_LIBRARY)

fast: lbfgs.c problem.c utils.c
	$(CC) $(FAST_FLAGS) lbfgs.c -o lbfgs.o -c
	$(LIBTOOL) --tag=CC --mode=link $(CC) $(FAST_FLAGS) -o lbfgs lbfgs.o $(LINK_LIBRARY)

try: try.c
	$(CC) try.c -o try $(LINK_LIBRARY)

clean:
	git clean -dfX
