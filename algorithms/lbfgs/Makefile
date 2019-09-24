CC = gcc
CFLAGS  = -Wall -std=c11
FAST_FLAGS  = -Wall -std=c11 -Ofast
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
	LIBTOOL := glibtool
else
	LIBTOOL := libtool
endif

all: lbfgs.c problem.c utils.c
	$(CC) $(CFLAGS) lbfgs.c -o lbfgs.o -c
	$(LIBTOOL) --tag=CC --mode=link $(CC) $(CFLAGS) -o lbfgs lbfgs.o -llbfgs -lm -lfftw3

fast: lbfgs.c problem.c utils.c
	$(CC) $(FAST_FLAGS) lbfgs.c -o lbfgs.o -c
	$(LIBTOOL) --tag=CC --mode=link $(CC) $(FAST_FLAGS) -o lbfgs lbfgs.o -llbfgs -lm -lfftw3

clean:
	git clean -dfX
