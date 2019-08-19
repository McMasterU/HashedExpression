CC = gcc
CFLAGS  = -Wall -std=c11
FAST_FLAGS  = -Wall -std=c11 -Ofast

all: lbfgs.c problem.c
	$(CC) $(CFLAGS) lbfgs.c -o lbfgs.o -c
	glibtool --tag=CC --mode=link $(CC) $(CFLAGS) -o lbfgs lbfgs.o /usr/local/lib/liblbfgs.la -lm

fast: lbfgs.c problem.c
	$(CC) $(FAST_FLAGS) lbfgs.c -o lbfgs.o -c
	glibtool --tag=CC --mode=link $(CC) $(FAST_FLAGS) -o lbfgs lbfgs.o /usr/local/lib/liblbfgs.la -lm
