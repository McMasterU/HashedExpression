CC = gcc
CFLAGS  = -Wall -std=c11 -Ofast

all: gradient_descent.c problem.c
	$(CC) $(CFLAGS) gradient_descent.c -o gradient_descent -lm -lfftw3

backtracking: gradient_descent_backtracking.c problem.c
	$(CC) $(CFLAGS) gradient_descent_backtracking.c -o gradient_descent_backtracking -lm -lfftw3

clean:
	git clean -dfX
