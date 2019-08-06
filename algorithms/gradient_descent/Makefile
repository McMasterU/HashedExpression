CC = gcc
CFLAGS  = -g -Wall -std=c11

all: gradient_descent.c
	$(CC) $(CFLAGS) gradient_descent.c -o gradient_descent -o problem.o

problem.o: problem.c
	$(CC) $(CFLAGS) -c problem.c

