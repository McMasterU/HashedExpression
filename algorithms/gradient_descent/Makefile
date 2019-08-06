CC = gcc
CFLAGS  = -g -Wall -std=c11

all: gradient_descent.o problem.o
	$(CC) $(CFLAGS) -o gradient_descent -o problem.o gradient_descent.o

gradient_descent.o: gradient_descent.c
	$(CC) $(CFLAGS) -c gradient_descent.c

problem.o: problem.c
	$(CC) $(CFLAGS) -c problem.c

