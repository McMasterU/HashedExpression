CC = gcc

CFLAGS  = -Wall -std=c11 $(shell pkg-config --cflags ipopt)
FAST_FLAGS  = $(CFLAGS) -Ofast

LINK_LIBRARY = -lipopt -lm -lfftw3 -lhdf5

all: ipopt.c problem.c $(SRC)
	$(CC) -o ipopt $(CFLAGS) ipopt.c $(SRC) $(LINK_LIBRARY)

fast: ipopt.c problem.c $(SRC)
	$(CC) -o ipopt $(FAST_FLAGS) ipopt.c $(SRC) $(LINK_LIBRARY)

clean:
	git clean -dfX

