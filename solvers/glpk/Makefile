CC = gcc

CFLAGS  = -Wall -std=c11 $(shell pkg-config --cflags glpk)
FAST_FLAGS  = $(CFLAGS) -Ofast

LINK_LIBRARY = -lglpk -lm -lhdf5

all: glpk.c problem.c $(SRC)
	$(CC) -o glpk $(CFLAGS) glpk.c $(SRC) $(LINK_LIBRARY)

fast: glpk.c problem.c $(SRC)
	$(CC) -o glpk $(FAST_FLAGS) glpk.c $(SRC) $(LINK_LIBRARY)

clean:
	git clean -dfX
