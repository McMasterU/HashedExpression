
SOLVER_DIR=../../solvers/ipopt

all:
	cp problem.c data.h5 $(SOLVER_DIR)
	cd $(SOLVER_DIR) && make fast && ./ipopt
	cp $(SOLVER_DIR)/alpha_out.h5 .
