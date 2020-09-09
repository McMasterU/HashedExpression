
SOLVER_DIR=../../solvers/lbfgs-b

all:
	cp problem.c bound.h5 kspace.h5 mask.h5 $(SOLVER_DIR)
	cd $(SOLVER_DIR) && make && ./lbfgs-b
	cp $(SOLVER_DIR)/x_out.h5 .