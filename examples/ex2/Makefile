
SOLVER_DIR=../../solvers/lbfgs-b

all:
	cp problem.c x_expanded.txt y.txt $(SOLVER_DIR)
	cd $(SOLVER_DIR) && make && ./lbfgs-b
	cp $(SOLVER_DIR)/theta_out.txt .