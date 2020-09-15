
SOLVER_DIR=../../solvers/lbfgs-b

all:
	cp problem.c x.txt y.txt $(SOLVER_DIR)
	cd $(SOLVER_DIR) && make && ./lbfgs-b
	cp $(SOLVER_DIR)/theta0_out.txt $(SOLVER_DIR)/theta1_out.txt .