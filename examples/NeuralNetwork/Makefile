
SOLVER_DIR=../../solvers/lbfgs-b

all:
	cp problem.c data.h5 $(SOLVER_DIR)
	cd $(SOLVER_DIR) && make fast && ./lbfgs-b
	cp $(SOLVER_DIR)/theta1_out.h5 $(SOLVER_DIR)/theta2_out.h5 .