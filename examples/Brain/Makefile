all:
	cp *.h5 ../../algorithms/lbfgs-b/
	symphony brain.sp
	cp problem.c ../../algorithms/lbfgs-b/
	cd ../../algorithms/lbfgs-b; make fast; ./lbfgs-b; python3 plot.py x
